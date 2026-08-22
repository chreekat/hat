-- | The @#{...}@ format-string engine, mirroring tmux's @format.c@: modifier
-- lists (@format_build_modifiers@), key replacement (@format_replace@) and
-- expansion (@format_expand1@) as a pure evaluator over a 'FormatCtx'.
-- @#(shell)@ segments are delegated to a resolver the caller supplies;
-- strftime @%@ sequences are left for the caller's final pass, with
-- expansion output escaped so only template text reaches it.
module Hat.Server.Format
    ( FormatEnv
    , FormatCtx (..)
    , formatCtx
    , evaluate
    , evaluateCtx
    , renderFormat
    , renderFormatCtx
    ) where

import qualified Data.Array as A
import qualified Data.ByteString as BS
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isAscii, isDigit, isPrint, isSpace, toLower)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    (TimeZone, ZonedTime (..), utc, utcToZonedTime, zonedTimeToUTC)
import Numeric (showFFloat)
import Text.Regex.TDFA
    (CompOption (..), MatchArray, Regex, defaultCompOpt, defaultExecOpt, matchOnce)
import qualified Text.Regex.TDFA.Text as RT

import Hat.Server.Format.Colour (colourFromText, colourToEscape, colourToHex)
import Hat.Server.Format.Fuzzy (fuzzyMatch)
import Hat.TextWidth (charWidth, textWidth)

type FormatEnv = Map Text Text

-- | Everything an expansion can read: the variable map, the shell resolver,
-- the clock, and the loop\/search context behind @S W P L@, @N@ and @C@.
data FormatCtx = FormatCtx
    { vars      :: FormatEnv
    , shellRes  :: Text -> Text
    , now       :: POSIXTime
    , tz        :: TimeZone
    , sessions  :: [FormatEnv]  -- ^ one env per session, creation order
    , windows   :: [FormatEnv]  -- ^ target session's windows, index order
    , panes     :: [FormatEnv]  -- ^ target window's panes, creation order
    , clients   :: [FormatEnv]  -- ^ attached clients
    , paneLines :: [Text]       -- ^ target pane's visible lines, for @C@
    }

-- | A context with no loop or search data.
formatCtx :: FormatEnv -> (Text -> Text) -> ZonedTime -> FormatCtx
formatCtx env shell zt = FormatCtx
    { vars = env
    , shellRes = shell
    , now = utcTimeToPOSIXSeconds (zonedTimeToUTC zt)
    , tz = zonedTimeZone zt
    , sessions = []
    , windows = []
    , panes = []
    , clients = []
    , paneLines = []
    }

evaluate :: FormatEnv -> (Text -> Text) -> Text -> Text
evaluate env shell = evaluateCtx (formatCtx env shell epoch)
  where
    epoch = utcToZonedTime utc (posixSecondsToUTCTime 0)

evaluateCtx :: FormatCtx -> Text -> Text
evaluateCtx ctx = scan ctx 1 True True

-- | Fully render a format string: expand, then the strftime pass over the
-- result. Expansion output has its percents escaped, so only the template's
-- own @%@ sequences reach strftime.
renderFormat :: FormatEnv -> (Text -> Text) -> ZonedTime -> Text -> Text
renderFormat env shell zt = renderFormatCtx (formatCtx env shell zt)

renderFormatCtx :: FormatCtx -> Text -> Text
renderFormatCtx ctx = strftimePass ctx . evaluateCtx ctx

-- Recursion depth cap, tmux's FORMAT_LOOP_LIMIT.
loopLimit :: Int
loopLimit = 100

-- Nested expansion: no percent escaping, depth-limited.
expand1 :: FormatCtx -> Int -> Text -> Text
expand1 ctx depth t
    | depth >= loopLimit = ""
    | otherwise = scan ctx (depth + 1) False True t

-- The template scanner (format_expand1): literal text, #-escapes, #()
-- shell segments, #{} keys, #[] styles, and single-character shorthands.
scan :: FormatCtx -> Int -> Bool -> Bool -> Text -> Text
scan ctx depth esc shorth = go
  where
    go t = case T.breakOn "#" t of
        (pre, rest)
            | T.null rest -> pre
            | otherwise -> pre <> dispatch (T.drop 1 rest)
    splice v = if esc then T.replace "%" "%%" v else v
    dispatch t = case T.uncons t of
        Nothing -> ""  -- a lone trailing # is dropped, like tmux
        Just ('(', rest) -> case balancedParen rest of
            Nothing -> ""
            Just (inner, rest') -> splice (ctx.shellRes inner) <> go rest'
        Just ('{', rest) -> case findClose rest of
            Nothing -> ""
            Just (inner, rest') -> case replaceKey ctx depth inner of
                Nothing -> ""  -- a failed replace truncates, like tmux
                Just v -> splice v <> go rest'
        Just ('#', rest) ->
            let extra = T.takeWhile (== '#') rest
                after = T.drop (T.length extra) rest
            in case T.uncons after of
                Just ('[', styRest) -> styleOpen (2 + T.length extra) styRest
                _ -> "#" <> go rest
        Just ('[', rest) -> styleOpen 1 rest
        Just (',', rest) -> "," <> go rest
        Just ('}', rest) -> "}" <> go rest
        Just (c, rest)
            | shorth, Just var <- shorthand c -> case replaceKey ctx depth var of
                Nothing -> ""
                Just v -> splice v <> go rest
            | otherwise -> "#" <> T.singleton c <> go rest
    -- Copy a style opener; shorthands are inert inside the style body.
    styleOpen k t = T.replicate k "#" <> "[" <> case skipTo "]" t of
        Nothing -> go t
        Just (sbody, rest) ->
            scan ctx depth esc False sbody <> "]" <> go (T.drop 1 rest)
    shorthand = \case
        'D' -> Just "pane_id"
        'F' -> Just "window_flags"
        'H' -> Just "host"
        'I' -> Just "window_index"
        'P' -> Just "pane_index"
        'S' -> Just "session_name"
        'T' -> Just "pane_title"
        'W' -> Just "window_name"
        'h' -> Just "host_short"
        _ -> Nothing

-- | tmux's format_skip1: the text before the first delimiter at bracket
-- depth 0 (honouring #-escapes), and the rest starting at that delimiter.
skipTo :: [Char] -> Text -> Maybe (Text, Text)
skipTo ends t0 = go (0 :: Int) (0 :: Int) t0
  where
    go !n !depth t = case T.uncons t of
        Nothing -> Nothing
        Just ('#', t')
            | Just (c2, t'') <- T.uncons t', c2 `elem` (",#{}:" :: String) ->
                go (n + 2) (if c2 == '{' then depth + 1 else depth) t''
        Just (c, t') ->
            let depth' = if c == '}' then depth - 1 else depth
            in if c `elem` ends && depth' == 0
                then Just (T.take n t0, T.drop n t0)
                else go (n + 1) depth' t'

-- The matching close of a "#{", nesting- and escape-aware.
findClose :: Text -> Maybe (Text, Text)
findClose t = case skipTo "}" ("#{" <> t) of
    Nothing -> Nothing
    Just (before, rest) -> Just (T.drop 2 before, T.drop 1 rest)

balancedParen :: Text -> Maybe (Text, Text)
balancedParen t0 = go (0 :: Int) (0 :: Int) t0
  where
    go !n !depth t = case T.uncons t of
        Nothing -> Nothing
        Just (')', t')
            | depth == 0 -> Just (T.take n t0, t')
            | otherwise -> go (n + 1) (depth - 1) t'
        Just ('(', t') -> go (n + 1) (depth + 1) t'
        Just (_, t') -> go (n + 1) depth t'

-- | tmux's format_unescape: resolve #-escapes outside #{...} nesting.
unescape :: Text -> Text
unescape = T.pack . go (0 :: Int) . T.unpack
  where
    go _ [] = []
    go depth ('#' : rest@(c2 : more))
        | c2 == '{' = '#' : '{' : go (depth + 1) more
        | depth == 0, c2 `elem` (",#}:" :: String) = c2 : go depth more
        | otherwise = '#' : go depth rest
    go depth ('}' : rest) = '}' : go (depth - 1) rest
    go depth (c : rest) = c : go depth rest

-- | tmux's format_strip: drop escape hashes outside #{...} nesting.
strip :: Text -> Text
strip = T.pack . go (0 :: Int) . T.unpack
  where
    go _ [] = []
    go depth ('#' : rest@(c2 : more))
        | c2 == '{' = '#' : '{' : go (depth + 1) more
        | c2 `elem` (",#}:" :: String) =
            (if depth /= 0 then ('#' :) else id) (go depth rest)
        | otherwise = '#' : go depth rest
    go depth ('}' : rest) = '}' : go (depth - 1) rest
    go depth (c : rest) = c : go depth rest

-- | tmux's format_choose: split at the first top-level comma.
choose :: Text -> Maybe (Text, Text)
choose t = case skipTo "," t of
    Nothing -> Nothing
    Just (l, rest) -> Just (l, T.drop 1 rest)

splitAllTop :: Text -> [Text]
splitAllTop t = case choose t of
    Just (l, r) -> l : splitAllTop r
    Nothing -> [t]

-- A parsed modifier: its (one- or two-character) name and arguments.
data Mod = Mod
    { mname :: Text
    , margs :: [Text]
    }

-- Modifier-list parser (format_build_modifiers). Nothing means the whole
-- key is a plain variable.
parseMods :: FormatCtx -> Int -> Text -> Maybe ([Mod], Text)
parseMods ctx depth = go []
  where
    ex = expand1 ctx depth . unescape
    singles = "labdnwETSWPOVL!<>A" :: String
    doubles = ["||", "&&", "!!", "!=", "==", "<=", ">="] :: [Text]
    argful = "ImCLNPSOVst=pReqWcA" :: String
    isEndT t = case T.uncons t of
        Just (c, _) -> c == ';' || c == ':'
        Nothing -> False
    isPunctC c = isAscii c && isPrint c && not (isAlphaNum c) && c /= ' '
    go acc t = case T.uncons t of
        Nothing -> Nothing
        Just (';', t') -> go acc t'
        Just (':', t') -> Just (reverse acc, t')
        Just (c, t')
            | c `elem` singles, isEndT t' ->
                go (Mod (T.singleton c) [] : acc) t'
            | T.take 2 t `elem` doubles, isEndT (T.drop 2 t) ->
                go (Mod (T.take 2 t) [] : acc) (T.drop 2 t)
            | c `notElem` argful -> Nothing
            | otherwise -> case T.uncons t' of
                Nothing -> Nothing
                Just (c2, _)
                    | c2 == ';' || c2 == ':' ->
                        go (Mod (T.singleton c) [] : acc) t'
                    | not (isPunctC c2) || c2 == '-' -> case skipTo ":;" t' of
                        Nothing -> Nothing
                        Just (raw, rest) ->
                            go (Mod (T.singleton c) [ex raw] : acc) rest
                    | otherwise -> case wrapperArgs c2 t' of
                        Nothing -> Nothing
                        Just (as, rest) ->
                            go (Mod (T.singleton c) as : acc) rest
    -- Arguments delimited by an arbitrary punctuation character; the text
    -- points at a delimiter on entry to each round.
    wrapperArgs d = loop []
      where
        loop as t = case T.uncons t of
            Just (c0, t')
                | c0 == d, isEndT t' -> Just (reverse as, t')
                | c0 == d -> case skipTo [d, ';', ':'] t' of
                    Nothing -> Nothing
                    Just (raw, rest) -> continue (ex raw : as) rest
                | c0 == ';' || c0 == ':' -> Just (reverse as, t)
            _ -> Nothing
        continue as rest = case T.uncons rest of
            Just (c0, _)
                | c0 == ';' || c0 == ':' -> Just (reverse as, rest)
                | otherwise -> loop as rest
            Nothing -> Nothing

data QuoteMode = QShell | QShellSq | QStyle | QArgs
data TimeMode = TCtime | TPretty | TRelative | TDifference | TCustom Text
data NameKind = NWindow | NSession
data LoopKind = LSessions | LWindows | LPanes | LClients
data SortOrder = OrderList | OrderIndex | OrderName | OrderActivity | OrderCreation | OrderZ
data ColourMode = ColHex | ColEscFg | ColEscBg

-- The accumulated effect of a key's modifier list.
data ModState = ModState
    { cmp :: Maybe Mod
    , notF :: Bool
    , notNotF :: Bool
    , boolAnd :: Maybe Bool
    , search :: Maybe Text
    , subs :: [(Text, Text, Bool)]
    , limit :: Int
    , marker :: Maybe Text
    , padW :: Int
    , eargs :: Maybe [Text]
    , literalF :: Bool
    , charF :: Bool
    , colourM :: Maybe ColourMode
    , baseF :: Bool
    , dirF :: Bool
    , lenF :: Bool
    , widF :: Bool
    , timeM :: Maybe TimeMode
    , quoteM :: Maybe QuoteMode
    , expandF :: Bool
    , expandTimeF :: Bool
    , nameK :: Maybe NameKind
    , loopK :: Maybe LoopKind
    , sortO :: SortOrder
    , sortR :: Bool
    , repeatF :: Bool
    , clientQ :: Bool
    , unimpl :: Maybe Text
    }

emptyModState :: ModState
emptyModState = ModState
    { cmp = Nothing, notF = False, notNotF = False, boolAnd = Nothing
    , search = Nothing, subs = [], limit = 0, marker = Nothing, padW = 0
    , eargs = Nothing, literalF = False, charF = False, colourM = Nothing
    , baseF = False, dirF = False, lenF = False, widF = False
    , timeM = Nothing, quoteM = Nothing, expandF = False, expandTimeF = False
    , nameK = Nothing, loopK = Nothing, sortO = OrderList, sortR = False
    , repeatF = False, clientQ = False, unimpl = Nothing
    }

collectMods :: [Mod] -> ModState
collectMods = List.foldl' step emptyModState
  where
    step st m = case m.mname of
        nm | nm `elem` (["m", "<", ">", "==", "!=", "<=", ">="] :: [Text]) ->
            st { cmp = Just m }
        "!" -> st { notF = True }
        "!!" -> st { notNotF = True }
        "||" -> st { boolAnd = Just False }
        "&&" -> st { boolAnd = Just True }
        "C" -> st { search = Just (arg0 m) }
        "s" -> case m.margs of
            (a1 : a2 : restA) ->
                st { subs = st.subs <> [(a1, a2, any (T.elem 'i') (take 1 restA))] }
            _ -> st
        "=" -> case m.margs of
            (a1 : restA) -> st
                { limit = numDefault (-10000) 10000 0 a1
                , marker = case restA of
                    (mk : _) -> Just mk
                    [] -> st.marker
                }
            [] -> st
        "p" -> case m.margs of
            (a1 : _) -> st { padW = numDefault (-10000) 10000 0 a1 }
            [] -> st
        "A" -> st { unimpl = Just "A" }
        "O" -> st { unimpl = Just "O" }
        "V" -> st { unimpl = Just "V" }
        "w" -> st { widF = True }
        "e" | n >= 1, n <= 3 -> st { eargs = Just m.margs }
            | otherwise -> st
          where n = length m.margs
        "l" -> st { literalF = True }
        "a" -> st { charF = True }
        "b" -> st { baseF = True }
        "c" -> st { colourM = Just colMode }
          where
            colMode
                | T.elem 'b' (arg0 m) = ColEscBg
                | T.elem 'f' (arg0 m) = ColEscFg
                | otherwise = ColHex
        "d" -> st { dirF = True }
        "n" -> st { lenF = True }
        "I" | not (null m.margs), T.any (`elem` ("fce" :: String)) (arg0 m) ->
                st { clientQ = True }
            | otherwise -> st
        "t" -> st { timeM = Just tmode }
          where
            tmode = case m.margs of
                (a1 : restA)
                    | T.elem 'p' a1 -> TPretty
                    | T.elem 'r' a1 -> TRelative
                    | T.elem 'd' a1 -> TDifference
                    | T.elem 'f' a1, (f : _) <- restA -> TCustom (strip f)
                _ -> TCtime
        "q" -> st { quoteM = qmode }
          where
            qmode = case m.margs of
                [] -> Just QShell
                (a1 : _)
                    | T.elem 's' a1 -> Just QShellSq
                    | T.elem 'e' a1 || T.elem 'h' a1 -> Just QStyle
                    | T.elem 'a' a1 -> Just QArgs
                    | otherwise -> Nothing
        "E" -> st { expandF = True }
        "T" -> st { expandTimeF = True }
        "N" -> st { nameK = nkind }
          where
            nkind = case m.margs of
                [] -> Just NWindow
                (a1 : _)
                    | T.elem 'w' a1 -> Just NWindow
                    | T.elem 's' a1 -> Just NSession
                    | otherwise -> Nothing
        "S" -> loopMod st LSessions OrderIndex
            [('i', OrderIndex), ('n', OrderName), ('t', OrderActivity)] m
        "W" -> loopMod st LWindows OrderList
            [('i', OrderList), ('n', OrderName), ('t', OrderActivity)] m
        "P" -> loopMod st LPanes OrderCreation
            [('i', OrderIndex), ('z', OrderZ)] m
        "L" -> loopMod st LClients OrderList
            [('i', OrderList), ('n', OrderName), ('t', OrderActivity)] m
        "R" -> st { repeatF = True }
        _ -> st
    arg0 m = fromMaybe "" (listToMaybe m.margs)
    loopMod st lk dflt table m = case m.margs of
        [] -> st { loopK = Just lk, sortO = dflt, sortR = False }
        (a1 : _) -> st
            { loopK = Just lk
            , sortO = fromMaybe dflt
                (listToMaybe [o | (c, o) <- table, T.elem c a1])
            , sortR = T.elem 'r' a1
            }

-- Key replacement (format_replace): parse modifiers, produce the base
-- value, then apply the value transformations. Nothing = failed replace.
replaceKey :: FormatCtx -> Int -> Text -> Maybe Text
replaceKey ctx depth key = postOps <$> value
  where
    (mods, copy) = fromMaybe ([], key) (parseMods ctx depth key)
    ms = collectMods mods
    exp1 = expand1 ctx depth
    value
        | ms.clientQ = Just ""  -- no per-client termcap/feature model
        | Just nm <- ms.unimpl =
            Just ("<format modifier '" <> nm <> "' unimplemented>")
        | ms.literalF = Just (unescape copy)
        | ms.charF = Just charVal
        | Just cm <- ms.colourM = Just (colourVal cm)
        | Just lk <- ms.loopK = Just (loopVal lk)
        | Just nk <- ms.nameK = Just (nameVal nk)
        | Just sf <- ms.search = Just (searchVal sf)
        | ms.repeatF = repeatVal
        | ms.notF = Just (boolVal True)
        | ms.notNotF = Just (boolVal False)
        | Just isAnd <- ms.boolAnd = Just (naryVal isAnd)
        | Just c <- ms.cmp = cmpVal c
        | Just ('?', rest) <- T.uncons copy = Just (condVal rest)
        | Just ea <- ms.eargs = Just (fromMaybe "" (exprVal ea))
        | "#{" `T.isInfixOf` copy = Just (exp1 copy)
        | otherwise = Just (fromMaybe "" (findVar ctx ms copy))

    postOps v0 =
        let v1 | ms.expandF = exp1 v0
               | ms.expandTimeF = exp1 (strftimePass ctx v0)
               | otherwise = v0
            v2 = List.foldl' applySub v1 ms.subs
            v3 | ms.limit > 0 =
                    let trimmed = trimLeft ms.limit v2
                    in case ms.marker of
                        Just mk | trimmed /= v2 -> trimmed <> mk
                        _ -> trimmed
               | ms.limit < 0 =
                    let trimmed = trimRight (negate ms.limit) v2
                    in case ms.marker of
                        Just mk | trimmed /= v2 -> mk <> trimmed
                        _ -> trimmed
               | otherwise = v2
            v4 = padTo ms.padW v3
            v5 | ms.lenF = tshow (BS.length (TE.encodeUtf8 v4))
               | otherwise = v4
            v6 | ms.widF = tshow (textWidth v5)
               | otherwise = v5
        in v6
    applySub acc (p0, w0, icase) = regexSub icase (exp1 p0) (exp1 w0) acc

    charVal = case numFull 32 126 (exp1 copy) of
        Just n -> T.singleton (chr n)
        Nothing -> ""

    colourVal cm =
        let new = exp1 copy
        in case cm of
            ColHex -> fromMaybe "" (colourToHex =<< colourFromText new)
            ColEscFg -> escFor False new
            ColEscBg -> escFor True new
      where
        escFor bg new
            | T.toLower new == "none" = "\ESC[0m"
            | otherwise = maybe "" (colourToEscape bg) (colourFromText new)

    loopVal lk =
        let items = case lk of
                LSessions -> ctx.sessions
                LWindows -> ctx.windows
                LPanes -> ctx.panes
                LClients -> ctx.clients
            sorted = sortLoop lk ms.sortO ms.sortR items
            (allF, activeF) = case choose copy of
                Just (l, r) -> (l, Just r)
                Nothing -> (copy, Nothing)
            n = length sorted
            body i item =
                let use = case activeF of
                        Just af | isActiveItem lk ctx item -> af
                        _ -> allF
                    extra = Map.fromList
                        [ ("loop_index", tshow i)
                        , ("loop_last_flag", if i == n - 1 then "1" else "0") ]
                    ctx' = ctx { vars = Map.unions [extra, item, ctx.vars] }
                in expand1 ctx' depth use
        in T.concat (zipWith body [0 ..] sorted)

    nameVal nk =
        let name = exp1 copy
            (nkey, items) = case nk of
                NWindow -> ("window_name", ctx.windows)
                NSession -> ("session_name", ctx.sessions)
        in boolText (any (\it -> Map.lookup nkey it == Just name) items)

    searchVal sf =
        let term = exp1 copy
            icase = T.elem 'i' sf
            isRe = T.elem 'r' sf
            hit ln
                | isRe = regexTest icase term ln
                | otherwise = globMatch icase ("*" <> term <> "*") ln
            trimmed = map (T.dropWhileEnd isSpace) ctx.paneLines
        in case List.findIndex hit trimmed of
            Just i -> tshow (i + 1)
            Nothing -> "0"

    repeatVal = do
        (l, r) <- choose copy
        Just $ case numFull 1 10000 (exp1 r) of
            Just k -> T.replicate k (exp1 l)
            Nothing -> ""

    boolVal negated =
        let v = truthy (exp1 copy)
        in boolText (if negated then not v else v)

    naryVal isAnd =
        let vals = map (truthy . exp1) (splitAllTop copy)
        in boolText (if isAnd then and vals else or vals)

    cmpVal c = do
        (l0, r0) <- choose copy
        let l = exp1 l0
            r = exp1 r0
        Just $ case c.mname of
            "==" -> boolText (l == r)
            "!=" -> boolText (l /= r)
            "<" -> boolText (l < r)
            ">" -> boolText (l > r)
            "<=" -> boolText (l <= r)
            ">=" -> boolText (l >= r)
            "m" -> matchVal (fromMaybe "" (listToMaybe c.margs)) l r
            _ -> ""

    matchVal flags pat txt
        | T.elem 'p' flags = case fuzzyMatch pat txt of
            Nothing -> ""
            Just cols -> T.intercalate "," (map tshow cols)
        | T.elem 'z' flags = maybe "0" (const "1") (fuzzyMatch pat txt)
        | T.elem 'r' flags = boolText (regexTest (T.elem 'i' flags) pat txt)
        | otherwise = boolText (globMatch (T.elem 'i' flags) pat txt)

    -- Conditional: (condition, value) pairs, then an optional default.
    condVal t = case skipTo "," t of
        Nothing -> exp1 t
        Just (cond, rest0) ->
            let rest1 = T.drop 1 rest0
                found = case findVar ctx ms cond of
                    Just v -> v
                    Nothing ->
                        let e = exp1 cond
                        in if e == cond then "" else e
            in case skipTo "," rest1 of
                Nothing
                    | truthy found -> exp1 rest1
                    | otherwise -> ""
                Just (thenPart, rest2)
                    | truthy found -> exp1 thenPart
                    | otherwise -> condVal (T.drop 1 rest2)

    exprVal args = do
        opName <- listToMaybe args
        oper <- List.lookup opName exprOps
        let useFp = case args of
                (_ : f : _) -> T.elem 'f' f
                _ -> False
        prec <- case args of
            (_ : _ : p : _) -> numFull (-100) 100 p
            _ -> Just (if useFp then 2 else 0)
        (l0, r0) <- choose copy
        dl <- parseNum (exp1 l0)
        dr <- parseNum (exp1 r0)
        let (l, r)
                | useFp = (dl, dr)
                | otherwise = (truncDouble dl, truncDouble dr)
            res = oper l r
        Just $ if isNaN res || isInfinite res
            then ""
            else formatFixed prec (if useFp then res else truncDouble res)
    truncDouble d = fromIntegral (truncate d :: Integer)

exprOps :: [(Text, Double -> Double -> Double)]
exprOps =
    [ ("+", (+))
    , ("-", (-))
    , ("*", (*))
    , ("/", (/))
    , ("%", cMod)
    , ("%%", cMod)
    , ("m", cMod)
    , ("==", \a b -> ind (abs (a - b) < 1e-9))
    , ("!=", \a b -> ind (abs (a - b) > 1e-9))
    , (">", \a b -> ind (a > b))
    , ("<", \a b -> ind (a < b))
    , (">=", \a b -> ind (a >= b))
    , ("<=", \a b -> ind (a <= b))
    ]
  where
    ind b = if b then 1 else 0
    cMod a b = a - fromIntegral (truncate (a / b) :: Integer) * b

-- Variable lookup with the find-level modifiers (format_find): t applies
-- to the numeric value; b, d and q transform the found text.
findVar :: FormatCtx -> ModState -> Text -> Maybe Text
findVar ctx ms key = do
    v <- Map.lookup key ctx.vars
    case ms.timeM of
        Just tm -> do
            secs <- numFull 0 (maxBound :: Int) v
            let t = fromIntegral secs :: Integer
            if t == 0 then Nothing else case tm of
                TCtime -> Just (strftimeAt ctx.tz t "%a %b %e %H:%M:%S %Y")
                TPretty -> Just (prettyTime ctx.tz ctx.now t)
                TRelative -> relativeTime ctx.now t
                TDifference -> Just (tshow (floor ctx.now - t))
                TCustom f -> Just (strftimeAt ctx.tz t f)
        Nothing ->
            Just (quoteApply (dirApply (baseApply v)))
  where
    baseApply v = if ms.baseF then basenameT v else v
    dirApply v = if ms.dirF then dirnameT v else v
    quoteApply v = case ms.quoteM of
        Nothing -> v
        Just QShell -> quoteShellT v
        Just QShellSq -> quoteShellSqT v
        Just QStyle -> T.replace "#" "##" v
        Just QArgs -> quoteArgsT v

-- Loop sorting: the orders hat can derive from item envs; activity falls
-- back to the default order (hat keeps no per-entity activity clocks).
sortLoop :: LoopKind -> SortOrder -> Bool -> [FormatEnv] -> [FormatEnv]
sortLoop lk order rev items = (if rev then reverse else id) sorted
  where
    sorted = case (lk, order) of
        (LSessions, OrderName) -> byText "session_name"
        (LSessions, _) -> byNum "session_id"
        (LWindows, OrderName) -> byText "window_name"
        (LWindows, _) -> byNum "window_index"
        (LPanes, OrderIndex) -> byNum "pane_index"
        (LPanes, _) -> byNum "pane_id"
        (LClients, OrderName) -> byText "client_name"
        (LClients, _) -> items
    byText k = List.sortOn (\it -> Map.findWithDefault "" k it) items
    byNum k = List.sortOn (\it -> numOf (Map.findWithDefault "" k it)) items
    numOf t = fromMaybe (0 :: Int) $ do
        let digits = T.dropWhile (not . isDigit) t
        numFull 0 maxBound digits

isActiveItem :: LoopKind -> FormatCtx -> FormatEnv -> Bool
isActiveItem lk ctx item = case lk of
    LSessions -> Map.lookup "session_id" item == Map.lookup "session_id" ctx.vars
        && isJust (Map.lookup "session_id" item)
    LWindows -> Map.lookup "window_active" item == Just "1"
    LPanes -> Map.lookup "pane_active" item == Just "1"
    LClients -> False

-- regsub: global ERE substitution with \\N back references; an anchored
-- pattern replaces only the first match; an invalid pattern is a no-op.
regexSub :: Bool -> Text -> Text -> Text -> Text
regexSub icase pat with txt
    | T.null txt = ""
    | T.null pat = txt
    | otherwise = case compileRe icase pat of
        Left _ -> txt
        Right re -> run re
  where
    anchored = "^" `T.isPrefixOf` pat
    n = T.length txt
    substr a b = T.take (b - a) (T.drop a txt)
    run re = go 0 0 False []
      where
        go start lastE empty acc
            | start > n = T.concat (reverse acc)
            | otherwise = case matchOnce re (T.drop start txt) of
                Nothing -> T.concat (reverse (substr start n : acc))
                Just m ->
                    let (so, ml) = m A.! 0
                        eo = so + ml
                        pre = substr lastE (start + so)
                        rep = expandRefs m (T.drop start txt) with
                    in if anchored
                        then T.concat
                            (reverse (substr (start + eo) n : rep : pre : acc))
                        else if empty || start + so /= lastE || so /= eo
                            then go (start + eo) (start + eo) False
                                (rep : pre : acc)
                            else go (start + eo + 1) (start + eo) True acc

expandRefs :: MatchArray -> Text -> Text -> Text
expandRefs m src = T.pack . go . T.unpack
  where
    (_, hi) = A.bounds m
    go [] = []
    go ('\\' : c : rest)
        | isDigit c
        , i <- digitToInt c
        , i <= hi
        , (so, ml) <- m A.! i
        , ml /= 0
        , so >= 0 =
            T.unpack (T.take ml (T.drop so src)) <> go rest
        | otherwise = c : go rest
    go (c : rest) = c : go rest

compileRe :: Bool -> Text -> Either String Regex
compileRe icase =
    RT.compile defaultCompOpt { caseSensitive = not icase, multiline = False }
        defaultExecOpt

regexTest :: Bool -> Text -> Text -> Bool
regexTest icase pat txt = case compileRe icase pat of
    Left _ -> False
    Right re -> isJust (matchOnce re txt)

-- fnmatch(3)-style glob: *, ?, [...] classes, backslash escapes.
globMatch :: Bool -> Text -> Text -> Bool
globMatch icase pat txt = go (prep pat) (prep txt)
  where
    prep = (if icase then map toLower else id) . T.unpack
    go [] [] = True
    go ('*' : ps) cs = any (go ps) (List.tails cs)
    go ('?' : ps) (_ : cs) = go ps cs
    go ('[' : ps) (c : cs) = case charClass ps of
        Just (member, ps') -> member c && go ps' cs
        Nothing -> c == '[' && go ps cs
    go ('\\' : p : ps) (c : cs) = p == c && go ps cs
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ _ = False
    charClass ps0 =
        let (neg, ps1) = case ps0 of
                ('!' : r) -> (True, r)
                ('^' : r) -> (True, r)
                _ -> (False, ps0)
            items acc = \case
                (']' : r) | not (null acc) -> Just (acc, r)
                (a : '-' : b : r) | b /= ']' -> items ((\c -> c >= a && c <= b) : acc) r
                (a : r) -> items ((== a) : acc) r
                [] -> Nothing
        in do
            (tests, rest) <- items [] ps1
            pure (\c -> neg /= any ($ c) tests, rest)

-- format_quote_shell: backslash-escape shell metacharacters.
quoteShellT :: Text -> Text
quoteShellT = T.concatMap escapeOne
  where
    escapeOne c
        | c `elem` ("|&;<>()$`\\\"'*?[# =%\n\t" :: String) = T.pack ['\\', c]
        | otherwise = T.singleton c

-- format_quote_shell_single: POSIX single-quote wrapping.
quoteShellSqT :: Text -> Text
quoteShellSqT t = "'" <> T.replace "'" "'\\''" t <> "'"

-- args_escape: quote a value as one shell argument.
quoteArgsT :: Text -> Text
quoteArgsT t
    | T.null t = "''"
    | T.length t == 1, T.head t /= ' ', isJust quotes || t == "~" = "\\" <> t
    | otherwise = case quotes of
        Just '\'' -> "'" <> escaped <> "'"
        Just _ -> "\"" <> tilde escaped <> "\""
        Nothing -> tilde escaped
  where
    dq = T.any (`elem` (" #';${}%" :: String)) t
    sq = T.any (`elem` (" \"" :: String)) t
    quotes
        | dq = Just '"'
        | sq = Just '\''
        | otherwise = Nothing
    tilde s = if "~" `T.isPrefixOf` s then "\\" <> s else s
    escaped = T.pack (vis (T.unpack t))
    vis [] = []
    vis ('$' : rest@(c2 : _))
        | dq, isAlpha c2 || c2 == '_' || c2 == '{' = '\\' : '$' : vis rest
    vis (c : rest)
        | c == '"' && dq = '\\' : '"' : vis rest
        | c == '\\' = '\\' : '\\' : vis rest
        | c == '\t' = '\\' : 't' : vis rest
        | c == '\n' = '\\' : 'n' : vis rest
        | c < ' ' || c == '\DEL' = '\\' : octal3 c <> vis rest
        | otherwise = c : vis rest
    octal3 c =
        let n = fromEnum c
        in [ "01234567" !! (n `div` 64)
           , "01234567" !! (n `div` 8 `mod` 8)
           , "01234567" !! (n `mod` 8) ]

basenameT :: Text -> Text
basenameT t
    | T.null t = "."
    | T.null t' = "/"
    | otherwise = T.takeWhileEnd (/= '/') t'
  where
    t' = T.dropWhileEnd (== '/') t

dirnameT :: Text -> Text
dirnameT t
    | T.null t = "."
    | T.null t' = "/"
    | T.null d = "."
    | T.null d' = "/"
    | otherwise = d'
  where
    t' = T.dropWhileEnd (== '/') t
    d = T.dropWhileEnd (/= '/') t'
    d' = T.dropWhileEnd (== '/') d

-- format_trim_left: width-aware truncation; a straddling wide character
-- is dropped but still counts toward the limit.
trimLeft :: Int -> Text -> Text
trimLeft limit = T.pack . go 0 . T.unpack
  where
    go w cs
        | w >= limit = []
        | otherwise = case cs of
            [] -> []
            (c : rest) ->
                let cw = charWidth c
                in if w + cw <= limit
                    then c : go (w + cw) rest
                    else go (w + cw) rest

trimRight :: Int -> Text -> Text
trimRight limit t
    | total <= limit = t
    | otherwise = T.pack (go 0 (T.unpack t))
  where
    total = textWidth t
    skip = total - limit
    go w cs
        | w >= skip = cs
        | otherwise = case cs of
            [] -> []
            (c : rest) -> go (w + charWidth c) rest

-- utf8_padcstr / utf8_rpadcstr: pad with spaces to a display width.
padTo :: Int -> Text -> Text
padTo w t
    | w > 0 = t <> T.replicate (max 0 (w - cur)) " "
    | w < 0 = T.replicate (max 0 (negate w - cur)) " " <> t
    | otherwise = t
  where
    cur = textWidth t

-- Time helpers ---------------------------------------------------------

strftimeAt :: TimeZone -> Integer -> Text -> Text
strftimeAt zone t f = T.pack
    (formatTime defaultTimeLocale (T.unpack f)
        (utcToZonedTime zone (posixSecondsToUTCTime (fromIntegral t))))

-- | The caller's strftime pass over expanded output ('renderFormatCtx').
strftimePass :: FormatCtx -> Text -> Text
strftimePass ctx t
    | "%" `T.isInfixOf` t = strftimeAt ctx.tz (floor ctx.now) t
    | otherwise = t

-- format_pretty_time: a compact form chosen by the value's age.
prettyTime :: TimeZone -> POSIXTime -> Integer -> Text
prettyTime zone nowP t
    | age < 24 * 3600 = fmt "%H:%M"
    | (yT, mT) == (yN, mN) || age < 28 * 24 * 3600 = fmt "%a%d"
    | (yT == yN && mT < mN) || (yT == yN - 1 && mT > mN) = fmt "%d%b"
    | otherwise = fmt "%h%y"
  where
    nowS = max (floor nowP) t
    age = nowS - t
    fmt = strftimeAt zone t
    (yT, mT) = yearMonth t
    (yN, mN) = yearMonth nowS
    yearMonth s =
        ( readT (strftimeAt zone s "%Y")
        , readT (strftimeAt zone s "%m") )
    readT txt = fromMaybe (0 :: Integer) $ case TR.decimal txt of
        Right (v, _) -> Just v
        Left _ -> Nothing

-- format_relative_time: age as up-to-two coarse units; none for a future time.
relativeTime :: POSIXTime -> Integer -> Maybe Text
relativeTime nowP t
    | t > nowS = Nothing
    | t == nowS = Just "0s"
    | d /= 0 = Just (tshow d <> "d" <> (if h /= 0 then tshow h <> "h" else ""))
    | h /= 0 = Just (tshow h <> "h" <> (if m /= 0 then tshow m <> "m" else ""))
    | m /= 0 = Just (tshow m <> "m" <> (if s /= 0 then tshow s <> "s" else ""))
    | otherwise = Just (tshow s <> "s")
  where
    nowS = floor nowP
    age = nowS - t
    d = age `div` 86400
    h = age `mod` 86400 `div` 3600
    m = age `mod` 3600 `div` 60
    s = age `mod` 60

-- Small shared helpers -------------------------------------------------

formatFixed :: Int -> Double -> Text
formatFixed prec d
    | prec <= 0 = tshow (round d :: Integer)
    | otherwise = T.pack (showFFloat (Just prec) d "")

-- strtonum: a full-string bounded decimal, no clamping.
numFull :: Int -> Int -> Text -> Maybe Int
numFull lo hi t = case TR.signed TR.decimal t of
    Right (v, "") | v >= lo, v <= hi -> Just v
    _ -> Nothing

numDefault :: Int -> Int -> Int -> Text -> Int
numDefault lo hi dflt = fromMaybe dflt . numFull lo hi

-- strtod: full-string double; the empty string parses as zero.
parseNum :: Text -> Maybe Double
parseNum t
    | T.null t = Just 0
    | otherwise = case TR.signed TR.double t of
        Right (v, "") -> Just v
        _ -> Nothing

truthy :: Text -> Bool
truthy v = not (T.null v) && v /= "0"

boolText :: Bool -> Text
boolText b = if b then "1" else "0"

tshow :: Show a => a -> Text
tshow = T.pack . show
