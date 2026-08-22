-- | The #{...} format-string engine: a pure evaluator over an
-- environment map. @#(shell)@ segments are delegated to a resolver the
-- caller supplies (the server wires in a cache); strftime @%@
-- sequences are left alone for the caller to post-process.
module Hat.Server.Format
    ( FormatEnv
    , evaluate
    , evaluateAt
    , renderFormat
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    (ZonedTime, utcToZonedTime, zonedTimeToUTC, zonedTimeZone)

type FormatEnv = Map Text Text

-- | Fully render a format string: run 'evaluate', then the strftime pass
-- over the result. Because 'evaluate' escapes expansion percents, only the
-- template's own @%@ sequences reach strftime.
renderFormat :: FormatEnv -> (Text -> Text) -> ZonedTime -> Text -> Text
renderFormat env shellRes t fmt =
    T.pack (formatTime defaultTimeLocale
        (T.unpack (evaluateAt (Just t) env shellRes fmt)) t)

evaluate :: FormatEnv -> (Text -> Text) -> Text -> Text
evaluate = evaluateAt Nothing

evaluateAt :: Maybe ZonedTime -> FormatEnv -> (Text -> Text) -> Text -> Text
evaluateAt mnow env shellRes = go
  where
    go t = case T.breakOn "#" t of
        (pre, rest)
            | T.null rest -> pre
            | otherwise -> pre <> dispatch (T.drop 1 rest)

    dispatch t = case T.uncons t of
        Nothing -> "#"
        Just ('#', rest) -> "#" <> go rest
        Just ('{', rest) ->
            let (inner, rest') = balanced '{' '}' rest
            in expand inner <> go rest'
        Just ('(', rest) ->
            let (inner, rest') = balanced '(' ')' rest
            in esc (shellRes inner) <> go rest'
        Just ('S', rest) -> var "session_name" <> go rest
        Just ('I', rest) -> var "window_index" <> go rest
        Just ('W', rest) -> var "window_name" <> go rest
        Just ('F', rest) -> var "window_flags" <> go rest
        Just ('P', rest) -> var "pane_index" <> go rest
        Just ('H', rest) -> var "host" <> go rest
        Just ('T', rest) -> var "pane_title" <> go rest
        Just (',', rest) -> "," <> go rest
        Just ('}', rest) -> "}" <> go rest
        Just (c, rest) -> "#" <> T.singleton c <> go rest

    var name = esc (Map.findWithDefault "" name env)

    -- Protect literal percents from the caller's strftime pass.
    esc = T.replace "%" "%%"

    expand inner
        | Just rest <- T.stripPrefix "?" inner = conditional rest
        | Just rest <- T.stripPrefix "e|" inner = arith rest
        | Just rest <- T.stripPrefix "==:" inner = strCmp (==) rest
        | Just rest <- T.stripPrefix "!=:" inner = strCmp (/=) rest
        | Just rest <- T.stripPrefix "m:" inner = globCmp rest
        | Just rest <- T.stripPrefix "t/p:" inner = prettyTime rest
        | Just rest <- T.stripPrefix "=" inner = truncation rest
        | otherwise = var inner

    -- Comparisons work on unescaped values, so a literal @%5@ argument
    -- equals a variable that expanded to @%5@.
    plain t = T.replace "%%" "%" (go t)

    strCmp op rest = case splitTop rest of
        [aT, bT] -> bool (plain aT `op` plain bT)
        _ -> ""

    globCmp rest = case splitTop rest of
        [patT, strT] -> bool (glob (plain patT) (plain strT))
        _ -> ""

    -- tmux's @t/p@ time modifier: a recent epoch renders as HH:MM, older
    -- ones coarser (day, then date, then month-year).
    prettyTime name = case (mnow, decimalMaybe (Map.findWithDefault "" name env)) of
        (Just now, Just (secs, "")) ->
            let fireUtc = posixSecondsToUTCTime (fromIntegral secs)
                fireLocal = utcToZonedTime (zonedTimeZone now) fireUtc
                age = utcTimeToPOSIXSeconds (zonedTimeToUTC now)
                    - fromIntegral secs
                fmt | age < 24 * 3600 = "%H:%M"
                    | age < 28 * 24 * 3600 = "%a%d"
                    | age < 365 * 24 * 3600 = "%d%b"
                    | otherwise = "%h%y"
            in T.pack (formatTime defaultTimeLocale fmt fireLocal)
        _ -> ""

    conditional rest = case splitTop rest of
        (cond : thenPart : elseParts) ->
            if truthy (condValue cond)
                then go thenPart
                else go (T.intercalate "," elseParts)
        [cond] -> if truthy (condValue cond) then "" else ""
        [] -> ""
    condValue cond
        | "#" `T.isInfixOf` cond = go cond
        | otherwise = var cond
    truthy v = not (T.null v) && v /= "0"

    truncation rest = case T.breakOn ":" rest of
        (nT, rest') | Just (n, "") <- decimalMaybe nT ->
            T.take n (go ("#{" <> T.drop 1 rest' <> "}"))
        _ -> ""

    arith rest = case T.breakOn ":" rest of
        (opT, rest') -> case splitTop (T.drop 1 rest') of
            [aT, bT] ->
                let ma = number (go aT)
                    mb = number (go bT)
                in case (ma, mb) of
                    (Just a, Just b) -> applyOp opT a b
                    _ -> ""
            _ -> ""
    applyOp op a b = case op of
        ">" -> bool (a > b)
        "<" -> bool (a < b)
        ">=" -> bool (a >= b)
        "<=" -> bool (a <= b)
        "==" -> bool (a == b)
        "!=" -> bool (a /= b)
        "+" -> showNum (a + b)
        "-" -> showNum (a - b)
        "*" -> showNum (a * b)
        "/" -> if b == 0 then "" else showNum (a / b)
        _ -> ""
    bool b = if b then "1" else "0"
    showNum :: Double -> Text
    showNum x
        | x == fromIntegral (round x :: Integer) =
            T.pack (show (round x :: Integer))
        | otherwise = T.pack (show x)
    number t = case TR.signed TR.double t of
        Right (d, restT) | T.null restT -> Just d
        _ -> Nothing
    decimalMaybe t = case TR.decimal t of
        Right (n, restT) -> Just (n :: Int, restT)
        Left _ -> Nothing

-- fnmatch-style glob: @*@ any run, @?@ any one character.
glob :: Text -> Text -> Bool
glob pat str = case T.uncons pat of
    Nothing -> T.null str
    Just ('*', pr) ->
        any (glob pr) (T.tails str)
    Just ('?', pr) -> case T.uncons str of
        Just (_, sr) -> glob pr sr
        Nothing -> False
    Just (c, pr) -> case T.uncons str of
        Just (s, sr) -> c == s && glob pr sr
        Nothing -> False

-- Split on top-level commas, ignoring commas inside #{...} and #(...).
splitTop :: Text -> [Text]
splitTop = go 0 "" []
  where
    go :: Int -> Text -> [Text] -> Text -> [Text]
    go depth acc parts t = case T.uncons t of
        Nothing -> Prelude.reverse (acc : parts)
        Just (',', rest)
            | depth == 0 -> go 0 "" (acc : parts) rest
        Just (c, rest)
            | c == '{' || c == '(' -> go (depth + 1) (acc <> T.singleton c) parts rest
            | c == '}' || c == ')' -> go (max 0 (depth - 1)) (acc <> T.singleton c) parts rest
            | otherwise -> go depth (acc <> T.singleton c) parts rest

-- Consume up to the matching close bracket; returns (inner, rest).
balanced :: Char -> Char -> Text -> (Text, Text)
balanced open close = go (0 :: Int) ""
  where
    go depth acc t = case T.uncons t of
        Nothing -> (acc, "")
        Just (c, rest)
            | c == close && depth == 0 -> (acc, rest)
            | c == close -> go (depth - 1) (acc <> T.singleton c) rest
            | c == open -> go (depth + 1) (acc <> T.singleton c) rest
            | otherwise -> go depth (acc <> T.singleton c) rest
