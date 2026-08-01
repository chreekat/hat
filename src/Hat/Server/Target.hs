-- | tmux's @cmd-find@ target resolution as a pure function over a
-- snapshot of the server tree. Mirrors upstream @cmd-find.c@: the
-- @session:window.pane@ grammar, whole-target specials, exact\/prefix\/
-- fnmatch name matching, offset and positional tokens, and the
-- fall-back chains (pane → window → session for bare names).
module Hat.Server.Target
    ( PaneTarget (..)
    , parsePaneTarget
    -- cmd-find core
    , FindType (..)
    , World (..)
    , SessionEntry (..)
    , WindowEntry (..)
    , Found (..)
    , resolveTarget
    , resolveWindowIndex
    , paneFound
    , sessionCurrent
    , sessionCurrentFound
    , wildMatch
    ) where

import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model.Ids (PaneId (..), SessionId (..), WindowId (..))
import Hat.Server.Layout (Direction (..), neighbor)

-- | A resolved @-t@ pane target — the subset of tmux's grammar the
-- author's config exercises: the current pane (no @-t@), the last
-- (alternate) pane (@!@), the marked pane (@~@ / @{marked}@), or a
-- specific @%N@ pane id.
data PaneTarget
    = PaneCurrent
    | PaneLast
    | PaneMarked
    | PaneById Int
    deriving (Eq, Show)

parsePaneTarget :: Maybe Text -> PaneTarget
parsePaneTarget = \case
    Nothing -> PaneCurrent
    Just t -> case t of
        "!"         -> PaneLast
        "{last}"    -> PaneLast
        "~"         -> PaneMarked
        "{marked}"  -> PaneMarked
        _ | Just n <- paneId t -> PaneById n
          | otherwise          -> PaneCurrent
  where
    paneId t = do
        rest <- T.stripPrefix "%" t
        case TR.decimal rest of
            Right (n, "") -> Just n
            _             -> Nothing

-- The cmd-find core --------------------------------------------------------

-- | How a bare (separator-free) token is classified, per the command's
-- target kind: @has-session -t x@ reads @x@ as a session, @select-window@
-- as a window, @display-message@ as a pane.
data FindType = FindSession | FindWindow | FindPane
    deriving (Eq, Show)

-- | A fully resolved target: a session, a window index within it, and a
-- pane within that window.
data Found = Found
    { session  :: SessionId
    , windowIx :: Int
    , pane     :: PaneId
    }
    deriving (Eq, Show)

-- | One window in the snapshot. 'panes' is in creation order (offset and
-- index tokens count through it) with each pane's rectangle in 'area'
-- (positional and directional tokens read the geometry).
data WindowEntry = WindowEntry
    { windowId   :: WindowId
    , name       :: Text
    , panes      :: [(PaneId, Rect)]
    , activePane :: PaneId
    , lastPane   :: Maybe PaneId
    , area       :: Size
    }
    deriving (Eq, Show)

-- | One session in the snapshot; 'windows' ascends by index.
data SessionEntry = SessionEntry
    { sessionId :: SessionId
    , name      :: Text
    , windows   :: [(Int, WindowEntry)]
    , currentIx :: Int
    , lastIx    :: Maybe Int
    }
    deriving (Eq, Show)

-- | The resolution snapshot. 'current' is cmd-find's current state (the
-- invoking client's view, its @TMUX_PANE@, or the best session);
-- 'clientCurrent' is only the attached client's view (@{active}@ needs a
-- real client and errors otherwise).
data World = World
    { sessions      :: [SessionEntry]
    , paneBase      :: Int
    , marked        :: Maybe Found
    , clientCurrent :: Maybe Found
    , current       :: Maybe Found
    }
    deriving (Eq, Show)

-- | Resolve a full target string (@Nothing@ = current) to a concrete
-- session\/window\/pane, tmux @cmd_find_target@ style. Errors carry
-- tmux's exact texts (@can't find session: x@ …).
resolveTarget :: FindType -> World -> Maybe Text -> Either Text Found
resolveTarget ftype world mtarget = do
    out <- resolve ftype False world mtarget
    case out of
        OutFound f -> Right f
        -- Unreachable: index-only outcomes need the WINDOW_INDEX flag.
        OutIndex sid _ -> case sessionById world sid >>= sessionCurrent of
            Just f -> Right f
            Nothing -> Left "no current target"

-- | Resolve a @new-window@\/@move-window@-style destination: a session
-- plus a window index that may not exist yet (@Nothing@ = let the
-- command pick). A pane part is rejected (@can't specify pane here@).
resolveWindowIndex :: World -> Maybe Text -> Either Text (SessionId, Maybe Int)
resolveWindowIndex world mtarget = do
    out <- resolve FindWindow True world mtarget
    pure $ case out of
        OutIndex sid mix -> (sid, mix)
        OutFound f -> (f.session, Just f.windowIx)

data Outcome
    = OutFound Found
    | OutIndex SessionId (Maybe Int)  -- window-index mode: index may not exist

resolve :: FindType -> Bool -> World -> Maybe Text -> Either Text Outcome
resolve ftype wantIndex world mtarget = do
    cur <- maybe (Left "no current target") Right world.current
    case mtarget of
        Nothing -> Right (currentOutcome cur)
        Just t
            | T.null t -> Right (currentOutcome cur)
            | t `elem` ["@", "{active}", "{current}"] ->
                case world.clientCurrent of
                    Nothing -> Left "no current client"
                    Just f -> Right $ if wantIndex
                        then OutIndex f.session Nothing
                        else OutFound f
            | t `elem` ["=", "{mouse}"] -> Left "no mouse target"
            | t `elem` ["~", "{marked}"] ->
                case world.marked of
                    Nothing -> Left "no marked target"
                    Just f -> Right $ if wantIndex
                        then OutIndex f.session (Just f.windowIx)
                        else OutFound f
            | otherwise -> resolveParts wantIndex world cur (parts ftype t)
  where
    currentOutcome cur
        | wantIndex = OutIndex cur.session Nothing
        | otherwise = OutFound cur

-- | The split parts of a target: session\/window\/pane texts (already
-- @=@-stripped, emptied to Nothing, and mapped through the token
-- tables) plus the exact flags and the only-flags that suppress
-- bare-name fallbacks.
data Parts = Parts
    { psession    :: Maybe Text
    , pwindow     :: Maybe Text
    , ppane       :: Maybe Text
    , exactSession :: Bool
    , exactWindow  :: Bool
    , windowOnly  :: Bool  -- ^ a @:@ was present: no session fallback
    , paneOnly    :: Bool  -- ^ a @.@ was present: no window fallback
    }

parts :: FindType -> Text -> Parts
parts ftype t =
    let (rawS, rawW, rawP, wOnly, pOnly) = split
        (exS, s') = stripExact rawS
        (exW, w') = stripExact rawW
    in Parts
        { psession = mapPart sessionTable s'
        , pwindow = mapPart windowTable w'
        , ppane = mapPart paneTable rawP
        , exactSession = exS
        , exactWindow = exW
        , windowOnly = wOnly
        , paneOnly = pOnly
        }
  where
    split = case T.breakOn ":" t of
        (s, rest) | not (T.null rest) ->
            let w0 = T.drop 1 rest
            in case T.breakOn "." w0 of
                (w, prest) | not (T.null prest) ->
                    (Just s, Just w, Just (T.drop 1 prest), True, True)
                _ -> (Just s, Just w0, Nothing, True, False)
        _ -> case T.breakOn "." t of
            (w, prest) | not (T.null prest) ->
                (Nothing, Just w, Just (T.drop 1 prest), False, True)
            _ -> single
    single
        | "$" `T.isPrefixOf` t = (Just t, Nothing, Nothing, False, False)
        | "@" `T.isPrefixOf` t = (Nothing, Just t, Nothing, False, False)
        | "%" `T.isPrefixOf` t = (Nothing, Nothing, Just t, False, False)
        | otherwise = case ftype of
            FindSession -> (Just t, Nothing, Nothing, False, False)
            FindWindow -> (Nothing, Just t, Nothing, False, False)
            FindPane -> (Nothing, Nothing, Just t, False, False)
    stripExact = \case
        Just s | Just rest <- T.stripPrefix "=" s -> (True, Just rest)
        ms -> (False, ms)
    mapPart table mp = do
        p <- mp
        if T.null p then Nothing else Just (mapTable table p)
    mapTable table s = fromMaybe s (lookup s table)
    sessionTable = []
    windowTable =
        [ ("{start}", "^"), ("{last}", "!"), ("{end}", "$")
        , ("{next}", "+"), ("{previous}", "-") ]
    paneTable =
        [ ("{last}", "!"), ("{next}", "+"), ("{previous}", "-")
        , ("{top}", "top"), ("{bottom}", "bottom")
        , ("{left}", "left"), ("{right}", "right")
        , ("{top-left}", "top-left"), ("{top-right}", "top-right")
        , ("{bottom-left}", "bottom-left"), ("{bottom-right}", "bottom-right")
        ]

resolveParts :: Bool -> World -> Found -> Parts -> Either Text Outcome
resolveParts wantIndex world cur p
    -- No pane may follow when the caller wants a bare index.
    | wantIndex, Just _ <- p.ppane = Left "can't specify pane here"
    | otherwise = case (p.psession, p.pwindow, p.ppane) of
        (Just s, mw, mp) -> do
            se <- orErr ("can't find session: " <> s)
                (findSession world p.exactSession s)
            case (mw, mp) of
                (Nothing, Nothing)
                    | wantIndex -> Right (OutIndex se.sessionId Nothing)
                    | otherwise -> currentOf se
                (Just w, Nothing) -> windowIn se w
                (Nothing, Just pn) -> do
                    f <- orErr ("can't find pane: " <> pn)
                        (paneWithSession se pn)
                    Right (OutFound f)
                (Just w, Just pn) -> do
                    (ix, we) <- orErr ("can't find window: " <> w) $
                        case findWindowInSession world False p.exactWindow se w of
                            Just (WinAt ix we) -> Just (ix, we)
                            _ -> Nothing
                    pid <- orErr ("can't find pane: " <> pn)
                        (findPaneInWindow world we pn)
                    Right (OutFound (Found se.sessionId ix pid))
        (Nothing, Just w, Just pn) -> do
            (sid, ix, we) <- orErr ("can't find window: " <> w)
                (getWindow world cur False p.windowOnly p.exactWindow w)
            case we of
                Just entry -> do
                    pid <- orErr ("can't find pane: " <> pn)
                        (findPaneInWindow world entry pn)
                    Right (OutFound (Found sid (expectIx ix) pid))
                Nothing -> Left ("can't find pane: " <> pn)
        (Nothing, Just w, Nothing) -> do
            (sid, mix, mwe) <- orErr ("can't find window: " <> w)
                (getWindow world cur wantIndex p.windowOnly p.exactWindow w)
            if wantIndex
                then Right (OutIndex sid mix)
                else case (mix, mwe) of
                    (Just ix, Just we) ->
                        Right (OutFound (Found sid ix we.activePane))
                    _ -> Left ("can't find window: " <> w)
        (Nothing, Nothing, Just pn) -> do
            f <- getPane world cur p.paneOnly pn
            Right (OutFound f)
        (Nothing, Nothing, Nothing)
            | wantIndex -> Right (OutIndex cur.session Nothing)
            | otherwise -> Right (OutFound cur)
  where
    orErr e = maybe (Left e) Right
    expectIx = maybe (error "resolveParts: window entry without index") id
    currentOf se = case sessionCurrent se of
        Just f -> Right (OutFound f)
        Nothing -> Left ("can't find session: " <> se.name)
    windowIn se w = case findWindowInSession world wantIndex p.exactWindow se w of
        Just (WinAt ix we)
            | wantIndex -> Right (OutIndex se.sessionId (Just ix))
            | otherwise -> Right (OutFound (Found se.sessionId ix we.activePane))
        Just (IndexOnly ix)
            | wantIndex -> Right (OutIndex se.sessionId (Just ix))
        _ -> Left ("can't find window: " <> w)
    paneWithSession se pn
        | "%" `T.isPrefixOf` pn = do
            f <- paneFound world.sessions =<< parsePaneId pn
            if f.session == se.sessionId then Just f else Nothing
        | otherwise = do
            we <- lookup se.currentIx se.windows
            pid <- findPaneInWindow world we pn
            Just (Found se.sessionId se.currentIx pid)

-- Session matching -----------------------------------------------------------

-- | @cmd_find_get_session@: @$id@, exact name, then (unless exact-only)
-- unique prefix, then unique fnmatch. Ambiguity is a miss.
findSession :: World -> Bool -> Text -> Maybe SessionEntry
findSession world exact s
    | Just rest <- T.stripPrefix "$" s = do
        n <- parseDecimal rest
        find (\se -> se.sessionId == SessionId n) world.sessions
    | Just se <- find (\se -> se.name == s) world.sessions = Just se
    | exact = Nothing
    | otherwise = case filter (\se -> s `T.isPrefixOf` se.name) world.sessions of
        [se] -> Just se
        (_ : _) -> Nothing
        [] -> case filter (\se -> wildMatch s se.name) world.sessions of
            [se] -> Just se
            _ -> Nothing

-- Window matching ------------------------------------------------------------

-- | A window part's meaning inside one session: a real window, or (in
-- window-index mode) a bare index that need not exist yet.
data WinResult
    = WinAt Int WindowEntry
    | IndexOnly Int

-- | @cmd_find_get_window_with_session@: @\@id@, offsets, @! ^ $@,
-- numeric index, exact\/prefix\/fnmatch name matching.
findWindowInSession :: World -> Bool -> Bool -> SessionEntry -> Text -> Maybe WinResult
findWindowInSession _world wantIndex exact se w
    | Just rest <- T.stripPrefix "@" w = do
        n <- parseDecimal rest
        (ix, we) <- find (\(_, we) -> we.windowId == WindowId n) se.windows
        Just (WinAt ix we)
    | not exact, Just off <- offset w =
        if wantIndex
            then case off of
                n | n >= 0 || se.currentIx + n >= 0 ->
                    Just (IndexOnly (se.currentIx + n))
                _ -> Nothing
            else do
                ix <- cyclicIx off
                WinAt ix <$> lookup ix se.windows
    | not exact, w == "!" = do
        ix <- se.lastIx
        WinAt ix <$> lookup ix se.windows
    | not exact, w == "^" = uncurry WinAt <$> listToMaybe se.windows
    | not exact, w == "$" = uncurry WinAt <$> listToMaybe (reverse se.windows)
    | Just n <- parseDecimal w = case lookup n se.windows of
        Just we -> Just (WinAt n we)
        Nothing
            | wantIndex -> Just (IndexOnly n)
            | otherwise -> byName
    | otherwise = byName
  where
    offset t = case T.uncons t of
        Just ('+', rest) -> countOf rest
        Just ('-', rest) -> negate <$> countOf rest
        _ -> Nothing
    countOf rest
        | T.null rest = Just 1
        | otherwise = parseDecimal rest
    cyclicIx step = do
        let ixs = map fst se.windows
        pos <- lookupIndex se.currentIx ixs
        case length ixs of
            0 -> Nothing
            n -> Just (ixs !! ((pos + step) `mod` n))
    byName = case filter (\(_, we) -> we.name == w) se.windows of
        [(ix, we)] -> Just (WinAt ix we)
        (_ : _) -> Nothing
        []
            | exact -> Nothing
            | otherwise ->
                case filter (\(_, we) -> w `T.isPrefixOf` we.name) se.windows of
                    [(ix, we)] -> Just (WinAt ix we)
                    (_ : _) -> Nothing
                    [] -> case filter (\(_, we) -> wildMatch w we.name) se.windows of
                        [(ix, we)] -> Just (WinAt ix we)
                        _ -> Nothing

-- | @cmd_find_get_window@: @\@id@ anywhere, else the current session,
-- else (unless a @:@ pinned it as a window) try as a session name.
-- The window entry is 'Nothing' only in window-index mode's bare-index
-- case; the index is 'Nothing' in its session-fallback case (cmd-find
-- leaves @idx@ unset there, so the command picks a free slot).
getWindow
    :: World -> Found -> Bool -> Bool -> Bool -> Text
    -> Maybe (SessionId, Maybe Int, Maybe WindowEntry)
getWindow world cur wantIndex only exact w
    | "@" `T.isPrefixOf` w = do
        n <- parseDecimal (T.drop 1 w)
        (se, ix, we) <- findWindowGlobal world (WindowId n)
        Just (se.sessionId, Just ix, Just we)
    | otherwise = case sessionById world cur.session of
        Nothing -> Nothing
        Just se -> case findWindowInSession world wantIndex exact se w of
            Just (WinAt ix we) -> Just (se.sessionId, Just ix, Just we)
            Just (IndexOnly ix) -> Just (se.sessionId, Just ix, Nothing)
            Nothing
                | only -> Nothing
                | otherwise -> do
                    se' <- findSession world False w
                    we <- lookup se'.currentIx se'.windows
                    if wantIndex
                        then Just (se'.sessionId, Nothing, Just we)
                        else Just (se'.sessionId, Just se'.currentIx, Just we)

-- Pane matching --------------------------------------------------------------

-- | @cmd_find_get_pane_with_window@: @%id@ (must live here), @!@,
-- directional and offset tokens, numeric index, positional names.
findPaneInWindow :: World -> WindowEntry -> Text -> Maybe PaneId
findPaneInWindow world we pn
    | "%" `T.isPrefixOf` pn = do
        n <- parseDecimal (T.drop 1 pn)
        fst <$> find (\(pid, _) -> pid == PaneId n) we.panes
    | pn == "!" = do
        pid <- we.lastPane
        fst <$> find (\(p, _) -> p == pid) we.panes
    | Just dir <- directional pn = neighbor we.panes we.activePane dir
    | Just off <- offsetOf pn = do
        let order = map fst we.panes
        pos <- lookupIndex we.activePane order
        case length order of
            0 -> Nothing
            n -> Just (order !! ((pos + off) `mod` n))
    | Just n <- parseDecimal pn =
        fst <$> listToMaybe (drop (n - world.paneBase) we.panes)
    | otherwise = do
        (x, y) <- positionalPoint we.area (T.toLower pn)
        fst <$> find (\(_, r) -> containsBorderInclusive r x y) we.panes
  where
    directional = \case
        "{up-of}" -> Just DirUp
        "{down-of}" -> Just DirDown
        "{left-of}" -> Just DirLeft
        "{right-of}" -> Just DirRight
        _ -> Nothing
    offsetOf t = case T.uncons t of
        Just ('+', rest) -> countOf rest
        Just ('-', rest) -> negate <$> countOf rest
        _ -> Nothing
    countOf rest
        | T.null rest = Just 1
        | otherwise = parseDecimal rest

-- | The probe point of a @window_find_string@ positional name in a
-- window of the given size.
positionalPoint :: Size -> Text -> Maybe (Int, Int)
positionalPoint area = \case
    "top" -> Just (cx, 0)
    "bottom" -> Just (cx, sy - 1)
    "left" -> Just (0, cy)
    "right" -> Just (sx - 1, cy)
    "top-left" -> Just (0, 0)
    "top-right" -> Just (sx - 1, 0)
    "bottom-left" -> Just (0, sy - 1)
    "bottom-right" -> Just (sx - 1, sy - 1)
    _ -> Nothing
  where
    sx = fromIntegral area.cols
    sy = fromIntegral area.rows
    cx = sx `div` 2
    cy = sy `div` 2

-- | tmux's @window_get_active_at@ counts the border cell as part of the
-- pane before it, so the probe uses closed bounds on the half-open rect.
containsBorderInclusive :: Rect -> Int -> Int -> Bool
containsBorderInclusive r x y =
    r.startCol <= x && x <= r.endCol && r.startRow <= y && y <= r.endRow

-- | @cmd_find_get_pane@: @%id@ anywhere, else the current window, else
-- (unless a @.@ pinned it as a pane) fall back window-then-session.
getPane :: World -> Found -> Bool -> Text -> Either Text Found
getPane world cur only pn
    | "%" `T.isPrefixOf` pn = orMiss $ do
        n <- parseDecimal (T.drop 1 pn)
        paneFound world.sessions (PaneId n)
    | otherwise = case inCurrentWindow of
        Just f -> Right f
        Nothing
            | only -> Left missing
            | otherwise -> case getWindow world cur False False False pn of
                Just (sid, Just ix, Just we) ->
                    Right (Found sid ix we.activePane)
                _ -> Left missing
  where
    missing = "can't find pane: " <> pn
    orMiss = maybe (Left missing) Right
    inCurrentWindow = do
        se <- sessionById world cur.session
        we <- lookup cur.windowIx se.windows
        pid <- findPaneInWindow world we pn
        Just (Found se.sessionId cur.windowIx pid)

-- Snapshot lookups -----------------------------------------------------------

sessionById :: World -> SessionId -> Maybe SessionEntry
sessionById world sid = find (\se -> se.sessionId == sid) world.sessions

-- | A session's current view: its current window and that window's
-- active pane.
sessionCurrent :: SessionEntry -> Maybe Found
sessionCurrent se = do
    we <- lookup se.currentIx se.windows
    Just (Found se.sessionId se.currentIx we.activePane)

-- | Locate a pane in the snapshot as itself (its own window), e.g. for
-- the marked pane.
paneFound :: [SessionEntry] -> PaneId -> Maybe Found
paneFound entries pid = listToMaybe
    [ Found se.sessionId ix pid
    | se <- entries
    , (ix, we) <- se.windows
    , any (\(p, _) -> p == pid) we.panes
    ]

-- | The current state a @TMUX_PANE@ implies: the pane's session, viewed
-- at that session's current window (mirroring @cmd_find_from_client@'s
-- unattached-client path).
sessionCurrentFound :: [SessionEntry] -> PaneId -> Maybe Found
sessionCurrentFound entries pid = do
    f <- paneFound entries pid
    se <- find (\se -> se.sessionId == f.session) entries
    sessionCurrent se

-- | The session (first in id order) holding a window, and the window's
-- index there (preferring the session's current index).
findWindowGlobal :: World -> WindowId -> Maybe (SessionEntry, Int, WindowEntry)
findWindowGlobal world wid = listToMaybe $ mapMaybe bestIn world.sessions
  where
    bestIn se = case [ (ix, we) | (ix, we) <- se.windows, we.windowId == wid ] of
        [] -> Nothing
        hits@(h : _) ->
            let (ix, we) = fromMaybe h (find ((== se.currentIx) . fst) hits)
            in Just (se, ix, we)

parsePaneId :: Text -> Maybe PaneId
parsePaneId t = PaneId <$> (parseDecimal =<< T.stripPrefix "%" t)

parseDecimal :: Text -> Maybe Int
parseDecimal t = case TR.decimal t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

lookupIndex :: Eq a => a -> [a] -> Maybe Int
lookupIndex x xs = lookup x (zip xs [0 ..])

-- | POSIX @fnmatch@ (no flags): @*@, @?@ and @[...]@ classes with
-- ranges and @!@\/@^@ negation.
wildMatch :: Text -> Text -> Bool
wildMatch pat = go (T.unpack pat) . T.unpack
  where
    go [] s = null s
    go ('*' : ps) s = go ps s || case s of
        (_ : rest) -> go ('*' : ps) rest
        [] -> False
    go ('?' : ps) (_ : rest) = go ps rest
    go ('[' : ps) (c : rest) = case classMatch ps c of
        Just ps' -> go ps' rest
        Nothing -> False
    go (p : ps) (c : rest) = p == c && go ps rest
    go _ [] = False
    classMatch ps c =
        let (neg, body) = case ps of
                ('!' : more) -> (True, more)
                ('^' : more) -> (True, more)
                _ -> (False, ps)
            fits seen = \case
                (']' : more) -> if neg /= seen then Just more else Nothing
                (lo : '-' : hi : more) | hi /= ']' ->
                    fits (seen || (lo <= c && c <= hi)) more
                (p : more) -> fits (seen || p == c) more
                [] -> Nothing
        in fits False body
