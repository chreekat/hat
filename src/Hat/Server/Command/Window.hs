-- | The window commands: creating, selecting, navigating (next\/prev\/last\/
-- activity), renaming, killing, moving, linking, and resizing windows.
module Hat.Server.Command.Window
    ( nextFreeWindowIndex
    , Insert (..)
    , Replace (..)
    , WindowPlacement (..)
    , placeWindow
    , applyShifts
    , selectNamed
    , cmdNewWindow
    , cmdSelectWindow
    , cmdNextWindow
    , cmdPrevWindow
    , cmdLastWindow
    , cmdActivityWindow
    , jumpToActivity
    , cycleWindow
    , cmdResizeWindow
    , cmdKillWindow
    , cmdRenameWindow
    , cmdMoveWindow
    , cmdLinkWindow
    , cmdUnlinkWindow
    , switchTo
    ) where

import Control.Concurrent.STM
import Control.Monad (foldM, forM, forM_, unless, void, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Mru (recordVisit)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), notify, notifyWindow
    , sessionTarget )
import Hat.Server.Locate (findTarget, findWindowIndexTarget, withTargetSession)
import Hat.Server.FormatEnv (refreshAutoNames)
import Hat.Server.Pane
    ( LifecycleNotify (..), killPaneLocs, killPaneLocsWith, newWindowWithPane
    , pickActivityTarget, sessionSpawnEnv, startPaneReader )
import Hat.Server.Resize (applySessionSize)
import Hat.Server.View (expandFormat, sessionFormatEnv)
import qualified Hat.Server.Target as Target

-- | The first index at or above @start@ not already taken by a window, so a
-- fresh window numbers from @base-index@ exactly like @new-window@ does.
nextFreeWindowIndex :: Int -> Map.Map Int a -> Int
nextFreeWindowIndex start ws = until (\i -> not (Map.member i ws)) (+ 1) start

-- | Where @new-window@ inserts relative to its target: at the target index
-- itself, or shuffled in after (@-a@) \/ before (@-b@) the target window.
data Insert = InsertAt | InsertAfter | InsertBefore
    deriving (Eq, Show)

-- | @-k@: whether an occupied target index is replaced or rejected.
data Replace = Replace | NoReplace
    deriving (Eq, Show)

-- | The tree mutations that place a @new-window@: windows to renumber
-- (highest source first — see 'applyShifts'), the occupant a @-k@ kills,
-- and the index the new window lands at.
data WindowPlacement = WindowPlacement
    { shifts   :: [(Int, Int)]
    , replaced :: Maybe Int
    , index    :: Int
    }
    deriving (Eq, Show)

-- | The placement @new-window@ performs: an explicit @-t@ index (occupied →
-- error, or replaced under @-k@), a shuffle-up insertion after\/before the
-- target window (@-a@\/@-b@, tmux's @winlink_shuffle_up@), or the next free
-- slot from @base-index@ — the /session/-resolved one, so @set base-index@
-- takes effect for that session (upstream new-session-base-index.sh).
placeWindow
    :: Maybe Int -> Insert -> Replace -> Int -> Int -> Map.Map Int a
    -> Either Text WindowPlacement
placeWindow requested insert replace cur base ws = case insert of
    InsertAfter  -> Right (shuffleUp 1)
    InsertBefore -> Right (shuffleUp 0)
    InsertAt -> case requested of
        Just n
            | Map.member n ws -> case replace of
                Replace -> Right WindowPlacement
                    { shifts = [], replaced = Just n, index = n }
                NoReplace -> Left
                    ("create window failed: index " <> tshow n <> " in use")
            | otherwise -> Right (plain n)
        Nothing -> Right (plain (nextFreeWindowIndex base ws))
  where
    plain n = WindowPlacement { shifts = [], replaced = Nothing, index = n }
    -- Renumber the occupied run at the insertion point up by one; a @-t@
    -- index with no live window is taken directly, as tmux does.
    shuffleUp off = case requested of
        Just n | not (Map.member n ws) -> plain n
        mreq ->
            let ix = fromMaybe cur mreq + off
                free = nextFreeWindowIndex ix ws
            in WindowPlacement
                { shifts = [(i, i + 1) | i <- [free - 1, free - 2 .. ix]]
                , replaced = Nothing
                , index = ix
                }

-- | Apply a placement's renumberings; sources run highest-first so a shift
-- never lands on a slot another shift has yet to vacate.
applyShifts :: [(Int, Int)] -> Map.Map Int a -> Map.Map Int a
applyShifts shifts ws = List.foldl' step ws shifts
  where
    step m (from, to) = case Map.lookup from m of
        Just w -> Map.insert to w (Map.delete from m)
        Nothing -> m

-- | The window @new-window -S -n name@ reuses instead of creating:
-- 'Nothing' when no window carries the name, its index when exactly one
-- does, an error when several do.
selectNamed :: Text -> [(Int, Text)] -> Either Text (Maybe Int)
selectNamed nm named = case [ix | (ix, n) <- named, n == nm] of
    []   -> Right Nothing
    [ix] -> Right (Just ix)
    _    -> Left ("multiple windows named " <> nm)

cmdNewWindow :: CommandImpl
cmdNewWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "nct" args
    res <- findWindowIndexTarget st mclient (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, requested) -> do
            reuse <- case (lookup "-n" opts, requested) of
                (Just nm, Nothing) | "-S" `elem` flags -> atomically $ do
                    ws <- readTVar sess.windows
                    named <- mapM (\(ix, w) -> (,) ix <$> readTVar w.name)
                        (Map.toList ws)
                    pure (selectNamed nm named)
                _ -> pure (Right Nothing)
            case reuse of
                Left e -> pure [RErr e]
                Right (Just ix) -> do
                    unless ("-d" `elem` flags) $ switchToWindow st sess ix
                    pure []
                Right Nothing -> createWindow st sess opts flags pos requested

-- | The creating body of @new-window@: reject an unusable @-t@ before
-- spawning, spawn the pane, then place it in one transaction — only after
-- which any @-k@ occupant (already displaced from the index map) is killed,
-- so the session never passes through empty.
createWindow
    :: ServerState -> Session -> [(Text, Text)] -> [Text] -> [Text]
    -> Maybe Int -> IO [Reply]
createWindow st sess opts flags pos requested = do
    let insert
            | "-b" `elem` flags = InsertBefore
            | "-a" `elem` flags = InsertAfter
            | otherwise = InsertAt
        replace = if "-k" `elem` flags then Replace else NoReplace
        plan = do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            sopts <- resolveForSession st sess
            pure (placeWindow requested insert replace cur sopts.baseIndex ws)
    precheck <- atomically plan
    case precheck of
        Left e -> pure [RErr e]
        Right _ -> do
            eff <- readTVarIO sess.lastSize
            environ <- sessionSpawnEnv st sess
            let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" environ)
                mrun = case pos of
                    [] -> Nothing
                    ws -> Just (T.unwords ws)
            dir <- case lookup "-c" opts of
                Nothing -> readTVarIO sess.startCwd
                Just d -> do
                    env <- sessionFormatEnv st sess
                    T.unpack <$> expandFormat st env d
            (win, pane) <- newWindowWithPane st sess.id shellCmd mrun dir
                environ (eff)
            forM_ (lookup "-n" opts) $ \nm -> atomically $ do
                writeTVar win.name nm
                writeTVar win.autoRename False
            moccupant <- atomically $ do
                ws <- readTVar sess.windows
                cur <- readTVar sess.currentIx
                hist <- readTVar sess.windowHist
                -- Replan on the live tree; a slot filled since the
                -- pre-check falls to a free index rather than clobbering.
                p <- either (const (freeSlot ws)) pure =<< plan
                let remap i = fromMaybe i (List.lookup i p.shifts)
                    wasCurrent = p.replaced == Just cur
                    -- The killed occupant leaves the history; survivors shift
                    -- with the insertion.
                    scrubbed = filter (\l -> Just l /= p.replaced) (map remap hist)
                writeTVar sess.windows
                    (Map.insert p.index win (applyShifts p.shifts ws))
                -- A replaced current window always selects the new one
                -- (tmux drops @-d@ there rather than leave a dead current).
                if "-d" `elem` flags && not wasCurrent
                    then do
                        writeTVar sess.currentIx (remap cur)
                        writeTVar sess.windowHist scrubbed
                    else do
                        writeTVar sess.windowHist
                            (if wasCurrent then scrubbed
                                           else recordVisit (remap cur) p.index scrubbed)
                        writeTVar sess.currentIx p.index
                bumpDirty st
                pure ((`Map.lookup` ws) =<< p.replaced)
            forM_ moccupant $ \old -> do
                ps <- readTVarIO old.panes
                killPaneLocs st [(sess.id, old, p) | p <- Map.elems ps]
            startPaneReader st sess.id win pane
            applySessionSize st sess.id
            sname <- readTVarIO sess.name
            notifyWindow st "window-created" (Just sess.id) win []
            notifyWindow st "window-linked" (Just sess.id) win
                [ ("session", PSessionRef sess.id sname) ]
            pure []
  where
    freeSlot ws = do
        sopts <- resolveForSession st sess
        pure WindowPlacement
            { shifts = [], replaced = Nothing
            , index = nextFreeWindowIndex sopts.baseIndex ws }

cmdSelectWindow :: CommandImpl
cmdSelectWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
        target = case (lookup "-t" opts, pos) of
            (Just t, _) -> Just t
            (Nothing, [t]) -> Just t
            _ -> Nothing
    res <- findTarget st mclient Target.FindWindow target
    case res of
        Left e -> pure [RErr e]
        Right (sess, ix, _, _) -> do
            switchToWindow st sess ix
            pure []

switchTo :: ServerState -> Session -> Int -> STM ()
switchTo st sess ix = void (switchToInfo st sess ix)

-- Like 'switchTo', reporting the change for session-window-changed:
-- (old index, old window, new index, new window).
switchToInfo
    :: ServerState -> Session -> Int
    -> STM (Maybe (Int, Maybe Window, Int, Window))
switchToInfo st sess ix = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    if ix == cur then pure Nothing else case Map.lookup ix ws of
        Nothing -> pure Nothing
        Just win -> do
            modifyTVar' sess.windowHist (recordVisit cur ix)
            writeTVar sess.currentIx ix
            writeTVar win.bellFlag False
            writeTVar win.activity False
            bumpDirty st
            pure (Just (cur, Map.lookup cur ws, ix, win))

-- | 'switchTo' plus its session-window-changed notification.
switchToWindow :: ServerState -> Session -> Int -> IO ()
switchToWindow st sess ix = do
    minfo <- atomically (switchToInfo st sess ix)
    forM_ minfo $ \(oldIx, mold, newIx, win) -> do
        sname <- readTVarIO sess.name
        wname <- readTVarIO win.name
        oldRef <- case mold of
            Nothing -> pure []
            Just old -> do
                oname <- readTVarIO old.name
                pure [ ("old_window", PWindowRef old.id oname)
                     , ("old_window_index", PInt oldIx) ]
        notify st "session-window-changed"
            (NotifyTarget (Just sess.id) (Just win.id) Nothing)
            ([ ("session", PSessionRef sess.id sname)
             , ("window", PWindowRef win.id wname)
             , ("new_window", PWindowRef win.id wname)
             , ("new_window_index", PInt newIx)
             ] <> oldRef)

cmdNextWindow, cmdPrevWindow, cmdLastWindow :: CommandImpl
cmdNextWindow st mclient args
    | "-a" `elem` flags = nextActivityWindow st mclient
    | otherwise = cycleWindow st mclient 1
  where (_, flags, _) = parseArgs "t" args
cmdPrevWindow st mclient _ = cycleWindow st mclient (-1)
cmdLastWindow st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        mix <- atomically (listToMaybe <$> readTVar sess.windowHist)
        forM_ mix (switchToWindow st sess)
        pure []

-- | @next-window -a@: switch to the next window (cyclically) that has an
-- activity flag set.
nextActivityWindow :: ServerState -> Maybe Client -> IO [Reply]
nextActivityWindow st mclient = jumpToActivity st mclient WithoutLastFallback

-- | The @<leader> a@ jump: prioritize a window carrying an activity flag,
-- degrading to @last-window@ when none does.
cmdActivityWindow :: CommandImpl
cmdActivityWindow st mclient _ = jumpToActivity st mclient WithLastFallback

-- | Whether an activity jump degrades to @last-window@ when nothing is
-- flagged (@<leader> a@) or simply stays put (@next-window -a@).
data ActivityFallback = WithLastFallback | WithoutLastFallback

-- | Shared body of the activity jumps: pick 'pickActivityTarget' over the
-- session's live activity flags and switch there.
jumpToActivity :: ServerState -> Maybe Client -> ActivityFallback -> IO [Reply]
jumpToActivity st mclient fallback =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            mfallback <- case fallback of
                WithLastFallback -> listToMaybe <$> readTVar sess.windowHist
                WithoutLastFallback -> pure Nothing
            flagged <- foldM (\acc (ix, win) -> do
                a <- readTVar win.activity
                pure (if a then Set.insert ix acc else acc))
                Set.empty (Map.toList ws)
            forM_ (pickActivityTarget (Map.keys ws) cur flagged mfallback)
                (switchTo st sess)
        pure []

cycleWindow :: ServerState -> Maybe Client -> Int -> IO [Reply]
cycleWindow st mclient step =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let ixs = Map.keys ws
            case ixs of
                [] -> pure ()
                _ -> do
                    let n = length ixs
                        curPos = fromMaybe 0 (List.elemIndex cur ixs)
                        ix = ixs !! ((curPos + step + n) `mod` n)
                    switchTo st sess ix
        pure []

cmdResizeWindow :: CommandImpl
cmdResizeWindow st mclient args = do
    let (opts, _, _) = parseArgs "txy" args
        parseInt t = case TR.decimal t of
            Right (n, rest) | T.null rest -> Just n
            _ -> Nothing
    res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
    case res of
      Left e -> pure [RErr e]
      Right (sess, _, win, _) -> do
        current <- readTVarIO sess.lastSize
        let sz = current
                { cols = fromMaybe current.cols (parseInt =<< lookup "-x" opts)
                , rows = fromMaybe current.rows (parseInt =<< lookup "-y" opts)
                }
        atomically $ writeTVar sess.lastSize sz
        applySessionSize st sess.id
        notifyWindow st "window-resized" (Just sess.id) win
            [ ("old_width", PInt (fromIntegral current.cols))
            , ("old_height", PInt (fromIntegral current.rows))
            , ("width", PInt (fromIntegral sz.cols))
            , ("height", PInt (fromIntegral sz.rows)) ]
        pure []

cmdKillWindow :: CommandImpl
cmdKillWindow st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, _, win, _) -> do
            ps <- readTVarIO win.panes
            killPaneLocs st [(sess.id, win, p) | p <- Map.elems ps]
            pure []

cmdRenameWindow :: CommandImpl
cmdRenameWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> do
            res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
            case res of
                Right (sess, _, win, _) -> do
                    -- An empty name hands the window back to
                    -- automatic-rename; a real name pins it.
                    if T.null nm
                        then do
                            atomically (writeTVar win.autoRename True)
                            refreshAutoNames st
                        else do
                            atomically $ do
                                writeTVar win.name nm
                                writeTVar win.autoRename False
                                bumpDirty st
                            notifyWindow st "window-renamed"
                                (Just sess.id) win []
                    pure []
                Left e -> pure [RErr e]
        _ -> pure [RErr "usage: rename-window [-t target] name"]

-- | @move-window -s src -t dst@: renumber (or relocate) a window to the
-- destination index, possibly in another session. Restore replays this
-- to place windows at their saved indices.
cmdMoveWindow :: CommandImpl
cmdMoveWindow st mclient args = do
    let (opts, _, _) = parseArgs "st" args
    esrc <- findTarget st mclient Target.FindWindow (lookup "-s" opts)
    edst <- findWindowIndexTarget st mclient (lookup "-t" opts)
    case (esrc, edst) of
        (Right (srcSess, srcIx, _, _), Right (dstSess, mdstIx)) -> do
            res <- atomically $ do
                sws <- readTVar srcSess.windows
                dws <- readTVar dstSess.windows
                sopts <- resolveForSession st dstSess
                let dstIx = fromMaybe
                        (until (`Map.notMember` dws) (+ 1) sopts.baseIndex)
                        mdstIx
                case Map.lookup srcIx sws of
                    Nothing -> pure (Right ())  -- nothing to move
                    Just win
                        | srcSess.id == dstSess.id, srcIx == dstIx ->
                            pure (Right ())  -- already there
                        | otherwise -> do
                            if Map.member dstIx dws
                                then pure (Left ("can't move window: "
                                    <> tshow dstIx <> " in use"))
                                else do
                                    modifyTVar' srcSess.windows (Map.delete srcIx)
                                    modifyTVar' dstSess.windows (Map.insert dstIx win)
                                    followFocus srcSess dstSess srcIx dstIx
                                    bumpDirty st
                                    pure (Right ())
            pure [RErr e | Left e <- [res]]
        (Left e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]
  where
    -- The moved window keeps the focus: within a session the current
    -- index follows it to the destination; across sessions the source
    -- session falls back to its lowest remaining window.
    followFocus srcSess dstSess srcIx dstIx = do
        cur <- readTVar srcSess.currentIx
        when (cur == srcIx) $
            if srcSess.id == dstSess.id
                then writeTVar srcSess.currentIx dstIx
                else do
                    ws' <- readTVar srcSess.windows
                    forM_ (Map.lookupMin ws') $ \(i, _) ->
                        writeTVar srcSess.currentIx i

-- | @link-window -s src -t dst@: link the source window into the
-- destination session at the given index (the same window, shared) and,
-- without @-d@, make it current there.
cmdLinkWindow :: CommandImpl
cmdLinkWindow st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
    esrc <- findTarget st mclient Target.FindWindow (lookup "-s" opts)
    edst <- findWindowIndexTarget st mclient (lookup "-t" opts)
    case (esrc, edst) of
        (Right (_, _, win, _), Right (dstSess, mdstIx)) -> do
            res <- atomically $ do
                dws <- readTVar dstSess.windows
                sopts <- resolveForSession st dstSess
                let dstIx = fromMaybe
                        (until (`Map.notMember` dws) (+ 1) sopts.baseIndex)
                        mdstIx
                if Map.member dstIx dws
                    then pure (Left ("index in use: " <> tshow dstIx))
                    else do
                        modifyTVar' dstSess.windows (Map.insert dstIx win)
                        unless ("-d" `elem` flags) $ do
                            cur <- readTVar dstSess.currentIx
                            modifyTVar' dstSess.windowHist (recordVisit cur dstIx)
                            writeTVar dstSess.currentIx dstIx
                        bumpDirty st
                        sname <- readTVar dstSess.name
                        pure (Right sname)
            case res of
                Left e -> pure [RErr e]
                Right sname -> do
                    applySessionSize st dstSess.id
                    notifyWindow st "window-linked" (Just dstSess.id) win
                        [ ("session", PSessionRef dstSess.id sname) ]
                    pure []
        (Left e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]

-- | @unlink-window [-k] -t target@: remove the window from its session
-- without killing it, provided it stays linked somewhere (or @-k@ kills).
cmdUnlinkWindow :: CommandImpl
cmdUnlinkWindow st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
    res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, wix, win, _) -> do
            elsewhere <- atomically $ do
                allSess <- readTVar st.sessions
                fmap Prelude.or . forM
                    [ other | (osid, other) <- Map.toList allSess
                    , osid /= sess.id ] $ \other -> do
                        ows <- readTVar other.windows
                        pure (any (\w -> w.id == win.id) (Map.elems ows))
            if not elsewhere && "-k" `notElem` flags
                then pure [RErr "window only linked to one session"]
                else do
                    (sname, wname) <- atomically $ do
                        sname <- readTVar sess.name
                        wname <- readTVar win.name
                        ws <- readTVar sess.windows
                        let ws' = Map.delete wix ws
                        writeTVar sess.windows ws'
                        cur <- readTVar sess.currentIx
                        when (cur == wix) $
                            forM_ (Map.lookupMin ws') $ \(ix, _) ->
                                writeTVar sess.currentIx ix
                        modifyTVar' sess.windowHist (filter (/= wix))
                        bumpDirty st
                        pure (sname, wname)
                    notify st "window-unlinked" (sessionTarget sess.id)
                        [ ("session", PSessionRef sess.id sname)
                        , ("window", PWindowRef win.id wname) ]
                    when (not elsewhere) $ do
                        ps <- readTVarIO win.panes
                        killPaneLocsWith QuietLifecycle st
                            [ (sess.id, win, p) | p <- Map.elems ps ]
                    applySessionSize st sess.id
                    pure []
