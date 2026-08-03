-- | The window commands: creating, selecting, navigating (next\/prev\/last\/
-- activity), renaming, killing, moving, linking, and resizing windows.
module Hat.Server.Command.Window
    ( nextFreeWindowIndex
    , placeWindow
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
    ) where

import Control.Concurrent.STM
import Control.Monad (foldM, forM_, unless, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Locate (findTarget, findWindowIndexTarget, withTargetSession)
import Hat.Server.FormatEnv (refreshAutoNames)
import Hat.Server.Pane (killPaneLocs, newWindowWithPane, pickActivityTarget, sessionSpawnEnv, startPaneReader)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.View (expandFormat, sessionFormatEnv)
import qualified Hat.Server.Target as Target

-- | The first index at or above @start@ not already taken by a window, so a
-- fresh window numbers from @base-index@ exactly like @new-window@ does.
nextFreeWindowIndex :: Int -> Map.Map Int a -> Int
nextFreeWindowIndex start ws = until (\i -> not (Map.member i ws)) (+ 1) start

-- | The index @new-window@ assigns: an explicit @-t@ index verbatim (or the
-- next free slot above it under @-a@), else the next free slot after the
-- current window (@-a@) or from @base-index@ — and a collision with a live
-- window always falls back to the next free slot from @base-index@. The
-- @base-index@ is the /session/-resolved one, so @set base-index@ takes effect
-- for that session (upstream new-session-base-index.sh).
placeWindow :: Maybe Int -> Bool -> Int -> Int -> Map.Map Int a -> Int
placeWindow requested afterCurrent cur base ws =
    if Map.member ix ws then nextFreeFrom base else ix
  where
    nextFreeFrom n = nextFreeWindowIndex n ws
    ix = case requested of
        Just n
            | afterCurrent -> nextFreeFrom (n + 1)
            | otherwise    -> n
        Nothing
            | afterCurrent -> nextFreeFrom (cur + 1)
            | otherwise    -> nextFreeFrom base

cmdNewWindow :: CommandImpl
cmdNewWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "nct" args
    res <- findWindowIndexTarget st mclient (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, requested) -> do
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
            atomically $ do
                ws <- readTVar sess.windows
                cur <- readTVar sess.currentIx
                sopts <- resolveForSession st sess
                let ix' = placeWindow requested ("-a" `elem` flags) cur
                        sopts.baseIndex ws
                modifyTVar' sess.windows (Map.insert ix' win)
                unless ("-d" `elem` flags) $ do
                    writeTVar sess.lastIx (Just cur)
                    writeTVar sess.currentIx ix'
                bumpDirty st
            startPaneReader st sess.id win pane
            applySessionSize st sess.id
            pure []

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
            atomically (switchTo st sess ix)
            pure []

switchTo :: ServerState -> Session -> Int -> STM ()
switchTo st sess ix = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    when (ix /= cur) $ forM_ (Map.lookup ix ws) $ \win -> do
        writeTVar sess.lastIx (Just cur)
        writeTVar sess.currentIx ix
        writeTVar win.bellFlag False
        writeTVar win.activity False
        bumpDirty st

cmdNextWindow, cmdPrevWindow, cmdLastWindow :: CommandImpl
cmdNextWindow st mclient args
    | "-a" `elem` flags = nextActivityWindow st mclient
    | otherwise = cycleWindow st mclient 1
  where (_, flags, _) = parseArgs "t" args
cmdPrevWindow st mclient _ = cycleWindow st mclient (-1)
cmdLastWindow st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            mlast <- readTVar sess.lastIx
            forM_ mlast (switchTo st sess)
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
                WithLastFallback -> readTVar sess.lastIx
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
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        current <- readTVarIO sess.lastSize
        let sz = current
                { cols = fromMaybe current.cols (parseInt =<< lookup "-x" opts)
                , rows = fromMaybe current.rows (parseInt =<< lookup "-y" opts)
                }
        atomically $ writeTVar sess.lastSize sz
        applySessionSize st sess.id
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
                Right (_, _, win, _) -> do
                    -- An empty name hands the window back to
                    -- automatic-rename; a real name pins it.
                    if T.null nm
                        then do
                            atomically (writeTVar win.autoRename True)
                            refreshAutoNames st
                        else atomically $ do
                            writeTVar win.name nm
                            writeTVar win.autoRename False
                            bumpDirty st
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
                            writeTVar dstSess.lastIx (Just cur)
                            writeTVar dstSess.currentIx dstIx
                        bumpDirty st
                        pure (Right ())
            case res of
                Left e -> pure [RErr e]
                Right () -> applySessionSize st dstSess.id >> pure []
        (Left e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]
