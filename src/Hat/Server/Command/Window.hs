-- | The window commands: creating, selecting, navigating (next\/prev\/last\/
-- activity), renaming, killing, moving, linking, and resizing windows.
module Hat.Server.Command.Window
    ( nextFreeWindowIndex
    , Insert (..)
    , Replace (..)
    , WindowPlacement (..)
    , Renumbered (..)
    , renumberSession
    , placeWindow
    , applyShifts
    , selectNamed
    , cmdNewWindow
    , cmdSelectWindow
    , cmdNextWindow
    , cmdPrevWindow
    , cmdLastWindow
    , cmdActivityWindow
    , cmdResizeWindow
    , cmdKillWindow
    , cmdRenameWindow
    , cmdMoveWindow
    , cmdLinkWindow
    , cmdUnlinkWindow
    , cmdSwapWindow
    , cmdRotateWindow
    , closeSessions
    , switchTo
    , switchToWindow
    , SourceLink (..)
    , linkWindow
    ) where

import Control.Concurrent.STM
import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Mru (recordVisit)
import Hat.Command.Arg (Arg (..))
import Hat.Server.Command.Types
    (CommandImpl, Reply (..), parseArgs, strtonum, unknownFlag)
import Hat.Server.Name (NameKind (..), admitName)
import Hat.Server.ClientSession (sendClientsAway)
import Hat.Server.Format (truthy)
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), noTarget, notify, notifyGroupDrops
    , notifyWindow, sessionTarget )
import Hat.Server.Layout (Rotation (..), layoutPanes, rotateLeaves)
import Hat.Server.Winlink
    ( Step (..), Renumbered (..), stepWindow, attachWindow, detachWindow, dropSessionGroup
    , groupMembers
    , renumberIfSet, renumberSession, repackSession, shuffleUp, windowLinks )
import Hat.Server.Locate
    (findTarget, findWindowIndexTarget, formatClient, paneIndexOf, withTargetSession)
import Hat.Server.Pane
    ( LifecycleNotify (..), killPaneLocs, killPaneLocsWith, launchOf, newWindowWithPane
    , paneShell, pickActivityTarget, rehomePanes, sessionSpawnEnv, startPaneReader
    , withDefaultCommand
    , windowActivity )
import Hat.Server.Resize
    ( applySessionSize, clampWindowSize, claimLatest, clientWindowArea, newWindowSize
    , recalculateSizes, windowResized )
import Hat.Server.FormatEnv
    (expandFormat, paneFormatEnv, sessionFormatEnv, windowFormatEnv)
import Hat.Server.Target qualified as Target

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
    InsertAfter  -> Right (shuffled 1)
    InsertBefore -> Right (shuffled 0)
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
    shuffled off = case requested of
        Just n | not (Map.member n ws) -> plain n
        mreq ->
            let ix = fromMaybe cur mreq + off
            in WindowPlacement
                { shifts = shuffleUp ix ws
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
cmdNewWindow st mclient args
    | Just e <- unknownFlag "new-window" "abdEkPS" flags = pure [RErr e]
    | Left e <- launchOf ("-E" `elem` flags) pos = pure [RErr e]
    | otherwise = do
        res <- findWindowIndexTarget st mclient (lookup "-t" opts0)
        case res of
            Left e -> pure [RErr e]
            Right (sess, requested) -> do
                env <- sessionFormatEnv st sess
                opts <- mapM (expandName env) opts0
                let refused = [ r | Just nm <- [lookup "-n" opts]
                                  , Left r <- [admitName WindowName (Utf8Arg nm)] ]
                reuse <- case (lookup "-n" opts, requested) of
                    (Just nm, Nothing) | "-S" `elem` flags -> atomically $ do
                        ws <- readTVar sess.windows
                        named <- mapM (\(ix, w) -> (,) ix <$> readTVar w.name)
                            (Map.toList ws)
                        pure (selectNamed nm named)
                    _ -> pure (Right Nothing)
                case reuse of
                    _ | r : _ <- refused -> pure [r]
                    Left e -> pure [RErr e]
                    Right (Just ix) -> do
                        unless ("-d" `elem` flags) $ switchToWindow st sess ix
                        pure []
                    Right Nothing -> createWindow st mclient sess opts flags pos requested
  where
    (opts0, flags, pos) = parseArgs "nctF" args
    expandName env = \case
        ("-n", nm) -> (,) "-n" <$> expandFormat st env nm
        o -> pure o

-- | The creating body of @new-window@: reject an unusable @-t@ before
-- spawning, spawn the pane, then place it in one transaction — only after
-- which any @-k@ occupant (already displaced from the index map) is killed,
-- so the session never passes through empty.
createWindow
    :: ServerState -> Maybe Client -> Session -> [(Text, Text)] -> [Text] -> [Text]
    -> Maybe Int -> IO [Reply]
createWindow st mclient sess opts flags pos requested = do
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
            tc <- either (const Nothing) Just
                <$> formatClient st mclient (Just sess.id) Nothing
            eff <- atomically (newWindowSize st tc sess)
            environ <- sessionSpawnEnv st sess
            sopts <- resolveForSessionIO st sess
            shellCmd <- paneShell sopts
            let (how, kind) = either (const (FreshShell, ProgramPane)) id
                    (launchOf ("-E" `elem` flags) pos)
            dir <- case lookup "-c" opts of
                Nothing -> readTVarIO sess.startCwd
                Just d -> do
                    env <- sessionFormatEnv st sess
                    T.unpack <$> expandFormat st env d
            (win, pane) <- newWindowWithPane st sess.id shellCmd
                (withDefaultCommand (optionText "default-command" sopts) how) kind
                dir environ eff ((.id) <$> tc)
            forM_ (lookup "-n" opts) $ \nm -> atomically $ do
                writeTVar win.name nm
                writeTVar win.autoRename False
            (wix, moccupant) <- atomically $ do
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
                let occupant = (`Map.lookup` ws) =<< p.replaced
                -- A displaced window linked elsewhere survives there.
                orphan <- case occupant of
                    Just old -> windowLinks st old >>= \case
                        [] -> pure (Just old)
                        (holder, _) : _ -> Nothing <$ rehomePanes holder.id old
                    Nothing -> pure Nothing
                pure (p.index, orphan)
            forM_ moccupant $ \old -> do
                ps <- atomically (windowPanes old)
                killPaneLocs st [(sess.id, old, p) | p <- ps]
            startPaneReader st sess.id win pane
            applySessionSize st sess.id
            -- Creation counts as activity for a monitored window.
            windowActivity st sess.id win
            sname <- readTVarIO sess.name
            notifyWindow st "window-created" (Just sess.id) win []
            notifyWindow st "window-linked" (Just sess.id) win
                [ ("session", PSessionRef sess.id sname) ]
            if "-P" `elem` flags
                then do
                    pix <- paneIndexOf st win pane
                    env <- paneFormatEnv st sess wix win pix pane
                    out <- expandFormat st env $ fromMaybe
                        "#{session_name}:#{window_index}.#{pane_index}"
                        (lookup "-F" opts)
                    pure [ROutput out]
                else pure []
  where
    freeSlot ws = do
        sopts <- resolveForSession st sess
        pure WindowPlacement
            { shifts = [], replaced = Nothing
            , index = nextFreeWindowIndex sopts.baseIndex ws }

-- | @select-window@: select the target window; @-n@\/@-p@\/@-l@ step
-- instead ('stepWindow'), and @-T@ on the current window toggles to the
-- last one.
cmdSelectWindow :: CommandImpl
cmdSelectWindow st mclient args
    | Just e <- unknownFlag "select-window" "lnpT" flags = pure [RErr e]
    | "-n" `elem` flags = stepCommand StepNext
    | "-p" `elem` flags = stepCommand StepPrevious
    | "-l" `elem` flags = stepCommand StepLast
    | otherwise = do
        res <- findTarget st mclient Target.FindWindow target
        case res of
            Left e -> pure [RErr e]
            Right (sess, ix, _, _) -> do
                cur <- readTVarIO sess.currentIx
                selectedBy st mclient sess =<< if "-T" `elem` flags && ix == cur
                    then stepSession st sess False StepLast
                    else [] <$ switchToWindow st sess ix
  where
    (opts, flags, pos) = parseArgs "t" args
    target = case (lookup "-t" opts, pos) of
        (Just t, _) -> Just t
        (Nothing, [t]) -> Just t
        _ -> Nothing
    stepCommand step = do
        res <- findTarget st mclient Target.FindWindow target
        either (pure . pure . RErr)
            (\(sess, _, _, _) -> selectedBy st mclient sess =<< stepSession st sess False step) res

-- | After a window selection, the selecting client becomes the latest
-- client of the window @sess@ now shows (tmux's @cmd_select_window_exec@).
selectedBy :: ServerState -> Maybe Client -> Session -> [Reply] -> IO [Reply]
selectedBy st mclient sess replies = do
    unless (any isErr replies) $ atomically $ do
        forM_ mclient $ \c -> claimLatest st c sess
        recalculateSizes st
        bumpDirty st
    pure replies
  where
    isErr = \case
        RErr _ -> True
        _ -> False

-- | Move the session's current window one 'Step', alerted windows only
-- when @alerts@, failing with tmux's words when there is nowhere to go.
stepSession :: ServerState -> Session -> Bool -> Step -> IO [Reply]
stepSession st sess alerts step = do
    mix <- atomically $ do
        ws <- readTVar sess.windows
        cur <- readTVar sess.currentIx
        hist <- readTVar sess.windowHist
        flagged <- if alerts
            then Just . Set.fromList . map fst <$> filterM (alerted . snd)
                (Map.toList ws)
            else pure Nothing
        pure (stepWindow step flagged (Map.keys ws) cur hist)
    case mix of
        Just ix -> [] <$ switchToWindow st sess ix
        Nothing -> pure [RErr $ case step of
            StepNext -> "no next window"
            StepPrevious -> "no previous window"
            StepLast -> "no last window"]
  where
    alerted win = or <$> mapM readTVar [win.bellFlag, win.activity, win.silenceFlag]

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
            writeTVar win.silenceFlag False
            -- On a detached session the selection itself counts as
            -- activity, so a monitored window's flag stays raised.
            attached <- not . null <$> sessionClients st sess.id
            wopts <- resolveForWindow st sess win
            writeTVar win.activity (not attached && wopts.monitorActivity)
            -- See Note [Sizing per window]
            recalculateSizes st
            bumpDirty st
            pure (Just (cur, Map.lookup cur ws, ix, win))

-- | 'switchTo' plus its session-window-changed notification.
switchToWindow :: ServerState -> Session -> Int -> IO ()
switchToWindow st sess ix =
    atomically (switchToInfo st sess ix) >>= mapM_ (announceSwitch st sess)

-- | Fire session-window-changed for a switch 'switchToInfo' reported.
announceSwitch
    :: ServerState -> Session -> (Int, Maybe Window, Int, Window) -> IO ()
announceSwitch st sess (oldIx, mold, newIx, win) = do
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
cmdNextWindow = sessionStep "next-window" StepNext
cmdPrevWindow = sessionStep "previous-window" StepPrevious
cmdLastWindow = sessionStep "last-window" StepLast

-- | The session-targeted step commands: @next-window@ and
-- @previous-window@ (@-a@: alerted windows only) and @last-window@.
sessionStep :: Text -> Step -> CommandImpl
sessionStep name step st mclient args
    | Just e <- unknownFlag name (if step == StepLast then "" else "a") flags =
        pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindSession (lookup "-t" opts)
        either (pure . pure . RErr)
            (\(sess, _, _, _) ->
                selectedBy st mclient sess =<< stepSession st sess ("-a" `elem` flags) step)
            res
  where
    (opts, flags, _) = parseArgs "t" args

-- | The @<leader> a@ jump: prioritize a window carrying an activity flag
-- ('pickActivityTarget'), degrading to @last-window@ when none does.
cmdActivityWindow :: CommandImpl
cmdActivityWindow st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            mfallback <- listToMaybe <$> readTVar sess.windowHist
            flagged <- foldM (\acc (ix, win) -> do
                a <- readTVar win.activity
                pure (if a then Set.insert ix acc else acc))
                Set.empty (Map.toList ws)
            forM_ (pickActivityTarget (Map.keys ws) cur flagged mfallback)
                (switchTo st sess)
        pure []

-- | @resize-window@: size a window by hand. Like tmux, any resize leaves the
-- window at @window-size manual@, so attached clients stop moving it.
cmdResizeWindow :: CommandImpl
cmdResizeWindow st mclient args
    | bad : _ <- filter (`notElem` ["-a", "-A", "-D", "-L", "-R", "-U"]) flags =
        pure [RErr ("resize-window: unknown flag " <> bad)]
    | length pos > 1 = pure [RErr usage]
    | otherwise = case parsed of
        Left e -> pure [RErr e]
        Right (mx, my, adjust) -> do
            res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
            case res of
              Left e -> pure [RErr e]
              Right (sess, _, win, _) -> do
                current <- readTVarIO win.size
                let given = current
                        { cols = maybe current.cols fromIntegral mx
                        , rows = maybe current.rows fromIntegral my }
                    nudged
                        | has "-L" = given { cols = shrink given.cols }
                        | has "-R" = given { cols = given.cols + fromIntegral adjust }
                        | has "-U" = given { rows = shrink given.rows }
                        | has "-D" = given { rows = given.rows + fromIntegral adjust }
                        | otherwise = given
                    shrink n
                        | toInteger n >= toInteger adjust = n - fromIntegral adjust
                        | otherwise = n
                let fromClients = atomically . clientWindowArea st sess win
                sz <- clampWindowSize <$> case () of
                    _ | has "-A" -> fromClients SizeLargest
                      | has "-a" -> fromClients SizeSmallest
                      | otherwise -> pure nudged
                atomically $ do
                    modifyOwn win.options
                        (insertDelta "window-size" (OVScalar "manual"))
                    writeTVar win.manualSize sz
                    recalculateSizes st
                    -- tmux's resize_window fires even at the size it had.
                    now <- readTVar win.size
                    when (now == current) $ windowResized st win current
                applySessionSize st sess.id
                pure []
  where
    (opts, flags, pos) = parseArgs "txy" args
    has f = f `elem` flags
    usage = "usage: resize-window [-aADLRU] [-x width] [-y height] "
        <> "[-t target-window] [adjustment]"
    parsed = do
        adjust <- case pos of
            [] -> Right 1
            t : _ -> either (Left . ("adjustment " <>)) Right
                (strtonum 1 maxBound t)
        mx <- traverse (either (Left . ("width " <>)) Right . strtonum 1 10000)
            (lookup "-x" opts)
        my <- traverse (either (Left . ("height " <>)) Right . strtonum 1 10000)
            (lookup "-y" opts)
        pure (mx, my, adjust :: Int)

-- | @kill-window@: destroy the window, unlinking it from every session
-- (see 'unlinkEverywhere'). Under @-a@ it instead destroys every other
-- window of the session that passes the @-f@ filter, and the target too
-- when it is linked into the session more than once.
cmdKillWindow :: CommandImpl
cmdKillWindow st mclient args
    | Just e <- unknownFlag "kill-window" "a" flags = pure [RErr e]
    | Just _ <- lookup "-f" opts, "-a" `notElem` flags =
        pure [RErr "-f only valid with -a"]
    | otherwise = do
        res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, win, _)
                | "-a" `elem` flags -> killOthers sess win
                | otherwise -> [] <$ killWindow win
  where
    (opts, flags, _) = parseArgs "ft" args
    killWindow win = do
        ps <- atomically (windowPanes win)
        loc <- atomically (listToMaybe . map ((.id) . fst) <$> windowLinks st win)
        forM_ loc $ \sid -> killPaneLocs st [(sid, win, p) | p <- ps]
    passes sess ix win = case lookup "-f" opts of
        Nothing -> pure True
        Just f -> do
            env <- windowFormatEnv st sess ix win
            truthy <$> expandFormat st env f
    killOthers sess win = do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        unless (length ws <= 1) $ do
            doomed <- filterM (\(ix, w) -> passes sess ix w)
                [(ix, w) | (ix, w) <- ws, w.id /= win.id]
            forM_ (List.nubBy (\a b -> a.id == b.id) (map snd doomed)) killWindow
            ws' <- Map.toAscList <$> readTVarIO sess.windows
            let selves = [(ix, w) | (ix, w) <- ws', w.id == win.id]
            matched <- or <$> mapM (uncurry (passes sess)) selves
            when (matched && length selves > 1) (killWindow win)
            atomically $
                mapM_ (renumberIfSet st) . Map.elems =<< readTVar st.sessions
        pure []

cmdRenameWindow :: CommandImpl
cmdRenameWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> do
            res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
            case res of
                Right (sess, wix, win, pane) -> do
                    pix <- paneIndexOf st win pane
                    env <- paneFormatEnv st sess wix win pix pane
                    expanded <- expandFormat st env nm
                    case admitName WindowName (Utf8Arg expanded) of
                      Left r -> pure [r]
                      Right name -> do
                        atomically $ do
                            writeTVar win.name name
                            writeTVar win.autoRename False
                            bumpDirty st
                        notifyWindow st "window-renamed" (Just sess.id) win []
                        pure []
                Left e -> pure [RErr e]
        _ -> pure [RErr "usage: rename-window [-t target] name"]

-- | @move-window@: link the source window at the destination, then drop
-- its source link. Restore replays this to place windows at their saved
-- indices. Under @-r@ it instead repacks the whole session named by @-t@.
cmdMoveWindow :: CommandImpl
cmdMoveWindow st mclient args
    | Just e <- unknownFlag "move-window" "abdkr" flags = pure [RErr e]
    | "-r" `elem` flags =
        withTargetSession st mclient (lookup "-t" opts) (([] <$) . renumber)
    | otherwise = linkWindow DropSource st mclient opts flags
  where
    (opts, flags, _) = parseArgs "st" args
    renumber sess = atomically (repackSession st sess)

-- | @link-window@: link the source window at the destination too, the
-- same window shared by both sessions.
cmdLinkWindow :: CommandImpl
cmdLinkWindow st mclient args
    | Just e <- unknownFlag "link-window" "abdk" flags = pure [RErr e]
    | otherwise = linkWindow KeepSource st mclient opts flags
  where
    (opts, flags, _) = parseArgs "st" args

-- | Whether 'linkWindow' drops the source link afterwards (@move-window@)
-- or keeps it (@link-window@).
data SourceLink = DropSource | KeepSource
    deriving (Eq, Show)

-- | What a 'linkWindow' transaction changed, for its announcements: the
-- destination's window switch, the sessions emptied by dropping the
-- source link, and a displaced occupant left with no link at all.
data Relinked = Relinked
    { switched :: Maybe (Int, Maybe Window, Int, Window)
    , dropped  :: [Session]
    , orphan   :: Maybe Window
    }

-- | The body shared by @move-window@ and @link-window@ (tmux's
-- @server_link_window@): @-a@\/@-b@ shuffle the destination's windows up
-- to make room after\/before the target window (else the current one),
-- @-k@ displaces an occupant, and the linked window becomes current
-- unless @-d@ (or unless it displaced the current window).
linkWindow
    :: SourceLink -> ServerState -> Maybe Client -> [(Text, Text)] -> [Text]
    -> IO [Reply]
linkWindow source st mclient opts flags = do
    esrc <- findTarget st mclient Target.FindWindow (lookup "-s" opts)
    edst <- findWindowIndexTarget st mclient (lookup "-t" opts)
    case (esrc, edst) of
        (Left e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]
        (Right (src, srcIx, win, _), Right (dst, mdstIx)) -> do
            res <- atomically $ do
                dws0 <- readTVar dst.windows
                dcur <- readTVar dst.currentIx
                sopts <- resolveForSession st dst
                let anchor = case mdstIx of
                        Just i | Map.member i dws0 -> i
                        _ -> dcur
                    shuffleAt
                        | "-b" `elem` flags = Just anchor
                        | "-a" `elem` flags = Just (anchor + 1)
                        | otherwise = Nothing
                    shifts = maybe [] (`shuffleUp` dws0) shuffleAt
                    dws = applyShifts shifts dws0
                    dstIx = fromMaybe (nextFreeWindowIndex sopts.baseIndex dws)
                        (maybe mdstIx Just shuffleAt)
                    occupant = Map.lookup dstIx dws
                case occupant of
                    _ | src.id /= dst.id && src.windows == dst.windows ->
                        pure (Left "sessions are grouped")
                    Just o | o.id == win.id ->
                        pure (Left ("same index: " <> tshow dstIx))
                    Just _ | "-k" `notElem` flags ->
                        pure (Left ("index in use: " <> tshow dstIx))
                    _ -> Right <$> relink src srcIx win dst dstIx shifts occupant
            case res of
                Left e -> pure [RErr e]
                Right r -> [] <$ announce src dst win r
  where
    relink src srcIx0 win dst dstIx shifts occupant = do
        let remap i = fromMaybe i (List.lookup i shifts)
            srcIx = if src.windows == dst.windows then remap srcIx0 else srcIx0
        modifyTVar' dst.windows (applyShifts shifts)
        modifyTVar' dst.currentIx remap
        modifyTVar' dst.windowHist (map remap)
        forced <- case occupant of
            Nothing -> pure False
            Just _ -> do
                modifyTVar' dst.windows (Map.delete dstIx)
                modifyTVar' dst.windowHist (filter (/= dstIx))
                (== dstIx) <$> readTVar dst.currentIx
        _ <- attachWindow st dst dstIx win
        sw <- if "-d" `notElem` flags || forced
            then switchToInfo st dst dstIx
            else pure Nothing
        gone <- case source of
            KeepSource -> pure []
            DropSource -> do
                emptied <- detachWindow st src srcIx
                if emptied then dropSessionGroup st src else do
                    -- tmux renumbers the source only when @-s@ did not
                    -- name it.
                    when (isNothing (lookup "-s" opts)) (renumberIfSet st src)
                    pure []
        rehomePanes dst.id win  -- See Note [Pane home]
        orph <- case occupant of
            Nothing -> pure Nothing
            Just o -> windowLinks st o >>= \case
                [] -> pure (Just o)
                (holder, _) : _ -> Nothing <$ rehomePanes holder.id o
        bumpDirty st
        pure Relinked { switched = sw, dropped = gone, orphan = orph }
    announce src dst win r = do
        forM_ r.orphan $ \o -> do
            ps <- atomically (windowPanes o)
            killPaneLocsWith QuietLifecycle st [(dst.id, o, p) | p <- ps]
        forM_ r.switched (announceSwitch st dst)
        dname <- readTVarIO dst.name
        notifyWindow st "window-linked" (Just dst.id) win
            [ ("session", PSessionRef dst.id dname) ]
        when (source == DropSource) $ do
            sname <- readTVarIO src.name
            wname <- readTVarIO win.name
            notify st "window-unlinked" (sessionTarget src.id)
                [ ("session", PSessionRef src.id sname)
                , ("window", PWindowRef win.id wname) ]
        closeSessions st r.dropped
        applySessionSize st src.id
        applySessionSize st dst.id

-- | Announce sessions a command destroyed by emptying them, and send
-- their clients away.
closeSessions :: ServerState -> [Session] -> IO ()
closeSessions st gone = do
    info <- forM gone $ \s -> do
        sname <- readTVarIO s.name
        notify st "session-closed" noTarget [ ("session", PSessionRef s.id sname) ]
        (,,) s.id sname <$> readTVarIO s.group
    notifyGroupDrops st info
    sendClientsAway st gone

-- | @unlink-window [-k]@: drop one link of the window. Its last link
-- outside the session group goes only under @-k@, destroying the window.
cmdUnlinkWindow :: CommandImpl
cmdUnlinkWindow st mclient args
    | Just e <- unknownFlag "unlink-window" "k" flags = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, _) -> do
                out <- atomically $ do
                    links <- windowLinks st win
                    members <- groupMembers st sess
                    if length links == length members && "-k" `notElem` flags
                        then pure (Left "window only linked to one session")
                        else do
                            emptied <- detachWindow st sess wix
                            gone <- if emptied
                                then dropSessionGroup st sess else pure []
                            left <- windowLinks st win
                            forM_ (take 1 left) $ \(holder, _) ->
                                rehomePanes holder.id win  -- See Note [Pane home]
                            pure (Right (gone, null left))
                case out of
                    Left e -> pure [RErr e]
                    Right (gone, orphaned) -> do
                        sname <- readTVarIO sess.name
                        wname <- readTVarIO win.name
                        notify st "window-unlinked" (sessionTarget sess.id)
                            [ ("session", PSessionRef sess.id sname)
                            , ("window", PWindowRef win.id wname) ]
                        when orphaned $ do
                            ps <- atomically (windowPanes win)
                            killPaneLocsWith QuietLifecycle st
                                [(sess.id, win, p) | p <- ps]
                        closeSessions st gone
                        applySessionSize st sess.id
                        pure []
  where
    (opts, flags, _) = parseArgs "t" args

-- | @swap-window@: exchange two links' windows. Under @-d@ the
-- destination (and a distinct source session) select the swapped index.
cmdSwapWindow :: CommandImpl
cmdSwapWindow st mclient args
    | Just e <- unknownFlag "swap-window" "d" flags = pure [RErr e]
    | otherwise = do
        esrc <- findTarget st mclient Target.FindWindow (lookup "-s" opts)
        edst <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
        case (esrc, edst) of
            (Left e, _) -> pure [RErr e]
            (_, Left e) -> pure [RErr e]
            (Right (src, srcIx, wsrc, _), Right (dst, dstIx, wdst, _))
                | src.id /= dst.id && src.windows == dst.windows ->
                    pure [RErr "can't move window, sessions are grouped"]
                | wsrc.id == wdst.id -> pure []
                | otherwise -> do
                    sws <- atomically $ do
                        modifyTVar' src.windows (Map.insert srcIx wdst)
                        modifyTVar' dst.windows (Map.insert dstIx wsrc)
                        rehomePanes dst.id wsrc  -- See Note [Pane home]
                        rehomePanes src.id wdst
                        bumpDirty st
                        if "-d" `elem` flags
                            then do
                                d <- switchToInfo st dst dstIx
                                s <- if src.id /= dst.id
                                    then switchToInfo st src srcIx
                                    else pure Nothing
                                pure [(dst, d), (src, s)]
                            else pure []
                    forM_ sws $ \(s, msw) -> forM_ msw (announceSwitch st s)
                    applySessionSize st src.id
                    applySessionSize st dst.id
                    pure []
  where
    (opts, flags, _) = parseArgs "st" args

-- | @rotate-window -U|-D@: turn the panes through the layout's positions
-- ('rotateLeaves'); the pane arriving at the active position becomes
-- active. A zoomed window unzooms, unless @-Z@ keeps the new active pane
-- zoomed.
cmdRotateWindow :: CommandImpl
cmdRotateWindow st mclient args
    | Just e <- unknownFlag "rotate-window" "DUZ" flags = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, win, _) -> do
                atomically $ do
                    lay <- readTVar win.layout
                    active <- readTVar win.activeId
                    zoomed <- readTVar win.zoomed
                    let dir = if "-D" `elem` flags then RotateDown else RotateUp
                        lay' = rotateLeaves dir lay
                        slot = List.elemIndex active (layoutPanes lay)
                        active' = maybe active (layoutPanes lay' !!) slot
                    writeTVar win.layout lay'
                    when (active' /= active) $ do
                        modifyTVar' win.paneHist (recordVisit active active')
                        writeTVar win.activeId active'
                    writeTVar win.zoomed $ case zoomed of
                        Just _ | "-Z" `elem` flags -> Just active'
                        _ -> Nothing
                    bumpDirty st
                applySessionSize st sess.id
                pure []
  where
    (opts, flags, _) = parseArgs "t" args
