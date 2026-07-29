-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients, streams frame diffs at them, and runs the command engine
-- that configs, bindings, and @hat <command>@ all share.
module Hat.Server
    ( runServer
    , resumeServer  -- ^ the reload re-exec re-enters here with a handover file
    , captureReloadScreen  -- ^ exported for the reload-screen round-trip test
    , replayPane           -- ^ exported for the reload-screen round-trip test
    , captureSize          -- ^ exported for the oversized-capture adopt test
    , cmdRestartServer     -- ^ exported for the reload-in-progress guard test
    , Reply (..)           -- ^ exported for the reload-in-progress guard test
    , setOption  -- ^ exported for the config-load burn-down test
    , SetMode (..)  -- ^ exported for the config-load burn-down test
    , chooseScope  -- ^ exported for the scope-routing test
    , SetScope (..)
    , SetDefault (..)
    , finallyClearRestoring  -- ^ exported for the restore-gate test
    , readConfigUtf8  -- ^ exported for the config-encoding test
    , cmdAttachSession  -- ^ exported for the session re-anchor test
    , cmdSourceFile  -- ^ exported for the reload tilde-expansion test
    , PaneStart (..)  -- ^ exported for the restore-argv test
    , SpawnOrigin (..)  -- ^ exported for the restore-argv test
    , restoreRun      -- ^ exported for the restore-argv test
    , restoreShellExec  -- ^ exported for the shell-relaunch test
    , DirenvAvailable (..)  -- ^ exported for the shell-relaunch test
    , defaultRestoreCommands  -- ^ exported for the restore-argv test
    , chooseCurrentOnClose  -- ^ exported for the close-to-last-window test
    , chooseActivePaneOnClose  -- ^ exported for the close-to-last-pane test
    , pickActivityTarget  -- ^ exported for the activity-jump test
    , pickAttachSession  -- ^ exported for the attach-to-last-active test
    , persistDecision  -- ^ exported for the store-pinning test
    , PersistDecision (..)
    , StorePin (..)
    , windowFlags  -- ^ exported for the window-flags test
    , WindowFlagState (..)
    , defaultKeymap  -- ^ exported for the copy-mode binding test
    , applySessionSize  -- ^ exported for the aggressive-resize test
    , awaitReconciled  -- ^ exported for the reconcile-barrier test
    , detachPane  -- ^ exported for the pane-detach test
    , detachPanes  -- ^ exported for the multi-pane-detach test
    , detachPaneCurrent  -- ^ exported for the mobile-pane teardown test
    , removePaneFromTree  -- ^ exported for the mobile-pane teardown test
    , DetachResult (..)
    , SessionFate (..)
    , serverIdle  -- ^ exported for the idle-predicate test
    , IdleInputs (..)
    , markBell  -- ^ exported for the current-window bell test
    , markActivity  -- ^ exported for the outer-focus activity test
    , noteOuterFocus  -- ^ exported for the focus-in-clears test
    , attentionSeen  -- ^ exported for the outer-focus gating test
    , deliversKey  -- ^ exported for the focus-event gating test
    , mainPaneRatio  -- ^ exported for the main-pane-size test
    , resizeModeOf  -- ^ exported for the aggressive-resize effect test
    , applyUpdateEnvironment  -- ^ exported for the update-environment effect test
    , nextZoom  -- ^ exported for the zoom-alternate-pane test
    ) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (link, race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, catch, displayException, finally,
     handle, throwIO, try)
import Database.SQLite.Simple (SQLError)
import Control.Monad (filterM, foldM, forM, forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Network.Socket as N
import System.Directory
    (createDirectoryIfMissing, doesFileExist, findExecutable, removeFile,
     renameFile)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hFlush)
import qualified System.Posix.IO as PIO
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (sigHUP, signalProcess)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)
import System.Process
    (CreateProcess (..), StdStream (..), createProcess, proc,
     readCreateProcess, readCreateProcessWithExitCode, shell,
     terminateProcess, waitForProcess, withCreateProcess)

import Hat.Command.Parser (parseCommandLine, parseConfig)
import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Path (expandTilde, hatPath, render, (</:>))
import Hat.Server.Persist
    (PaneSnap (..), SessionSnap (..), Snapshot (..), WindowSnap (..)
    , loadSnapshot, saveSnapshot, withStore)
import Hat.Server.Reload
    (Handover (..), ReloadCleanup (..), ReloadModes (..), ReloadPane (..)
    , ReloadScreen (..), ReloadSession (..), ReloadState (..), ReloadWindow (..)
    , decodeHandover, encodeHandover)
import qualified Hat.Term.Pty
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.ClientIO (broadcast, send)
import Hat.Server.ColorScheme
    ( ColorScheme (..), WatcherFault (..), applyPalette, parseSchemeLine
    , watcherFault )
import Hat.Server.Format (FormatEnv)
import Hat.Server.Keys
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout, layoutFromString)
import qualified Hat.Server.Picker as Picker
import qualified Hat.Server.Prompt as Prompt
import Hat.Server.Render
import Hat.Server.Style (parseStyle)
import Hat.Server.Target (PaneTarget (..), parsePaneTarget)
import Hat.Server.Title (TitleParts (..), composeTitle)
import Hat.Server.View
    (WindowFlagState (..), expandFormat, renderLoop, sessionFormatEnv,
     windowArrange, windowFlags)
import Hat.Transport.Socket (ensureSocketDir, listenOn)
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Transport.Wire

runServer :: FilePath -> Maybe FilePath -> IO ()
runServer path mconfig = runServerWith path mconfig Nothing

-- | Re-enter the server after a self-exec reload, reading the handover file
-- an outgoing image left behind ('cmdRestartServer'): adopt the inherited
-- listening socket and pane ptys instead of binding and spawning afresh.
resumeServer :: FilePath -> Maybe FilePath -> FilePath -> IO ()
resumeServer path mconfig handover = runServerWith path mconfig (Just handover)

runServerWith :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO ()
runServerWith path mconfig mhandover = do
    -- The lock and log files live next to the socket; the directory
    -- must exist before any of them are touched.
    ensureSocketDir path
    -- A reload continues the same process (same PID via execve), which
    -- already holds the lock through the inherited fd, so re-acquiring it
    -- here would deadlock against ourselves.
    case mhandover of
        Just _ -> pure ()
        Nothing -> do
            lockResult <- acquireLock (path <> ".lock")
            case lockResult of
                LockHeldElsewhere -> exitSuccess  -- another server won the race
                LockWon -> pure ()
    withLogger (render (hatPath (takeDirectory path) </:> "server.log")) serve
  where
   serve lg = do
    persistOn <- persistEnabled
    mstore <- if persistOn then Just <$> storePathFor path else pure Nothing
    st <- newServerState defaultKeymap lg path mstore
    -- On a reload, read the handover the outgoing image left. An era match
    -- yields the tree to adopt (and the socket fd to reuse); an incompatible
    -- or corrupt payload yields only the cleanup core, so we hang the
    -- inherited processes up and start fresh rather than orphan them.
    mhand <- case mhandover of
        Just hp -> readReload lg hp
        Nothing -> pure Nothing
    mreload <- case mhand of
        Just h | Right rs <- h.tree -> pure (Just (h.cleanup.listenFd, rs))
        Just h -> do
            let reason = either id (const "handover unusable") h.tree
            logEvent lg ServerCrash
                { err = "reload: " <> reason <> "; hanging up "
                    <> tshow (length h.cleanup.live) <> " inherited pane(s)"
                    <> " and starting fresh" }
            cleanupInherited h.cleanup
            pure Nothing
        Nothing -> pure Nothing
    let openListen = case mreload of
            Just (sockFd, _) -> N.mkSocket (fromIntegral sockFd)
            Nothing -> listenOn path
    bracket openListen N.close $ \lsock -> do
        lfd <- N.unsafeFdSocket lsock
        atomically $ do
            writeTVar st.listenFd (Just (fromIntegral lfd))
            writeTVar st.serverConfig mconfig
        logEvent lg ServerStarted { socket = path }
        -- Load the config in a background thread so shell conditions
        -- like `if '$TMUX run ...' ...` can reach the accept loop while
        -- the config is still running. configLoading suppresses the
        -- empty-idle exit until the config has drained.
        atomically $ do
            writeTVar st.configLoading True
            -- Armed before the accept loop can serve, so a client that
            -- autostarts us and attaches waits for the restore to finish
            -- (see 'ensureSession') and joins the restored tree. A reload
            -- rebuilds the tree too, so the same gate applies.
            when (persistOn || isJust mreload) (writeTVar st.restoring True)
        _ <- forkIO $ do
            finallyClearRestoring st $
                (loadConfig st mconfig >> case mreload of
                    -- Reload: re-adopt the still-running tree the outgoing
                    -- image handed over, rather than respawning from disk.
                    Just (_, rs) -> rebuildReload st rs
                    Nothing -> forM_ mstore (restoreSaved st))
                    `catch` \(e :: SomeException) ->
                        logEvent lg ServerCrash
                            { err = "startup restore failed: " <> T.pack (show e) }
            -- No fixed grace: the idle-exit now waits on 'served' (a real
            -- connection), so the autostarting client is always counted
            -- before we can drain, whatever the config-load timing.
            atomically (writeTVar st.configLoading False)
        titlesRef <- newIORef Map.empty
        -- Background daemons run under 'withDaemons': each is bracketed by
        -- 'withAsync' (all torn down when the serve loop returns — no leaked
        -- threads, and the persist mirror stops before the store is dropped)
        -- and 'link'ed, so an unexpected fault re-raises here rather than
        -- vanishing. Each daemon catches its own expected failures, so a link
        -- fires only on a genuine bug.
        let clockDaemon = forever $ do            -- keep status-line clocks fresh
                threadDelay 15_000_000
                atomically (bumpDirty st)
            -- Track foreground commands for automatic-rename windows and the
            -- clients' desktop titles. A dead pane's /proc read is the expected
            -- failure here (logged, skipped) — anything else is a real bug.
            titleDaemon = forever $ do
                threadDelay 500_000
                (refreshAutoNames st >> refreshTitles st titlesRef)
                    `catch` \(e :: IOException) ->
                        logEvent lg DaemonFault
                            { daemon = "auto-rename", err = T.pack (show e) }
            daemons =
                [ clockDaemon
                , reconcileLoop st                -- pane sizes track the layout
                , titleDaemon
                , watchColorScheme st             -- follow the desktop theme
                ] <> [ persistLoop st p | p <- maybe [] pure mstore ]
        -- Last-resort trace: an exception escaping the accept loop or a linked
        -- daemon takes the process down (e.g. a reload's resume fault), and
        -- otherwise vanishes to stderr with nothing in the log. Record its
        -- 'displayException' (backtrace included) and flush before it dies,
        -- then re-raise so the process still exits.
        r <- withDaemons daemons (race (acceptLoop st lsock) (waitIdle st))
                `catch` \(e :: SomeException) -> do
                    logEvent lg ServerFatal { err = T.pack (displayException e) }
                    flushLogger lg
                    throwIO e
        case r of
            Left () -> pure ()
            Right () -> do
                logEvent lg ServerStopping { reason = "no sessions left" }
                -- The daemons (the persist mirror included) are already torn
                -- down, so no in-flight write can recreate the store after we
                -- drop it. The tree drained (every window closed), so the next
                -- start must be pristine — unless kill-server asked to keep the
                -- tree for a restore.
                preserve <- readTVarIO st.preserveStore
                unless preserve $ forM_ mstore $ \p ->
                    removeFile p `catch` \(_ :: IOException) -> pure ()
                removeFile path `catch` \(_ :: IOException) -> pure ()

-- | Run @body@ with each daemon alive under 'withAsync', so all are cancelled
-- when @body@ returns or throws (the bracket pattern: no leaked threads, and
-- teardown is ordered — every daemon stops before whatever runs after the
-- scope). Each is 'link'ed, so a daemon dying of an unexpected fault is
-- re-raised in this thread instead of vanishing silently; daemons catch their
-- own expected failures, so a link only ever fires on a genuine bug.
withDaemons :: [IO ()] -> IO a -> IO a
withDaemons ds body = foldr spawn body ds
  where
    spawn d k = withAsync d $ \a -> link a >> k

-- | Run a startup action (config load + restore), then clear the
-- @restoring@ gate — always, even if it throws. A gate left set parks
-- every attach forever on 'ensureSession'\'s retry, so the clear must be
-- structural (a @finally@), never a line a crash can skip.
finallyClearRestoring :: ServerState -> IO a -> IO a
finallyClearRestoring st act =
    act `finally` atomically (writeTVar st.restoring False)

-- | The outcome of racing for the server's lock file.
data LockResult
    = LockWon            -- ^ this process now holds the lock
    | LockHeldElsewhere  -- ^ another server already holds it

-- flock-style: O_CREAT + posix write lock, held for the server's life.
acquireLock :: FilePath -> IO LockResult
acquireLock lockPath = do
    fd <- PIO.createFile lockPath 0o600
    r <- try $ PIO.setLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
    pure $ case r of
        Left (_ :: IOException) -> LockHeldElsewhere
        Right () -> LockWon

-- | The reasons a server stays alive, gathered for 'serverIdle'. See it.
data IdleInputs = IdleInputs
    { idleAttached :: Bool
    , idleServed   :: Bool
    , idleLoading  :: Bool
    , idleSessions :: Int
    , idleClients  :: Int
    , idlePanes    :: Int
    }

-- | Whether the server may exit: it has served at least one client, is done
-- loading config, and has no sessions, clients, or live panes left. The
-- live-pane term is what keeps a drained server alive until every child it
-- spawned has been reaped — since 'cmdKillPane' detaches a pane from the
-- model before its child is reaped, an empty session map no longer implies
-- the children are gone, and exiting first would orphan a SIGHUP-ignoring
-- child mid-'reapPane'. See 'waitIdle'.
serverIdle :: IdleInputs -> Bool
serverIdle i =
    i.idleAttached && i.idleServed && not i.idleLoading
        && i.idleSessions == 0 && i.idleClients == 0 && i.idlePanes == 0

-- Exit once every session is gone, every attached client has drained and
-- disconnected (so nobody's final Exited message is cut off), and every
-- pane's child has been reaped (so a drain never orphans one). See 'serverIdle'.
waitIdle :: ServerState -> IO ()
waitIdle st = atomically $ do
    inputs <- IdleInputs
        <$> readTVar st.everAttached
        <*> readTVar st.served
        <*> readTVar st.configLoading
        <*> (Map.size <$> readTVar st.sessions)
        <*> (Map.size <$> readTVar st.clients)
        <*> readTVar st.livePanes
    check (serverIdle inputs)

-- Persistence ----------------------------------------------------------

-- | Whether to persist the session tree. On by default; @HAT_PERSIST=0@
-- turns it off (tests set this so each server starts from a clean slate).
persistEnabled :: IO Bool
persistEnabled = (/= Just "0") <$> lookupEnv "HAT_PERSIST"

-- | The SQLite store for a socket: @$HAT_STORE_DIR/<socket>.db@ when that
-- override is set, else @$XDG_DATA_HOME/hat/<socket>.db@ falling back to
-- @~/.local/share@. It lives in a reboot-surviving location (not beside
-- the socket under @/tmp@) and is keyed per socket, so @-L foo@ and
-- @-L bar@ never clobber each other. The directory is created if absent.
storePathFor :: FilePath -> IO FilePath
storePathFor sockPath = do
    dir <- storeDir
    createDirectoryIfMissing True (render dir)
    pure (render (dir </:> (takeFileName sockPath <> ".db")))
  where
    storeDir = lookupEnv "HAT_STORE_DIR" >>= \case
        Just d | not (null d) -> pure (hatPath d)
        _ -> do
            base <- lookupEnv "XDG_DATA_HOME" >>= \case
                Just d | not (null d) -> pure (hatPath d)
                _ -> do
                    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
                    pure (hatPath home </:> ".local" </:> "share")
            pure (base </:> "hat")

-- | Whether the store already holds an explicitly saved final tree.
-- See 'persistDecision'.
data StorePin = Pinned | Unpinned
    deriving (Eq, Show)

-- | The mirror's per-tick verdict on the freshly captured snapshot.
data PersistDecision
    = PinnedSkip     -- ^ store pinned; the last tree is final, never overwrite
    | EmptySkip      -- ^ an empty tree is never mirrored
    | UnchangedSkip  -- ^ identical to the last write, nothing to do
    | WriteSnapshot  -- ^ changed, non-empty, and unpinned: write it
    deriving (Eq, Show)

-- | Decide whether the mirror should write a captured snapshot. A pin
-- (set by @kill-server@) wins over everything: the explicit quit already
-- saved the final tree, so a stray fresh session on the dying server can
-- never clobber it. Otherwise an empty tree is skipped (shutdown, not the
-- mirror, decides an empty store's fate) as is a snapshot unchanged since
-- the last write.
persistDecision :: StorePin -> Maybe Snapshot -> Snapshot -> PersistDecision
persistDecision Pinned _ _ = PinnedSkip
persistDecision Unpinned prev snap
    | null snap.sessions = EmptySkip
    | prev == Just snap  = UnchangedSkip
    | otherwise          = WriteSnapshot

-- | Poll the live tree and write a fresh snapshot whenever it changes.
-- The tree is tiny, so we rewrite it wholesale rather than diffing, and
-- skip writes when nothing changed. A change to a pane's working
-- directory (a bare @cd@, which fires no event) is caught here too. Once
-- the store is pinned by @kill-server@ the loop stops writing for good
-- (see 'persistDecision'), so a fresh session on the dying server cannot
-- overwrite the saved tree.
persistLoop :: ServerState -> FilePath -> IO ()
persistLoop st path = go Nothing
  where
    go prev = do
        threadDelay 2_000_000
        snap <- captureSnapshot st
        pinned <- readTVarIO st.preserveStore
        let pin = if pinned then Pinned else Unpinned
        next <- case persistDecision pin prev snap of
            WriteSnapshot -> saveSnapshotNow path snap >> pure (Just snap)
            _             -> pure prev
        go next

-- | Capture and persist immediately. Called at 'cmdKillServer' so an
-- explicit quit never loses a last-moment change. This is the pinning
-- write: it runs after 'preserveStore' is set, directly rather than via
-- 'persistLoop', so the pin never suppresses it. A no-op when persistence
-- is off. An empty tree is never written here: whether an empty store
-- survives shutdown is decided by 'preserveStore' (kill-server keeps the
-- tree; a natural drain deletes the store, see 'runServer').
saveNow :: ServerState -> IO ()
saveNow st = forM_ st.store $ \path -> do
    snap <- captureSnapshot st
    unless (null snap.sessions) (saveSnapshotNow path snap)

-- Best-effort write; persistence must never take down the server, so a store
-- failure (a SQLite error, a lost lock race, a filesystem error) is swallowed
-- rather than raised. Only these synchronous failures are caught: an async
-- exception (the persist daemon being cancelled at shutdown) must pass through,
-- or 'cancel' would hang waiting on a loop that ate its own cancellation.
saveSnapshotNow :: FilePath -> Snapshot -> IO ()
saveSnapshotNow path snap =
    (withStore path $ \conn -> saveSnapshot conn snap)
        `catch` \(_ :: SQLError) -> pure ()
        `catch` \(_ :: IOException) -> pure ()

-- | Read the whole session tree into a pure 'Snapshot': sessions in id
-- order, windows by index, panes in layout order with their live cwd.
captureSnapshot :: ServerState -> IO Snapshot
captureSnapshot st = do
    (sess, laName) <- atomically $ do
        sessMap <- readTVar st.sessions
        laId    <- readTVar st.lastActiveSession
        laName  <- traverse (readTVar . (.name)) (laId >>= (`Map.lookup` sessMap))
        pure (Map.elems sessMap, laName)
    Snapshot <$> mapM captureSession sess <*> pure laName

-- | One window's structure read as a single consistent unit. Its layout,
-- active\/last-active ordinals and live panes are read together in one STM
-- transaction, so a concurrent split or close cannot leave the saved layout
-- referring to panes the snapshot dropped. The per-pane cwd and argv are
-- gathered afterwards in IO ('captureWindow').
data WindowStruct = WindowStruct
    { wsIx         :: Int
    , wsName       :: Text
    , wsLayout     :: Text
    , wsActive     :: Int
    , wsLastActive :: Maybe Int
    , wsAutoRename :: Bool
    , wsPanes      :: [Pane]
    }

captureSession :: Session -> IO SessionSnap
captureSession s = do
    (nm, cwd, curIx, lastI, wstructs) <- atomically $ do
        nm    <- readTVar s.name
        cwd   <- readTVar s.startCwd
        curIx <- readTVar s.currentIx
        lastI <- readTVar s.lastIx
        eff   <- readTVar s.lastSize
        ws    <- Map.toAscList <$> readTVar s.windows
        wstructs <- mapM (windowStruct eff) ws
        pure (nm, cwd, curIx, lastI, wstructs)
    wsnaps <- mapM captureWindow wstructs
    pure SessionSnap
        { name = nm, startCwd = T.pack cwd
        , currentIx = curIx, lastIx = lastI, windows = wsnaps }

windowStruct :: Size -> (Int, Window) -> STM WindowStruct
windowStruct eff (wix, w) = do
    nm       <- readTVar w.name
    lay      <- readTVar w.layout
    activeId <- readTVar w.activeId
    lastAId  <- readTVar w.lastActive
    auto     <- readTVar w.autoRename
    paneMap  <- readTVar w.panes
    let order = layoutPanes lay
        activeOrd = fromMaybe 0 (List.elemIndex activeId order)
        lastOrd = lastAId >>= \pid -> List.elemIndex pid order
    pure WindowStruct
        { wsIx = wix, wsName = nm
        , wsLayout = emitLayout (sizeRect (windowArea eff)) lay
        , wsActive = activeOrd, wsLastActive = lastOrd
        , wsAutoRename = auto
        , wsPanes = mapMaybe (`Map.lookup` paneMap) order }

captureWindow :: WindowStruct -> IO WindowSnap
captureWindow ws = do
    psnaps <- forM ws.wsPanes $ \pane -> do
        dir  <- paneCurrentPath pane
        -- The whole argv, so a restore re-opens the same file; the
        -- whitelist (see 'restoreRun') decides whether it is re-run.
        argv <- Hat.Term.Pty.foregroundArgv pane.pty
        -- Whether that program was a child of the pane's interactive shell,
        -- so a restore can relaunch it through the shell (see 'restoreRun').
        shellSp <- Hat.Term.Pty.foregroundIsChild pane.pty
        pure PaneSnap { cwd = T.pack dir, command = argv, shellSpawned = shellSp }
    pure WindowSnap
        { ix = ws.wsIx, name = ws.wsName, layout = ws.wsLayout
        , active = ws.wsActive, lastActive = ws.wsLastActive
        , autoRename = ws.wsAutoRename, panes = psnaps }

-- Color scheme -----------------------------------------------------------

-- | Follow the desktop's light\/dark preference: read it once, then tail
-- @gsettings monitor@ for changes. Runs after the config has loaded so
-- the @\@color-scheme-*@ options are set before the initial apply. The
-- monitor is a durable daemon: if its subprocess dies (a crash, EOF on its
-- pipe) or the @gsettings@ binary is absent (a host without a desktop
-- session), the watcher is respawned after a capped backoff so theme-
-- following resumes on its own — a transient death never silently ends the
-- feature for the server's life. Only synchronous faults restart; an async
-- cancellation (the shutdown 'cancel' 'withDaemons' issues) or an 'ExitCode'
-- is re-raised, never swallowed and never restarted (see 'watcherFault'), so
-- teardown stays clean. Killed at server shutdown, which also terminates the
-- monitor subprocess (withCreateProcess's cleanup).
watchColorScheme :: ServerState -> IO ()
watchColorScheme st = do
    atomically (readTVar st.configLoading >>= check . not)
    loop minBackoff
  where
    loop backoff = do
        r <- try watch
        case r of
            Right () -> pure ()  -- the monitor returned; nothing left to tail
            Left e -> case watcherFault e of
                PropagateFault -> throwIO e
                AbandonWatcher ->
                    -- gsettings is not installed; theme-following can never
                    -- start here, so give up quietly-but-logged instead of an
                    -- endless respawn.
                    logEvent st.logger DaemonStopped
                        { daemon = "color-scheme"
                        , reason = "gsettings not found; desktop theme-following disabled" }
                RestartWatcher -> do
                    logEvent st.logger DaemonFault
                        { daemon = "color-scheme", err = T.pack (show e) }
                    threadDelay backoff
                    loop (min maxBackoff (backoff * 2))
    watch = do
        out <- readCreateProcess
            (proc "gsettings" ["get", schemaKey, key]) { close_fds = True } ""
        forM_ (parseSchemeLine (T.strip (T.pack out))) (applyScheme st)
        withCreateProcess
            (proc "gsettings" ["monitor", schemaKey, key])
                { std_out = CreatePipe, close_fds = True } $ \_ mout _ _ ->
            forM_ mout $ \h -> forever $ do
                line <- TIO.hGetLine h
                forM_ (parseSchemeLine line) (applyScheme st)
    minBackoff = 1_000_000     -- 1s after the first death
    maxBackoff = 60_000_000    -- capped at 60s so an absent gsettings idles
    schemaKey = "org.gnome.desktop.interface"
    key = "color-scheme"

-- | Record a (possibly unchanged) scheme; on a change, source the
-- config file the user pointed at it (@set -g \@color-scheme-dark
-- \<file\>@, likewise @-light@) and redraw.
applyScheme :: ServerState -> ColorScheme -> IO ()
applyScheme st scheme = do
    old <- atomically $ swapTVar st.colorScheme (Just scheme)
    unless (old == Just scheme) $ do
        -- The scheme is a base layer under every user scope, so a user's
        -- own set always wins; then the user's per-scheme config on top.
        atomically $ do
            writeTVar st.schemeOptions (applyPalette scheme)
            refreshGlobalOptions st
        opts <- readTVarIO st.options
        let optName = case scheme of
                SchemeDark -> "@color-scheme-dark"
                SchemeLight -> "@color-scheme-light"
        forM_ (Map.lookup optName opts.user) $ \path ->
            unless (T.null (T.strip path)) $
                void $ runArgv st Nothing ["source-file", T.strip path]
        notifySubscribedPanes st scheme
        atomically (bumpDirty st)

-- | Tell every pane whose app subscribed to color-scheme reports (DEC mode
-- 2031) that the OS scheme is now @scheme@, by writing the @CSI ? 997@ report
-- into its pty. hat is the app's terminal, so it answers 2031 the way a
-- terminal would, sourced from the scheme it already watches via gsettings.
notifySubscribedPanes :: ServerState -> ColorScheme -> IO ()
notifySubscribedPanes st scheme = do
    sessMap <- readTVarIO st.sessions
    forM_ (Map.elems sessMap) $ \sess -> do
        winMap <- readTVarIO sess.windows
        forM_ (Map.elems winMap) $ \win -> do
            paneMap <- readTVarIO win.panes
            forM_ (Map.elems paneMap) $ \pane -> do
                subscribed <- (.colorReport) <$> Emu.modes pane.emulator
                when subscribed (notifyColorScheme pane scheme)

-- | Write a single DEC-mode-2031 color-scheme report to a pane's pty:
-- @CSI ? 997 ; 1 n@ for dark, @CSI ? 997 ; 2 n@ for light.
notifyColorScheme :: Pane -> ColorScheme -> IO ()
notifyColorScheme pane scheme = Hat.Term.Pty.writePty pane.pty $ case scheme of
    SchemeDark  -> "\ESC[?997;1n"
    SchemeLight -> "\ESC[?997;2n"

-- | The reply to an OSC 10/11 color query, echoing the query's terminator
-- (xterm answers BEL with BEL, ST with ST).
oscColorReply :: Emu.OscColorTarget -> Emu.OscTerm -> ColorScheme -> B.ByteString
oscColorReply target term scheme =
    "\ESC]" <> code <> ";rgb:" <> color <> terminator
  where
    code = case target of
        Emu.Foreground -> "10"
        Emu.Background -> "11"
    color = case (scheme, target) of
        (SchemeDark, Emu.Background)  -> black
        (SchemeDark, Emu.Foreground)  -> white
        (SchemeLight, Emu.Background) -> white
        (SchemeLight, Emu.Foreground) -> black
    black = "0000/0000/0000"
    white = "ffff/ffff/ffff"
    terminator = case term of
        Emu.TermBel -> "\a"
        Emu.TermSt  -> "\ESC\\"

-- Persistence restore ----------------------------------------------------

-- | Rebuild any previously-saved session tree. An absent store or a read
-- failure yields an empty snapshot, i.e. a normal fresh start.
restoreSaved :: ServerState -> FilePath -> IO ()
restoreSaved st path = do
    snap <- withStore path loadSnapshot
        `catch` \(_ :: SomeException) ->
            pure (Snapshot { sessions = [], lastActiveSession = Nothing })
    restoreSnapshot st snap

-- | Recreate every session in the snapshot, spawning a fresh shell in
-- each pane's saved working directory and reapplying the saved layout.
restoreSnapshot :: ServerState -> Snapshot -> IO ()
restoreSnapshot st snap = do
    forM_ snap.sessions (restoreSession st)
    -- Point the next attach at the session that was focused before the
    -- restart. Names are the stable key across restart (ids are fresh).
    forM_ snap.lastActiveSession $ \nm -> do
        sessMap <- readTVarIO st.sessions
        hits <- filterM (fmap (== nm) . readTVarIO . (.name)) (Map.elems sessMap)
        forM_ (listToMaybe hits) $ \s ->
            atomically (writeTVar st.lastActiveSession (Just s.id))

restoreSession :: ServerState -> SessionSnap -> IO ()
restoreSession st ssnap = do
    let wins = filter (not . null . (.panes)) ssnap.windows
    unless (null wins) $ do
        sid <- SessionId <$> atomically (freshId st.nextSession)
        env <- restoreEnv
        whitelist <- restoreWhitelist st
        let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" env)
            sz = Size { rows = 24, cols = 80 }  -- resized on client attach
        built <- forM wins $ \wsnap -> do
            (win, panes) <- restoreWindow st sid shellCmd env sz whitelist wsnap
            pure (wsnap.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member ssnap.currentIx winMap = ssnap.currentIx
                  | otherwise = maybe ssnap.currentIx fst (Map.lookupMin winMap)
            -- Keep the last-active window only if it survived and isn't the
            -- current one; otherwise there is no meaningful "last" to return to.
            lastI = ssnap.lastIx >>= \l ->
                if l /= curIx && Map.member l winMap then Just l else Nothing
        nameVar    <- newTVarIO ssnap.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        lastVar    <- newTVarIO lastI
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO env
        cwdVar     <- newTVarIO (T.unpack ssnap.startCwd)
        optionsVar <- newTVarIO emptyDelta
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, lastIx = lastVar
                , lastSize = sizeVar, environ = environVar
                , startCwd = cwdVar, options = optionsVar }
        atomically $ modifyTVar' st.sessions (Map.insert sid sess)
        forM_ built $ \(_, win, panes) ->
            forM_ panes (startPaneReader st sid win)

restoreWindow
    :: ServerState -> SessionId -> FilePath -> [(Text, Text)] -> Size
    -> [Text] -> WindowSnap -> IO (Window, [Pane])
restoreWindow st sid shellCmd env sz whitelist wsnap = do
    wid <- WindowId <$> atomically (freshId st.nextWindow)
    panes <- forM wsnap.panes $ restorePane st sid shellCmd env sz whitelist
    let pids = map (.id) panes
        paneMap = Map.fromList [(p.id, p) | p <- panes]
        -- Our own emitted string round-trips; the named layout is only a
        -- fallback for a corrupt string, and still contains every pane.
        lay = fromMaybe (namedLayout EvenHorizontal (1 % 2) pids)
                        (layoutFromString wsnap.layout pids)
        activePid = pids !! max 0 (min (length pids - 1) wsnap.active)
        -- Keep the last-active pane only if its ordinal is in range and it
        -- isn't the active pane (nothing to return to otherwise).
        lastActivePid = wsnap.lastActive >>= \o ->
            if o >= 0 && o < length pids && pids !! o /= activePid
                then Just (pids !! o) else Nothing
    nameVar       <- newTVarIO wsnap.name
    layoutVar     <- newTVarIO lay
    layoutNameVar <- newTVarIO Nothing
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    lastActiveVar <- newTVarIO lastActivePid
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    -- Auto-rename status survives the round-trip: a window renaming
    -- automatically keeps tracking its active pane, a manually-named one
    -- keeps its pinned name.
    autoRenameVar <- newTVarIO wsnap.autoRename
    optionsVar    <- newTVarIO emptyDelta
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , lastActive = lastActiveVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , autoRename = autoRenameVar, options = optionsVar }
    pure (win, panes)

restorePane
    :: ServerState -> SessionId -> FilePath -> [(Text, Text)] -> Size
    -> [Text] -> PaneSnap -> IO Pane
restorePane st sid shellCmd env sz whitelist psnap = do
    pid <- PaneId <$> atomically (freshId st.nextPane)
    let origin = if psnap.shellSpawned then ShellSpawned else Direct
    spawnPane st pid sid shellCmd (restoreRun whitelist origin psnap.command)
        (T.unpack psnap.cwd) env sz

-- Reload: capture the live tree with its inherited handles, and rebuild it
-- in the re-exec'd image by adopting them ------------------------------------

-- | Capture the running tree into the two halves of a handover: the evolving
-- 'ReloadState' (the same structure 'captureSnapshot' records, plus each
-- pane's live pty master fd and child pid), and the version-independent
-- 'ReloadCleanup' core (the listening socket fd and the flat list of every
-- pane's (master fd, child pid), so a version-mismatched reload can hang the
-- inherited processes up cleanly rather than orphan them).
captureReload :: ServerState -> IO (ReloadCleanup, ReloadState)
captureReload st = do
    (sess, laName, lsName, mfd) <- atomically $ do
        sessMap <- readTVar st.sessions
        laId    <- readTVar st.lastActiveSession
        laName  <- traverse (readTVar . (.name)) (laId >>= (`Map.lookup` sessMap))
        lsId    <- readTVar st.lastSession
        lsName  <- traverse (readTVar . (.name)) (lsId >>= (`Map.lookup` sessMap))
        mfd     <- readTVar st.listenFd
        pure (Map.elems sessMap, laName, lsName, mfd)
    rsessions <- mapM captureReloadSession sess
    let tree = ReloadState rsessions laName lsName
        liveHandles =
            [ (p.masterFd, p.childPid)
            | s <- rsessions, w <- s.windows, p <- w.panes ]
        cleanup = ReloadCleanup
            { listenFd = fromMaybe (-1) mfd, live = liveHandles }
    pure (cleanup, tree)

captureReloadSession :: Session -> IO ReloadSession
captureReloadSession s = do
    (nm, cwd, curIx, lastI, wstructs) <- atomically $ do
        nm    <- readTVar s.name
        cwd   <- readTVar s.startCwd
        curIx <- readTVar s.currentIx
        lastI <- readTVar s.lastIx
        eff   <- readTVar s.lastSize
        ws    <- Map.toAscList <$> readTVar s.windows
        wstructs <- mapM (windowStruct eff) ws
        pure (nm, cwd, curIx, lastI, wstructs)
    rwins <- mapM captureReloadWindow wstructs
    pure (ReloadSession nm (T.pack cwd) curIx lastI rwins)

captureReloadWindow :: WindowStruct -> IO ReloadWindow
captureReloadWindow ws = do
    rpanes <- forM ws.wsPanes $ \pane -> do
        dir <- paneCurrentPath pane
        let Fd fd = Hat.Term.Pty.masterFd pane.pty
        ms <- Emu.modes pane.emulator
        sc <- captureReloadScreen pane.emulator
        pure (ReloadPane (T.pack dir) (fromIntegral fd)
                (fromIntegral (Hat.Term.Pty.pid pane.pty)) (reloadModesOf ms) sc)
    pure (ReloadWindow ws.wsIx ws.wsName ws.wsLayout ws.wsActive
            ws.wsLastActive ws.wsAutoRename rpanes)

-- | Freeze a pane's emulator into the reload payload: its live grid and cursor,
-- its alt-screen flag, and its scrollback (oldest line first). 'adoptPane'
-- replays this back into the fresh emulator after a reload.
captureReloadScreen :: Emu.Emulator -> IO ReloadScreen
captureReloadScreen emu = do
    scr <- Emu.snapshot emu
    m   <- Emu.modes emu
    len <- Emu.scrollbackLength emu
    sb  <- catMaybes <$> mapM (Emu.scrollbackLine emu) [0 .. len - 1]
    pure ReloadScreen
        { altScreen     = m.altScreen
        , cursorRow     = scr.cursor.row
        , cursorCol     = scr.cursor.col
        , cursorVisible = scr.cursorVisible
        , rows          = map V.toList (V.toList scr.cells)
        , scrollback    = sb
        }

-- | The app-set mode subscriptions to carry across a reload; the inverse
-- rebuild happens in 'adoptPane'.
reloadModesOf :: Emu.Modes -> ReloadModes
reloadModesOf m = ReloadModes
    { colorReport = m.colorReport
    , focusReport = m.focusReport
    , mouse = case m.mouse of
        Emu.MouseOff   -> 0
        Emu.MouseClick -> 1
        Emu.MouseDrag  -> 2
        Emu.MouseMove  -> 3
    }

-- Read and consume the handover file the outgoing image wrote. The frozen
-- envelope yields the cleanup core even for an incompatible version; 'Nothing'
-- only when even that is unreadable (a corrupt or foreign file), where there
-- are no inherited fds to reclaim.
readReload :: Logger -> FilePath -> IO (Maybe Handover)
readReload lg hp = do
    r <- try (B.readFile hp)
    -- Keep the consumed blob as .last rather than deleting it: a resume that
    -- crashes the process (e.g. a native abort in libvterm) leaves the exact
    -- bytes that reproduce it on disk for offline replay.
    renameFile hp (hp <> ".last") `catch` \(_ :: IOException) -> pure ()
    case r of
        Left (e :: IOException) -> do
            logEvent lg ServerCrash
                { err = "reload: unreadable handover: " <> T.pack (show e) }
            pure Nothing
        Right bs -> case decodeHandover bs of
            Left derr -> do
                logEvent lg ServerCrash { err = "reload: bad handover: " <> derr }
                pure Nothing
            Right h -> pure (Just h)

-- | Rebuild the tree from a reload handover, adopting each pane's inherited
-- pty and child rather than spawning. Emulators come up blank; the programs
-- repaint on their next output (a client attach resizes the panes, delivering
-- SIGWINCH, which prompts full-screen apps to redraw).
rebuildReload :: ServerState -> ReloadState -> IO ()
rebuildReload st rs = do
    forM_ rs.sessions (rebuildReloadSession st)
    forM_ rs.currentSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastActiveSession (Just s.id))
    forM_ rs.lastSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastSession (Just s.id))

resolveSessionByName :: ServerState -> Text -> (Session -> IO ()) -> IO ()
resolveSessionByName st nm act = do
    sessMap <- readTVarIO st.sessions
    hits <- filterM (fmap (== nm) . readTVarIO . (.name)) (Map.elems sessMap)
    forM_ (listToMaybe hits) act

rebuildReloadSession :: ServerState -> ReloadSession -> IO ()
rebuildReloadSession st rsess = do
    let wins = filter (not . null . (.panes)) rsess.windows
    unless (null wins) $ do
        sid <- SessionId <$> atomically (freshId st.nextSession)
        env <- restoreEnv
        let sz = Size { rows = 24, cols = 80 }  -- resized on client attach
        built <- forM wins $ \rwin -> do
            (win, panes) <- rebuildReloadWindow st sz rwin
            pure (rwin.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member rsess.currentIx winMap = rsess.currentIx
                  | otherwise = maybe rsess.currentIx fst (Map.lookupMin winMap)
            lastI = rsess.lastIx >>= \l ->
                if l /= curIx && Map.member l winMap then Just l else Nothing
        nameVar    <- newTVarIO rsess.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        lastVar    <- newTVarIO lastI
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO env
        cwdVar     <- newTVarIO (T.unpack rsess.startCwd)
        optionsVar <- newTVarIO emptyDelta
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, lastIx = lastVar
                , lastSize = sizeVar, environ = environVar
                , startCwd = cwdVar, options = optionsVar }
        atomically $ modifyTVar' st.sessions (Map.insert sid sess)
        forM_ built $ \(_, win, panes) ->
            forM_ panes (startPaneReader st sid win)

rebuildReloadWindow :: ServerState -> Size -> ReloadWindow -> IO (Window, [Pane])
rebuildReloadWindow st sz rwin = do
    wid <- WindowId <$> atomically (freshId st.nextWindow)
    histLimit <- (.historyLimit) <$> readTVarIO st.options
    panes <- forM rwin.panes (adoptPane st sz histLimit)
    let pids = map (.id) panes
        paneMap = Map.fromList [(p.id, p) | p <- panes]
        lay = fromMaybe (namedLayout EvenHorizontal (1 % 2) pids)
                        (layoutFromString rwin.layout pids)
        activePid = pids !! max 0 (min (length pids - 1) rwin.active)
        lastActivePid = rwin.lastActive >>= \o ->
            if o >= 0 && o < length pids && pids !! o /= activePid
                then Just (pids !! o) else Nothing
    nameVar       <- newTVarIO rwin.name
    layoutVar     <- newTVarIO lay
    layoutNameVar <- newTVarIO Nothing
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    lastActiveVar <- newTVarIO lastActivePid
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    autoRenameVar <- newTVarIO rwin.autoRename
    optionsVar    <- newTVarIO emptyDelta
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , lastActive = lastActiveVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , autoRename = autoRenameVar, options = optionsVar }
    pure (win, panes)

-- | Build a pane around an inherited pty ('Hat.Term.Pty.adopt') and a blank
-- emulator, for the reload path — the analogue of 'spawnPane' that re-adopts
-- a running child instead of forking a new one.
adoptPane :: ServerState -> Size -> Int -> ReloadPane -> IO Pane
adoptPane st sz histLimit rp = do
    pid <- PaneId <$> atomically (freshId st.nextPane)
    -- Trace each adopt phase so a resume that stalls names the pane and the
    -- step it stalled on (fd adopt vs. screen replay) instead of going silent.
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "start" }
    pty <- Hat.Term.Pty.adopt (Fd (fromIntegral rp.masterFd))
                              (fromIntegral rp.childPid)
    -- Adopt at the size the pane was CAPTURED at, not the session default:
    -- replaying a capture into a smaller grid wraps and clamps it into a
    -- state whose later reflow-resize aborts inside libvterm ("screen_resize
    -- failed to update cursor position", the 2026-07-28 field crash). The
    -- reconcile loop then resizes toward the layout as for any live pane.
    let esz = fromMaybe sz (captureSize rp.screen)
    emu <- Emu.newEmulator esz histLimit
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "replaying" }
    -- Replay the app's mode subscriptions the running program set before the
    -- reload; the fresh emulator starts blank, and the watcher's first scheme
    -- read then re-notifies whatever re-subscribed to ?2031 here.
    -- Then repaint the captured live screen (re-entering the alt screen when
    -- the program was in it, so a later exit reverts cleanly) and reseed the
    -- scrollback, so a full-screen program survives the reload with its display.
    let (replayBytes, replaySb) = replayPane esz rp
    _ <- Emu.feed emu replayBytes
    Emu.seedScrollback emu replaySb
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "ready" }
    sizeVar   <- newTVarIO esz
    deadVar   <- newTVarIO False
    modeVar   <- newTVarIO Nothing
    pipeVar   <- newTVarIO Nothing
    readerVar <- newTVarIO Nothing
    pure Pane
        { id = pid, pty = pty, emulator = emu, size = sizeVar
        , dead = deadVar, startCwd = T.unpack rp.cwd, mode = modeVar
        , pipe = pipeVar, readerTid = readerVar }

-- | Rebuild the emulator mode subscriptions a reload carried. @altScreen@ is
-- left off here; 'adoptPane' sets it from the captured screen before replaying
-- it. Inverse of 'reloadModesOf'.
emuModesOf :: ReloadModes -> Emu.Modes
emuModesOf rm = Emu.Modes
    { altScreen = False
    , colorReport = rm.colorReport
    , focusReport = rm.focusReport
    , mouse = case rm.mouse of
        1 -> Emu.MouseClick
        2 -> Emu.MouseDrag
        3 -> Emu.MouseMove
        _ -> Emu.MouseOff
    }

-- | Rebuild the 'Emu.Screen' a reload captured, sized to the pane, for
-- 'Emu.restoreBytes'. See 'replayPane'.
screenOf :: Size -> ReloadScreen -> Emu.Screen
screenOf sz rs = Emu.Screen
    { size = sz
    , cells = V.fromList (map V.fromList rs.rows)
    , cursor = Pos { row = rs.cursorRow, col = rs.cursorCol }
    , cursorVisible = rs.cursorVisible
    }

-- | What 'adoptPane' feeds a reloaded pane's fresh emulator to reconstruct it:
-- the bytes that replay the mode subscriptions then repaint the captured
-- screen (re-entering the alt screen when the program was in it), paired with
-- the scrollback lines to reseed. Pure, so the capture→replay round trip is
-- testable without a pty.
replayPane :: Size -> ReloadPane -> (B.ByteString, [[Cell.Cell]])
replayPane sz rp =
    ( Emu.modeReplayBytes (emuModesOf rp.modes)
        <> Emu.restoreBytes restoreModes (screenOf sz rp.screen)
    , rp.screen.scrollback )
  where
    restoreModes = (emuModesOf rp.modes) { Emu.altScreen = rp.screen.altScreen }

-- | The size a reload capture was taken at, reconstructed from its grid;
-- 'Nothing' for a blank capture (a migrated pre-screen blob), where there is
-- nothing to preserve and the caller's default applies. See 'adoptPane'.
captureSize :: ReloadScreen -> Maybe Size
captureSize sc = case sc.rows of
    [] -> Nothing
    rs -> Just Size
        { rows = clamp (length rs)
        , cols = clamp (maximum (map length rs)) }
  where
    -- Sane bounds armor a hand-edited or corrupt blob: a Word16-overflowing
    -- or zero dimension must not produce a degenerate emulator.
    clamp n = fromIntegral (max 1 (min 1000 n))

-- The server's own environment seeds restored panes; spawnPane strips and
-- re-adds the hat-specific vars (TERM, TMUX, HAT, …).
restoreEnv :: IO [(Text, Text)]
restoreEnv = map (\(k, v) -> (T.pack k, T.pack v)) <$> getEnvironment

-- | Commands worth re-running when a pane is restored, rather than
-- dropping to a fresh shell. Overridable via the @\@restore-commands@
-- user option (a space-separated list).
defaultRestoreCommands :: [Text]
defaultRestoreCommands =
    [ "vim", "nvim", "vi", "view", "emacs", "nano"
    , "less", "man", "tail", "watch"
    , "top", "htop", "atop", "btop"
    , "claude" ]

restoreWhitelist :: ServerState -> IO [Text]
restoreWhitelist st = do
    opts <- readTVarIO st.options
    pure $ case Map.lookup "@restore-commands" opts.user of
        Just v | not (T.null (T.strip v)) -> T.words v
        _                                 -> defaultRestoreCommands

-- | Re-run the captured argv only when its program is whitelisted; otherwise
-- the pane comes back as a plain shell. The argv is passed through unsplit.
restoreRun :: [Text] -> SpawnOrigin -> Maybe [Text] -> PaneStart
restoreRun whitelist origin mcmd = case mcmd of
    Just argv@(prog : _) | commandName prog `elem` whitelist ->
        let resumed = resumeArgv (commandName prog) argv
        in case origin of
            -- Started from the pane's shell: bring its per-directory env back.
            ShellSpawned -> ShellExecArgv resumed
            -- Launched directly: exec bare, so @vim "Foo Bar.txt"@ isn't re-split.
            Direct       -> ExecArgv resumed
    _ -> FreshShell

-- | The argv a whitelisted program is re-exec'd with. Most programs get
-- their captured argv back verbatim, but claude's saved arguments name a
-- past invocation, not the running conversation — only @claude --continue@
-- resumes it, so the arguments are rewritten to that regardless of what
-- was saved. The captured program path (argv[0]) is kept, so the exact
-- binary re-execs.
resumeArgv :: Text -> [Text] -> [Text]
resumeArgv "claude" (prog : _) = [prog, "--continue"]
resumeArgv _        argv       = argv

-- | The program a captured foreground command names: its last path
-- segment with NixOS's @.<name>-wrapped@ decoration stripped, so a pane
-- running @\/nix\/store\/…\/.vim-wrapped@ is recorded (and matched) as
-- @vim@.
commandName :: Text -> Text
commandName raw =
    let base  = T.takeWhileEnd (/= '/') (firstWord raw)
        undot = T.dropWhile (== '.') base
    in fromMaybe undot (T.stripSuffix "-wrapped" undot)
  where
    firstWord t = case T.words t of
        (w : _) -> w
        []      -> ""

-- Configuration --------------------------------------------------------

defaultKeymap :: Keymap
defaultKeymap = Map.fromList
    [ ("prefix", Map.fromList (map bindArgv prefixBindings))
    , ("root", Map.empty)
    , ("copy-mode", Map.fromList (map copyBind copyModeBindings))
    , ("copy-mode-vi", Map.fromList (map copyBind copyModeViBindings <> digitBinds <> searchBinds))
    ]
  where
    bindArgv (k, cmd) = (k, [cmd])
    -- A copy-mode key runs a single @send-keys -X <name>@ command.
    copyBind (k, name) = (k, [["send-keys", "-X", name]])
    -- Digit keys feed the vi @[count]@ prefix; a bare @0@ is start-of-line.
    digitBinds =
        [ (tshow d, [["send-keys", "-X", "digit", tshow d]]) | d <- [0 .. 9 :: Int] ]
    -- @/@ and @?@ open the command prompt to collect a search query, then
    -- run the search on submit (the @%%@ splice carries the typed line).
    searchBinds =
        [ ("/", [["command-prompt", "-p", "(search down)"
                 , "send-keys -X search-forward '%%'"]])
        , ("?", [["command-prompt", "-p", "(search up)"
                 , "send-keys -X search-backward '%%'"]])
        ]
    prefixBindings =
        [ ("a", ["activity-window"])
        , ("d", ["detach-client"])
        , ("c", ["new-window"])
        , ("w", ["choose-tree", "-Zw"])
        , ("%", ["split-window", "-h"])
        , ("\"", ["split-window", "-v"])
        , ("x", ["kill-pane"])
        , ("&", ["kill-window"])
        , (",", ["command-prompt", "-I", "#W", "rename-window '%%'"])
        , (".", ["command-prompt", "-p", "(index)", "move-window -t '%%'"])
        , ("$", ["command-prompt", "-I", "#S", "rename-session '%%'"])
        , ("z", ["resize-pane", "-Z"])
        , ("Space", ["next-layout"])
        , ("M-1", ["select-layout", "even-horizontal"])
        , ("M-2", ["select-layout", "even-vertical"])
        , ("M-3", ["select-layout", "main-horizontal"])
        , ("M-4", ["select-layout", "main-vertical"])
        , ("M-5", ["select-layout", "tiled"])
        , ("o", ["select-pane", "-t", ":.+"])
        , ("O", ["select-pane", "-t", ":.-"])
        , ("n", ["next-window"])
        , ("p", ["previous-window"])
        , ("l", ["last-window"])
        , (";", ["last-pane"])
        , ("C-b", ["send-prefix"])
        , ("[", ["copy-mode"])
        , ("]", ["paste-buffer"])
        , (":", ["command-prompt"])
        , ("Left", ["select-pane", "-L"])
        , ("Right", ["select-pane", "-R"])
        , ("Up", ["select-pane", "-U"])
        , ("Down", ["select-pane", "-D"])
        , ("C-Left", ["resize-pane", "-L"])
        , ("C-Right", ["resize-pane", "-R"])
        , ("C-Up", ["resize-pane", "-U"])
        , ("C-Down", ["resize-pane", "-D"])
        ]
        <> [ (tshow i, ["select-window", "-t", tshow (i :: Int)])
           | i <- [0 .. 9]
           ]
    copyModeViBindings =
        [ ("h", "cursor-left"), ("j", "cursor-down")
        , ("k", "cursor-up"), ("l", "cursor-right")
        , ("$", "end-of-line")
        , ("w", "next-word"), ("b", "previous-word"), ("e", "next-word-end")
        , ("W", "next-space"), ("B", "previous-space"), ("E", "next-space-end")
        , ("^", "back-to-indentation")
        , ("{", "previous-paragraph"), ("}", "next-paragraph")
        , ("f", "jump-forward"), ("F", "jump-backward")
        , ("t", "jump-to-forward"), ("T", "jump-to-backward")
        , (";", "jump-again"), (",", "jump-reverse")
        , ("n", "search-again"), ("N", "search-reverse")
        , ("g", "history-top"), ("G", "history-bottom")
        , ("H", "top-line"), ("M", "middle-line"), ("L", "bottom-line")
        , ("C-f", "page-down"), ("PgDn", "page-down")
        , ("C-b", "page-up"), ("PgUp", "page-up")
        , ("C-d", "halfpage-down"), ("C-u", "halfpage-up")
        , ("v", "begin-selection"), ("Space", "begin-selection")
        , ("V", "select-line")
        , ("o", "other-end"), ("Escape", "clear-selection")
        , ("y", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]
    copyModeBindings =
        [ ("C-b", "cursor-left"), ("C-f", "cursor-right")
        , ("C-p", "cursor-up"), ("C-n", "cursor-down")
        , ("C-a", "start-of-line"), ("C-e", "end-of-line")
        , ("M-f", "next-word-end"), ("M-b", "previous-word")
        , ("M-<", "history-top")
        , ("Space", "begin-selection"), ("C-g", "clear-selection")
        , ("M-w", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel"), ("Escape", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]

-- | Read a config file as UTF-8, independent of the process locale and
-- tolerant of malformed bytes. 'TIO.readFile' decodes with the locale
-- encoding, so under a non-UTF-8 locale a config with any non-ASCII byte
-- (a @·@ separator, a @👀@ marker) threw mid-read and aborted the whole
-- startup — read the bytes and decode UTF-8 leniently instead.
readConfigUtf8 :: FilePath -> IO Text
readConfigUtf8 p = TE.decodeUtf8With TEE.lenientDecode <$> B.readFile p

loadConfig :: ServerState -> Maybe FilePath -> IO ()
loadConfig st mconfig =
    forM_ mconfig $ \p -> do
        exists <- doesFileExist p
        when exists $ do
            contents <- readConfigUtf8 p
            case parseConfig contents of
                Left err -> logEvent st.logger ConfigError
                    { file = p, err = err }
                Right cmds -> forM_ cmds $ \argv -> do
                    replies <- runArgv st Nothing argv
                    forM_ [e | RErr e <- replies] $ \e ->
                        logEvent st.logger ConfigError { file = p, err = e }

-- Connections ----------------------------------------------------------

acceptLoop :: ServerState -> N.Socket -> IO ()
acceptLoop st lsock = forever $ do
    (conn, _) <- N.accept lsock
    -- The autostarting client has now reached us; the idle-exit may
    -- consider draining (see 'waitIdle'). This is what lets us drop the
    -- old fixed-delay grace period without racing that client.
    atomically $ writeTVar st.served True
    void . forkIO $
        handleConn st conn
            `catch` (\(e :: SomeException) ->
                logEvent st.logger ServerCrash { err = T.pack (show e) })
            `finally` (N.close conn `catch` \(_ :: SomeException) -> pure ())

handleConn :: ServerState -> N.Socket -> IO ()
handleConn st conn = do
    m <- recvMessage conn
    case m of
        Just (Known (ClientHello h))
            | h.protoVersion == protocolVersion -> welcome st conn h
            | otherwise -> sendMessage conn $ ServerError $
                "protocol mismatch: server " <> tshow protocolVersion
                <> ", client " <> tshow h.protoVersion
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: ServerState -> N.Socket -> Hello -> IO ()
welcome st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            atomically $ writeTVar client.ready True
            controlLoop st client `finally` removeClient st client
        AttachIntent setupCmds -> do
            -- Register early so the setup commands (new-session,
            -- attach-session -t) act on a live client and can switch it.
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            setupErr <- attachSetup st client setupCmds
            msess <- case setupErr of
                Just _ -> pure Nothing
                Nothing -> currentSession st client
            case (setupErr, msess) of
                (Just e, _) ->
                    sendMessage conn (ServerError e) >> removeClient st client
                (_, Nothing) -> do
                    sendMessage conn (ServerError "no session to attach")
                    removeClient st client
                (_, Just sess) -> do
                    refreshSessionEnv st sess client
                    sname <- readTVarIO sess.name
                    atomically $ writeTVar st.everAttached True
                    applySessionSize st sess.id
                    sendMessage conn (Welcome sname)
                    atomically $ writeTVar client.ready True
                    logEvent st.logger ClientConnected
                        { client = rawClient client.id, term = h.term }
                    withAsync (renderLoop st client) $ \_ ->
                        inputLoop st client
                            `finally` removeClient st client

-- | Run an attaching client's setup commands, leaving @client.session@
-- pointing at the session it should render. An empty command list means a
-- plain attach: reuse an existing session or create one. Returns the
-- first error, if any, so 'welcome' can reject the attach.
attachSetup :: ServerState -> Client -> [[Text]] -> IO (Maybe Text)
attachSetup st client [] = do
    sess <- ensureSession st client
    atomically $ do
        writeTVar client.session sess.id
        writeTVar st.lastActiveSession (Just sess.id)
        -- Adopt the server-wide alternate (e.g. one carried across a reload) so
        -- @switch-client -l@ still returns to it. See 'switchClientTo'.
        gLast <- readTVar st.lastSession
        forM_ gLast $ \g -> when (g /= sess.id) $
            writeTVar client.lastSession (Just g)
    pure Nothing
attachSetup st client cmds = do
    replies <- runCommands st (Just client) cmds
    pure (listToMaybe [e | RErr e <- replies])

-- | The session @client.session@ currently names, if it still exists.
currentSession :: ServerState -> Client -> IO (Maybe Session)
currentSession st client = do
    sid <- readTVarIO client.session
    Map.lookup sid <$> readTVarIO st.sessions

newClient :: ServerState -> N.Socket -> Hello -> IO Client
newClient st conn h = do
    cid <- atomically (freshId st.nextClient)
    sendLock <- newMVar ()
    sizeVar <- newTVarIO h.size
    activeVar <- newTVarIO 0
    sessVar <- newTVarIO (SessionId (-1))
    lastSessVar <- newTVarIO Nothing
    keyVar <- newIORef NoPrefix
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    readyVar <- newTVarIO False
    focusVar <- newTVarIO True
    pure Client
        { id = ClientId cid
        , sock = conn
        , sendLock = sendLock
        , size = sizeVar
        , lastActive = activeVar
        , session = sessVar
        , lastSession = lastSessVar
        , ready = readyVar
        , keyState = keyVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , picker = pickerVar
        , outerFocused = focusVar
        , env = h.env
        , cwd = h.cwd
        }

removeClient :: ServerState -> Client -> IO ()
removeClient st client = do
    sid <- readTVarIO client.session
    atomically $ modifyTVar' st.clients (Map.delete client.id)
    applySessionSize st sid
    logEvent st.logger ClientDetached
        { client = rawClient client.id, reason = "gone" }

-- Sessions --------------------------------------------------------------

-- | On attach, copy the @update-environment@ vars from the attaching
-- client's env into the session, so panes spawned afterward see fresh
-- values (e.g. a new @DISPLAY@ after reconnecting over @ssh -X@).
refreshSessionEnv :: ServerState -> Session -> Client -> IO ()
refreshSessionEnv st sess client = do
    vars <- (.updateEnvironment) <$> readTVarIO st.options
    atomically $ modifyTVar' sess.environ
        (applyUpdateEnvironment vars client.env)

-- | Fold the @update-environment@ vars from a client's env into a session's:
-- each listed var the client has replaces the session's entry; unlisted vars,
-- and listed vars the client lacks, are left as they were. See
-- 'refreshSessionEnv'.
applyUpdateEnvironment
    :: [Text] -> [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
applyUpdateEnvironment vars clientEnv = \env0 -> List.foldl' step env0 vars
  where
    step env v = case List.lookup v clientEnv of
        Just val -> (v, val) : filter ((/= v) . fst) env
        Nothing  -> env

ensureSession :: ServerState -> Client -> IO Session
ensureSession st client = do
    -- Let any restore finish first, so we attach to the restored tree
    -- rather than racing it and creating a redundant fresh session.
    atomically $ readTVar st.restoring >>= \r -> when r retry
    (existing, laId) <- atomically $
        (,) <$> readTVar st.sessions <*> readTVar st.lastActiveSession
    case pickAttachSession laId existing of
        Just (_, sess) -> pure sess
        Nothing -> createSession st Nothing Nothing client.env
            (T.unpack client.cwd) =<< readTVarIO client.size

-- | Which existing session a fresh client attaches to: the last-active one
-- if it still exists, else the first-created (lowest-id) session. After a
-- restore 'lastActiveSession' names the session that was focused, so a reboot
-- returns there rather than always landing on @$1@.
pickAttachSession
    :: Ord k => Maybe k -> Map.Map k v -> Maybe (k, v)
pickAttachSession lastActive m =
    case lastActive >>= \k -> (,) k <$> Map.lookup k m of
        Just kv -> Just kv
        Nothing -> Map.lookupMin m

createSession
    :: ServerState -> Maybe Text -> Maybe Text -> [(Text, Text)]
    -> FilePath -> Size -> IO Session
createSession st mname mrun environ dir sz = do
    sid <- atomically (freshId st.nextSession)
    opts <- readTVarIO st.options
    let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" environ)
    (win, pane) <- newWindowWithPane st (SessionId sid) shellCmd mrun
        dir environ (windowArea sz)
    nameVar <- newTVarIO (fromMaybe (tshow sid) mname)
    windowsVar <- newTVarIO (Map.singleton opts.baseIndex win)
    currentVar <- newTVarIO opts.baseIndex
    lastVar <- newTVarIO Nothing
    sizeVar <- newTVarIO sz
    environVar <- newTVarIO environ
    cwdVar <- newTVarIO dir
    optionsVar <- newTVarIO emptyDelta
    let sess = Session
            { id = SessionId sid
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , lastIx = lastVar
            , lastSize = sizeVar
            , environ = environVar
            , startCwd = cwdVar
            , options = optionsVar
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    pure sess

newWindowWithPane
    :: ServerState -> SessionId -> FilePath -> Maybe Text -> FilePath
    -> [(Text, Text)] -> Size -> IO (Window, Pane)
newWindowWithPane st sid shellCmd mrun dir environ sz = do
    (wid, pid) <- atomically $
        (,) <$> freshId st.nextWindow <*> freshId st.nextPane
    pane <- spawnPane st (PaneId pid) sid shellCmd (shellStart mrun) dir environ sz
    nameVar <- newTVarIO $ case mrun of
        Just cmd -> T.takeWhile (/= ' ') cmd
        Nothing -> T.pack (baseName shellCmd)
    layoutVar <- newTVarIO (Leaf pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    lastActiveVar <- newTVarIO Nothing
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar    <- newTVarIO emptyDelta
    let win = Window
            { id = WindowId wid
            , name = nameVar
            , layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar
            , activeId = activeVar
            , lastActive = lastActiveVar
            , bellFlag = bellVar
            , activity = activityVar
            , zoomed = zoomVar
            , autoRename = autoRenameVar
            , options = optionsVar
            }
    pure (win, pane)
  where
    baseName = Prelude.reverse . takeWhile (/= '/') . Prelude.reverse

-- | How a freshly-spawned pane chooses its process.
data PaneStart
    = FreshShell         -- ^ the session's login shell, no command
    | ShellCommand Text  -- ^ a user-supplied command line, run via @sh -c@
                         --   (@new-window@\/@split-window@ with an argument)
    | ExecArgv [Text]    -- ^ exec this argv directly, no shell. See
                         --   'restoreRun'.
    | ShellExecArgv [Text]
                         -- ^ relaunch this argv with the pane's per-directory
                         --   env restored. See 'restoreShellExec'.
    deriving (Eq, Show)

-- | Where a captured program was running: as a child of the pane's
-- interactive shell (the user typed it), or as the pane's own top-level
-- process (a pane launched directly on a program). See 'restoreRun'.
data SpawnOrigin = ShellSpawned | Direct
    deriving (Eq, Show)

-- | The shell-command spawn semantics for the user-facing @new-window@ and
-- @split-window@: an argument is a command line for the shell to interpret.
shellStart :: Maybe Text -> PaneStart
shellStart = maybe FreshShell ShellCommand

-- | Whether @direnv@ is on the server's PATH. See 'restoreShellExec'.
data DirenvAvailable = DirenvOnPath | DirenvAbsent
    deriving (Eq, Show)

-- | How to relaunch a shell-spawned restored program so its pane's
-- per-directory env loads before it runs. @argv@ rides as separate arguments,
-- never spliced into a command string, so an argument with spaces stays one.
restoreShellExec
    :: DirenvAvailable -> FilePath -> FilePath -> [Text] -> (String, [String])
-- direnv execs the program directly with the cwd's @.envrc@ loaded, so the env
-- is restored the same under any shell — no rc, no shell-specific prompt hook.
restoreShellExec DirenvOnPath _shell cwd argv =
    ("direnv", ["exec", cwd] <> map T.unpack argv)
-- Without direnv, a login shell sources the user's rc (aliases, PATH) first.
restoreShellExec DirenvAbsent shellCmd _cwd argv =
    (shellCmd, ["-i", "-c", "exec \"$@\"", "hat-restore"] <> map T.unpack argv)

spawnPane
    :: ServerState -> PaneId -> SessionId -> FilePath -> PaneStart
    -> FilePath -> [(Text, Text)] -> Size -> IO Pane
spawnPane st pid sid shellCmd mrun dir environ sz = do
    serverPid <- getProcessID
    opts <- readTVarIO st.options
    direnv <- maybe DirenvAbsent (const DirenvOnPath)
        <$> findExecutable "direnv"
    let cleanEnv =
            [ (T.unpack k, T.unpack v)
            | (k, v) <- environ
            , k `notElem` ["TERM", "TMUX", "TMUX_PANE", "HAT", "HAT_PANE"]
            ]
        hatVar = st.sockPath <> "," <> show serverPid <> ","
            <> show (rawSession sid)
        term = T.unpack opts.defaultTerminal
        paneEnv = cleanEnv <>
            [ ("TERM", term)
            , ("TMUX", hatVar)
            , ("HAT", hatVar)
            , ("TMUX_PANE", "%" <> show (rawPane pid))
            , ("HAT_PANE", "%" <> show (rawPane pid))
            ]
        (cmd, args) = case mrun of
            FreshShell             -> (shellCmd, [])
            ShellCommand run       -> ("/bin/sh", ["-c", T.unpack run])
            ExecArgv (p:rest)      -> (T.unpack p, map T.unpack rest)
            ExecArgv []            -> (shellCmd, [])
            ShellExecArgv []       -> (shellCmd, [])
            ShellExecArgv argv     -> restoreShellExec direnv shellCmd dir argv
    pty <- Hat.Term.Pty.spawn Hat.Term.Pty.Spawn
        { cmd = cmd
        , args = args
        , env = paneEnv
        , cwd = Just dir
        , size = sz
        }
    emu <- Emu.newEmulator sz opts.historyLimit
    sizeVar <- newTVarIO sz
    deadVar <- newTVarIO False
    modeVar <- newTVarIO Nothing
    pipeVar <- newTVarIO Nothing
    readerVar <- newTVarIO Nothing
    logEvent st.logger PaneSpawned
        { pane = rawPane pid, cmd = T.pack cmd }
    pure Pane
        { id = pid
        , pty = pty
        , emulator = emu
        , size = sizeVar
        , dead = deadVar
        , startCwd = dir
        , mode = modeVar
        , pipe = pipeVar
        , readerTid = readerVar
        }

-- | The reader thread owns a pane's lifetime: it pumps pty output into
-- the emulator until end-of-file, and 'closePane' runs in a @finally@ so
-- the pane's resources and model entry are released however the loop ends
-- — clean EOF, a hang-up from a kill command, or an exception.
--
-- The @sid@\/@win@ are the pane's window AT SPAWN. They drive only the
-- best-effort attention markers (bell\/activity) and desktop-notification
-- routing, where a stale window after a re-parent is a harmless misroute — not
-- worth a tree scan on every screen change. Teardown must NOT use them: a
-- re-parented pane's 'closePane' finds its CURRENT window (see
-- 'detachPaneCurrent'), or it would strand the pane in the wrong window.
startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st sid win pane = do
    -- Count the pane live before forking, so a kill that lands before the
    -- reader is scheduled still finds it counted and 'waitIdle' waits for
    -- its reap. Decremented once the reader (and its 'reapPane') is done.
    atomically $ modifyTVar' st.livePanes (+ 1)
    void . forkIO $ do
        tid <- myThreadId
        atomically $ writeTVar pane.readerTid (Just tid)
        readLoop
            `finally` closePane st pane
            `finally` atomically (modifyTVar' st.livePanes (subtract 1))
  where
    readLoop = do
        bs <- Hat.Term.Pty.readPty pane.pty
        unless (B8.null bs) $ do
            forwardToPipe pane bs
            events <- Emu.feed pane.emulator bs
            forM_ events $ \case
                Emu.Output out -> Hat.Term.Pty.writePty pane.pty out
                -- The app asked the current light/dark scheme (CSI ? 996 n);
                -- answer from the OS scheme the server already tracks.
                Emu.ColorSchemeQuery -> do
                    mscheme <- readTVarIO st.colorScheme
                    forM_ mscheme (notifyColorScheme pane)
                -- The app asked its terminal's fg/bg color (OSC 10/11 ;?),
                -- the classic light-versus-dark probe; hat doesn't paint a
                -- pane background, so answer with the canonical color for
                -- the OS scheme it tracks (white-on-black when dark).
                Emu.OscColorQuery target term -> do
                    mscheme <- readTVarIO st.colorScheme
                    forM_ mscheme $ \scheme ->
                        Hat.Term.Pty.writePty pane.pty (oscColorReply target term scheme)
                -- The app raised a desktop notification (OSC 9/777); hat has
                -- no notification UI of its own, so forward it verbatim to
                -- the session's attached terminals to raise with the OS.
                Emu.DesktopNotification raw -> broadcast st sid (Notify raw)
                -- The pane's own OSC title only feeds #{pane_title} (the
                -- emulator stores it); the client's desktop title is
                -- composed in 'refreshTitles'.
                Emu.TitleChanged _ -> pure ()
                Emu.Bell -> do
                    atomically $ do
                        markBell st sid win
                        bumpDirty st
                    broadcast st sid RingBell
                Emu.ScreenChanged -> atomically $ do
                    markActivity st sid win
                    bumpDirty st
                -- libvterm reported a terminal property hat does not act on;
                -- surface it as a warning rather than silently dropping it.
                Emu.UnknownProp kind prop ->
                    logEvent st.logger UnknownTermProp
                        { pane = rawPane pane.id
                        , propKind = propKindLabel kind
                        , prop = prop
                        }
            readLoop

-- | Name a vterm prop's value kind for the 'UnknownTermProp' log.
propKindLabel :: Emu.PropKind -> Text
propKindLabel = \case
    Emu.PropBool -> "bool"
    Emu.PropInt  -> "int"
    Emu.PropStr  -> "str"

-- | Whether @win@ is the session's current (foreground) window.
isCurrentWindow :: Session -> Window -> STM Bool
isCurrentWindow sess win = do
    cur <- readTVar sess.currentIx
    ws <- readTVar sess.windows
    pure $ maybe False (\w -> w.id == win.id) (Map.lookup cur ws)

-- | Whether a window counts as being watched, so its attention marker is
-- suppressed. A window is "seen" only when it is the session's current
-- window AND some attached client viewing it has its outer terminal focused
-- (?1004). If every viewer's outer terminal is unfocused — the user has
-- switched to another OS window — even the current window is unwatched and
-- must still flag activity/bell.
attentionSeen :: Bool -> Bool -> Bool
attentionSeen isCurrent anyViewerFocused = isCurrent && anyViewerFocused

-- | Resolve 'attentionSeen' against live state: whether @win@ is the
-- session's current window and any attached client has outer focus.
windowSeen :: ServerState -> Session -> Window -> STM Bool
windowSeen st sess win = do
    current <- isCurrentWindow sess win
    cs <- sessionClients st sess.id
    anyFocused <- or <$> mapM (readTVar . (.outerFocused)) cs
    pure (attentionSeen current anyFocused)

-- | Flag a window as having activity, when @monitor-activity@ is on. A
-- window being watched ('windowSeen') is exempt — no attention marker
-- belongs on what the user is looking at (mirrors 'markBell', which exempts
-- a watched window the same way).
markActivity :: ServerState -> SessionId -> Window -> STM ()
markActivity st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        opts <- resolveForWindow st sess win
        when opts.monitorActivity $ do
            seen <- windowSeen st sess win
            unless seen $ writeTVar win.activity True

-- | Raise a window's bell marker. A window being watched ('windowSeen') is
-- exempt — you are already looking at it — so only a bell you are not
-- watching leaves a visible marker; the audible bell rings regardless (see
-- the 'Emu.Bell' handler).
markBell :: ServerState -> SessionId -> Window -> STM ()
markBell st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        seen <- windowSeen st sess win
        unless seen $ writeTVar win.bellFlag True

-- | Track a client's outer-terminal focus from its ?1004 reports. FocusOut
-- means the user looked away, so activity/bell on the window it is viewing
-- will start flagging (see 'windowSeen'). FocusIn means they are looking
-- again: clear the marker on the window the client now views. Non-focus
-- keys are ignored.
noteOuterFocus :: ServerState -> Client -> Key -> IO ()
noteOuterFocus st client k
    | k.name == "FocusOut" = atomically $ writeTVar client.outerFocused False
    | k.name == "FocusIn"  = atomically $ do
        writeTVar client.outerFocused True
        sid <- readTVar client.session
        msess <- Map.lookup sid <$> readTVar st.sessions
        forM_ msess $ \sess -> do
            mwin <- currentWindow sess
            forM_ mwin $ \win -> do
                writeTVar win.activity False
                writeTVar win.bellFlag False
        bumpDirty st
    | otherwise = pure ()

-- The grace a hung-up child gets before SIGKILL: long enough for a
-- well-behaved child to die on SIGHUP, short enough that a shutdown never
-- stalls on a stubborn one. See 'closePane'.
paneExitWaitMicros :: Int
paneExitWaitMicros = 400_000

-- | Tear a pane down from an external command (kill-pane/window/session,
-- kill-server) without deadlocking. 'closePty' would block closing the
-- master Handle while the reader still holds it in a read that a
-- SIGHUP-ignoring child never ends; instead SIGHUP the child, then
-- interrupt the reader so its blocked read returns and its @finally@ runs
-- 'closePane' (which releases the master Handle and the rest). Before the
-- reader has recorded its id (a brief spawn race) fall back to 'closePty'.
hangupPane :: Pane -> IO ()
hangupPane pane = do
    Hat.Term.Pty.signalHangup pane.pty
    mtid <- readTVarIO pane.readerTid
    case mtid of
        Just tid -> killThread tid
        Nothing  -> Hat.Term.Pty.closePty pane.pty

-- | The model half of the reader thread's teardown: detach the pane from the
-- window it lives in NOW. A pane is mobile — break-pane, join-pane, and
-- swap-pane re-parent a live pane after its reader started — so teardown must
-- find the pane's current window ('locatePane') rather than the one it was
-- spawned in, or it detaches from the wrong window and strands the pane. Yields
-- the session detached from (for the reflow and the emptied-session notice), or
-- 'Nothing' when a killing command already removed the pane from the tree.
detachPaneCurrent :: ServerState -> Pane -> STM (Maybe SessionId, DetachResult)
detachPaneCurrent st pane = do
    mloc <- locatePane st pane.id
    case mloc of
        Just (sid, win) -> (,) (Just sid) <$> detachPane st sid win pane
        Nothing         -> pure (Nothing, AlreadyDetached)

-- | Tear a pane down from the reader thread's @finally@: the model detach
-- (idempotent — a killing command may already have done it, see
-- 'detachPane') followed by the OS-side reap, which runs exactly once here
-- so no teardown path can forget a resource. (The emulator frees itself via
-- its finalizer.)
closePane :: ServerState -> Pane -> IO ()
closePane st pane = do
    (msid, r) <- atomically (detachPaneCurrent st pane)
    forM_ msid $ \sid -> when (r /= AlreadyDetached) $ applySessionSize st sid
    reapPane st pane
    forM_ msid $ \sid -> when (r == Detached SessionEmptied) $ broadcast st sid Exited

-- | The model half of a pane's teardown, in one atomic transaction: drop
-- the pane from its window's map and layout, reactivate a surviving pane,
-- and collapse an emptied window (and session) upward. Guarded by
-- @pane.dead@, so of the two racers that may both attempt it — the killing
-- command ('cmdKillPane', for a reflow synchronous with the command) and
-- the reader thread's @finally@ — exactly one performs it under ANY
-- interleaving; the loser sees 'AlreadyDetached' and must not touch the
-- model (a second removal would find the leaf gone and wrongly collapse
-- the window). Deliberately free of any OS-side effect: the child's
-- lifetime is 'reapPane''s business, so the screen never waits on a
-- process.
detachPane :: ServerState -> SessionId -> Window -> Pane -> STM DetachResult
detachPane st sid win pane = do
    isDead <- readTVar pane.dead
    if isDead then pure AlreadyDetached else do
        writeTVar pane.dead True
        modifyTVar' win.panes (Map.delete pane.id)
        lay <- readTVar win.layout
        mz <- readTVar win.zoomed
        when (mz == Just pane.id) $ writeTVar win.zoomed Nothing
        case removeLeaf pane.id lay of
            Just lay' -> do
                writeTVar win.layout lay'
                active <- readTVar win.activeId
                when (active == pane.id) $ do
                    mlast <- readTVar win.lastActive
                    forM_ (chooseActivePaneOnClose (layoutPanes lay') mlast) $ \next -> do
                        writeTVar win.activeId next
                        writeTVar win.lastActive Nothing
                bumpDirty st
                pure (Detached SessionSurvives)
            Nothing -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure (Detached SessionSurvives)
                    Just sess -> do
                        ws <- readTVar sess.windows
                        let ws' = Map.filter (\w -> w.id /= win.id) ws
                        writeTVar sess.windows ws'
                        if Map.null ws'
                            then do
                                modifyTVar' st.sessions (Map.delete sid)
                                pure (Detached SessionEmptied)
                            else do
                                cur <- readTVar sess.currentIx
                                mlast <- readTVar sess.lastIx
                                let survivors = Map.keysSet ws'
                                forM_ (chooseCurrentOnClose survivors cur mlast) $ \ix -> do
                                    writeTVar sess.currentIx ix
                                    writeTVar sess.lastIx Nothing
                                bumpDirty st
                                pure (Detached SessionSurvives)

-- | What 'detachPane' did. See 'detachPane'.
data DetachResult = AlreadyDetached | Detached SessionFate
    deriving (Eq, Show)

-- | Whether a detach emptied the pane's whole session. See 'detachPane'.
data SessionFate = SessionSurvives | SessionEmptied
    deriving (Eq, Show)

-- | The OS half of a pane's teardown: stop any pipe-pane, wait for the
-- child — bounded, since a child that ignores SIGHUP would never exit on
-- its own; on timeout escalate to SIGKILL and wait again (the reaper
-- always fills the exit slot, so the second wait is guaranteed to
-- complete) — then release the pty. Runs once, in the reader thread's
-- @finally@ (see 'closePane').
reapPane :: ServerState -> Pane -> IO ()
reapPane st pane = do
    stopPipe pane
    reaped <- timeout paneExitWaitMicros (Hat.Term.Pty.waitExit pane.pty)
    when (reaped == Nothing) $ do
        Hat.Term.Pty.signalKill pane.pty
        void $ Hat.Term.Pty.waitExit pane.pty
    Hat.Term.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }

-- | Detach a whole set of located panes in one transaction, so a window —
-- and a session emptied with it — collapses atomically. Returns the
-- sessions the detach emptied (their clients need an @Exited@). The model
-- primitive behind kill-window\/-session\/-server; see 'killPaneLocs' for
-- the surrounding OS teardown and 'detachPane' for the per-pane guard.
detachPanes :: ServerState -> [(SessionId, Window, Pane)] -> STM [SessionId]
detachPanes st = fmap catMaybes . mapM detach
  where
    detach (sid, win, pane) = do
        r <- detachPane st sid win pane
        pure (if r == Detached SessionEmptied then Just sid else Nothing)

-- | Kill a set of located panes: detach them from the model in one
-- transaction (the reflow is synchronous with the command and never waits
-- on signal delivery, child exit, or the reader's reap), redraw the
-- sessions that survive, tell the clients of any emptied session, and hang
-- the children up for their reader threads to reap ('closePane'). The one
-- IO primitive behind kill-pane\/-window\/-session.
killPaneLocs :: ServerState -> [(SessionId, Window, Pane)] -> IO ()
killPaneLocs st locs = do
    emptied <- atomically (detachPanes st locs)
    forM_ (List.nub [sid | (sid, _, _) <- locs]) (applySessionSize st)
    forM_ (List.nub emptied) $ \sid -> broadcast st sid Exited
    forM_ locs $ \(_, _, pane) -> hangupPane pane

-- | Pick the window to make current after one is closed. 'Nothing' means
-- leave the current window as-is (it survived the close). Otherwise, when
-- the current window is gone, prefer the session's last-active window (as
-- tmux does), falling back to the lowest-numbered survivor when there is
-- no last-active window or it too has been closed.
chooseCurrentOnClose
    :: Set.Set Int   -- ^ indices of the windows that remain
    -> Int           -- ^ the current window index
    -> Maybe Int     -- ^ the last-active window index, if any
    -> Maybe Int
chooseCurrentOnClose survivors cur mlast
    | Set.member cur survivors = Nothing
    | Just lastIx <- mlast, Set.member lastIx survivors = Just lastIx
    | otherwise = Set.lookupMin survivors

-- | Pick the pane to make active after the active one is closed. Prefer the
-- window's last-active pane (as tmux does), falling back to the first
-- surviving pane in layout order when there is no last-active pane or it too
-- has been closed. 'Nothing' means no panes remain.
chooseActivePaneOnClose
    :: [PaneId]        -- ^ surviving panes, in layout order
    -> Maybe PaneId    -- ^ the last-active pane, if any
    -> Maybe PaneId
chooseActivePaneOnClose survivors mlast
    | Just lastP <- mlast, lastP `elem` survivors = Just lastP
    | otherwise = case survivors of
        (next : _) -> Just next
        []         -> Nothing

-- | Where @<leader> a@ should jump. An activity-marked window takes
-- priority: pick the first one in the same cyclic scan @next-window -a@
-- uses (indices after the current, then wrapping around to those before
-- it), so repeated presses walk the flagged windows in order. With no
-- activity anywhere, fall back to @last-window@ (the last-active index).
pickActivityTarget
    :: [Int]        -- ^ window indices in ascending order
    -> Int          -- ^ the current window index
    -> Set.Set Int  -- ^ indices carrying an activity flag
    -> Maybe Int    -- ^ the last-active window index, if any
    -> Maybe Int
pickActivityTarget ixs cur flagged mlast =
    case filter (`Set.member` flagged) scan of
        (ix : _) -> Just ix
        []       -> mlast
  where
    curPos = fromMaybe (-1) (List.elemIndex cur ixs)
    (before, after) = splitAt (curPos + 1) ixs
    scan = after <> before

-- Sizing ----------------------------------------------------------------

-- | The resize mode in force for a session's displayed size: the current
-- window's resolved @aggressive-resize@ (or the global setting when the
-- session has no window). 'ActiveClient' follows the most-recently-active
-- client; 'SmallestClient' fits every client.
resizeModeFor :: ServerState -> Session -> Maybe Window -> STM ResizeMode
resizeModeFor st sess mwin =
    resizeModeOf <$> maybe (resolveGlobal st) (resolveForWindow st sess) mwin

-- | The resize mode @aggressive-resize@ selects: on, follow the active
-- client; off, fit the smallest. See 'resizeModeFor'.
resizeModeOf :: Options -> ResizeMode
resizeModeOf opts = if opts.aggressiveResize then ActiveClient else SmallestClient

-- | Recompute a session's effective window size from its clients (honoring
-- @aggressive-resize@) and mark the clients for a full redraw. Pane pty and
-- emulator sizes are not touched here: 'reconcileLoop' pulls them into
-- agreement with the layout off the same dirty tick this bumps, so a session
-- resized here — or a layout changed anywhere else — reconciles uniformly.
applySessionSize :: ServerState -> SessionId -> IO ()
applySessionSize st sid = atomically $ do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        cs <- sessionClients st sid
        stamps <- mapM (\c -> (,) <$> readTVar c.lastActive
                                   <*> readTVar c.size) cs
        lastSz <- readTVar sess.lastSize
        mwin <- currentWindow sess
        mode <- resizeModeFor st sess mwin
        let eff = effectiveWindowSize mode lastSz stamps
        writeTVar sess.lastSize eff
        forM_ cs $ \c -> writeTVar c.needsFull True
        bumpDirty st

-- | A single server-wide task that pulls every pane's pty and emulator size
-- into agreement with the current layout whenever the screen is marked
-- dirty. Because the same 'bumpDirty' that schedules a repaint also wakes
-- this loop, no state change can update the picture without resizing the
-- panes behind it — a zoom cancelled by 'cmdSelectPane', a split, or a
-- client resize all reconcile here. Sole writer of 'pane.size' and sole
-- caller of the resize primitives, so no per-client 'renderLoop' races it.
reconcileLoop :: ServerState -> IO ()
reconcileLoop st = loop (-1)
  where
    loop lastGen = do
        gen <- atomically $ do
            g <- readTVar st.dirty
            check (g /= lastGen)
            pure g
        reconcilePaneSizes st
        atomically (writeTVar st.reconciled gen)
        loop gen

-- | Block until 'reconcileLoop' has resized panes through the current dirty
-- generation, so the caller observes a screen whose children have already
-- been told their size. 'controlLoop' waits here before reporting a command
-- done, giving @select-pane@ and friends a happens-before with the pty
-- resize (@TIOCSWINSZ@ \/ @SIGWINCH@): when the command returns, no child is
-- still holding a stale size. A no-op command reconciles to nothing and
-- returns at once. Depends on a live 'reconcileLoop' to advance
-- 'Hat.Model.reconciled'.
awaitReconciled :: ServerState -> IO ()
awaitReconciled st = do
    target <- readTVarIO st.dirty
    atomically $ do
        done <- readTVar st.reconciled
        check (done >= target)

-- | Resize the pty and emulator of every pane whose stored size lags the
-- layout, waking renderers once the child has been told (see 'reconcileLoop').
-- A pane whose pty has already closed (its child exiting mid-tick) fails only
-- its own resize — logged, not raised — so one dead pane neither aborts the
-- rest nor kills the loop. 'pane.size' is committed only after the child is
-- actually resized, so a failure leaves the pane to retry rather than claiming
-- a size its child never heard about.
reconcilePaneSizes :: ServerState -> IO ()
reconcilePaneSizes st = do
    targets <- atomically (paneSizeTargets st)
    forM_ targets $ \(pane, sz) -> do
        old <- readTVarIO pane.size
        when (old /= sz) $
            handle (\(e :: IOException) ->
                        logEvent st.logger PaneResizeFailed
                            { pane = rawPane pane.id, err = T.pack (show e) }) $ do
                -- Flushed, not just queued: libvterm's resize can abort() the
                -- whole process (a native assertion), and this line is what
                -- names the culprit pane and dimensions afterwards.
                logEvent st.logger PaneResizing
                    { pane = rawPane pane.id
                    , toRows = fromIntegral sz.rows
                    , toCols = fromIntegral sz.cols }
                flushLogger st.logger
                Hat.Term.Pty.resize pane.pty sz
                Emu.resize pane.emulator sz
                atomically $ do
                    writeTVar pane.size sz
                    bumpDirty st

-- | The size the current layout assigns to every live pane across all
-- sessions. See 'reconcilePaneSizes'.
paneSizeTargets :: ServerState -> STM [(Pane, Size)]
paneSizeTargets st = do
    sessions <- Map.elems <$> readTVar st.sessions
    fmap concat $ forM sessions $ \sess -> do
        eff <- readTVar sess.lastSize
        ws <- Map.elems <$> readTVar sess.windows
        fmap concat $ forM ws $ \win -> do
            (rects, _) <- windowArrange (windowArea eff) win
            ps <- readTVar win.panes
            pure [ (p, rectSize rect)
                 | (pidL, rect) <- rects
                 , Just p <- [Map.lookup pidL ps]
                 ]

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }

-- Input ---------------------------------------------------------------------

inputLoop :: ServerState -> Client -> IO ()
inputLoop st client = loop
  where
    loop = do
        m <- recvMessage client.sock
        case m of
            Nothing -> pure ()
            Just (Malformed err) -> logEvent st.logger ProtocolError
                { client = rawClient client.id, err = T.pack err }
            Just (UnknownTag _) -> loop
            Just (Known msg) -> case msg of
                Input bs -> do
                    handleInput st client bs
                    loop
                Resize sz -> do
                    atomically (writeTVar client.size sz)
                    sid <- readTVarIO client.session
                    applySessionSize st sid
                    loop
                Detach -> send client DetachOk
                Command cmds -> do
                    replies <- runCommands st (Just client) cmds
                    forM_ replies $ \case
                        ROutput out -> showToast st client out
                        RErr e -> showToast st client ("error: " <> e)
                    loop
                ClientHello {} -> pure ()

-- | Whether @client@ already holds the largest activity stamp among the
-- clients attached to its session (so it is the one an aggressive-resize
-- window currently follows).
isMostActive :: ServerState -> Client -> STM Bool
isMostActive st client = do
    sid <- readTVar client.session
    cs <- sessionClients st sid
    mine <- readTVar client.lastActive
    others <- mapM (\c -> readTVar c.lastActive)
        (filter (\c -> c.id /= client.id) cs)
    pure (all (< mine) others)

-- | Record activity from a client and, when it becomes the newly
-- most-recently-active one on its session, re-apply the session size so an
-- aggressive-resize window follows it. A no-op resize when nothing aggressive
-- is in play (the smallest-client size is independent of who is active).
noteClientActivity :: ServerState -> Client -> IO ()
noteClientActivity st client = do
    becameActive <- atomically $ do
        already <- isMostActive st client
        markActive st client
        pure (not already)
    when becameActive $ do
        sid <- readTVarIO client.session
        applySessionSize st sid

-- | Whether a key event is delivered to the focused pane's program. Focus
-- in/out reports reach the pane only when @focus-events@ is on and the pane's
-- app enabled focus reporting (?1004); a bare shell never asks, so its focus
-- report is dropped rather than echoed as a stray "^[[I". Every other key is
-- always delivered.
deliversKey :: Options -> Bool -> Key -> Bool
deliversKey opts paneFocusReport k
    | k.name `notElem` ["FocusIn", "FocusOut"] = True
    | otherwise = opts.focusEvents && paneFocusReport

handleInput :: ServerState -> Client -> B.ByteString -> IO ()
handleInput st client bs = do
    noteClientActivity st client
    mpicker <- readTVarIO client.picker
    mprompt <- readTVarIO client.prompt
    case (mpicker, mprompt) of
        (Just pk, _) -> handlePickerInput st client pk (tokenizeKeys bs)
        (_, Just pr) -> handlePromptInput st client pr bs
        _            -> handleKeys st client bs

-- | While a chooser is open it owns every keystroke: navigate/search
-- until Enter (run the item's command and close) or Escape (close).
handlePickerInput
    :: ServerState -> Client -> PickerState -> [Key] -> IO ()
handlePickerInput _ _ _ [] = pure ()
handlePickerInput st client pk0 keys = do
    let step acc k = case acc of
            Picker.PickerStay pk -> Picker.editPicker pk k
            done -> done
        result = List.foldl' step (Picker.PickerStay pk0) keys
    case result of
        Picker.PickerStay pk -> atomically $ do
            writeTVar client.picker (Just pk)
            bumpDirty st
        Picker.PickerCancel -> closePicker st client
        Picker.PickerRun line -> do
            closePicker st client
            replies <- runCommandText st (Just client) line
            forM_ replies $ \case
                ROutput out -> showToast st client out
                RErr e -> showToast st client ("error: " <> e)

closePicker :: ServerState -> Client -> IO ()
closePicker st client = atomically $ do
    writeTVar client.picker Nothing
    bumpDirty st

-- | While the command prompt is open it owns every keystroke: the line
-- editor consumes them until Enter (run and close) or Escape (close).
handlePromptInput
    :: ServerState -> Client -> PromptState -> B.ByteString -> IO ()
handlePromptInput st client pr0 bs = do
    history <- readTVarIO st.cmdHistory
    let step acc k = case acc of
            Prompt.Editing pr -> Prompt.editPrompt history pr k
            done -> done
        result = List.foldl' step (Prompt.Editing pr0) (tokenizeKeys bs)
    case result of
        Prompt.Editing pr -> atomically $ do
            writeTVar client.prompt (Just pr)
            bumpDirty st
        Prompt.Cancel -> atomically $ do
            writeTVar client.prompt Nothing
            bumpDirty st
        Prompt.Submit line -> do
            let templated = not (T.null pr0.template)
                cmd = Prompt.applyTemplate pr0.template line
            atomically $ do
                writeTVar client.prompt Nothing
                -- Templated prompts (rename-window, …) keep the : history
                -- clean; only bare command lines are remembered.
                unless templated $
                    modifyTVar' st.cmdHistory (Prompt.pushHistory line)
                bumpDirty st
            -- A blank submission cancels: never rename a window to "".
            unless (T.null (T.strip line)) $ do
                replies <- runCommandText st (Just client) cmd
                forM_ replies $ \case
                    ROutput out -> showToast st client out
                    RErr e -> showToast st client ("error: " <> e)

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
handleKeys :: ServerState -> Client -> B.ByteString -> IO ()
handleKeys st client bs = do
    opts <- clientOptions st client
    km <- readTVarIO st.keymap
    let loop kst [] = writeIORef client.keyState kst
        loop kst (k0 : rest) = do
            mpane <- clientActivePane st client
            -- A pending vi char search (f/F/t/T) captures this key as its
            -- target, ahead of any keymap lookup.
            searchFed <- maybe (pure False) (feedPendingSearch k0) mpane
            if searchFed then loop kst rest else do
              modeTable <- case mpane of
                Just pane -> fmap (fmap (.copyState.keyTable)) (readTVarIO pane.mode)
                Nothing -> pure Nothing
              k <- reencodeCursor mpane k0
              -- The outer terminal's ?1004 focus reports track whether the
              -- user is watching this client, independent of whether the
              -- pane's app asked for them (the 'keepKey' gate below).
              noteOuterFocus st client k
              keep <- keepKey opts mpane k
              if not keep
                then loop kst rest
                else do
                    let (kst', actions) =
                            routeKeys opts.prefix km modeTable kst [k]
                    forM_ actions $ \case
                        Passthrough raw ->
                            forM_ mpane $ \pane -> Hat.Term.Pty.writePty pane.pty raw
                        RunCommands cmds -> forM_ cmds $ \argv -> do
                            replies <- runArgv st (Just client) argv
                            forM_ replies $ \case
                                ROutput out -> showToast st client out
                                RErr e -> showToast st client ("error: " <> e)
                    loop kst' rest
    st0 <- readIORef client.keyState
    loop st0 (tokenizeKeys bs)
  where
    -- Reads the pane's live focus-reporting mode, then defers the actual
    -- keep/drop decision to the pure 'deliversKey'.
    keepKey opts mpane k
        | k.name `notElem` ["FocusIn", "FocusOut"] = pure True
        | otherwise = do
            report <- maybe (pure False)
                (\pane -> (.focusReport) <$> Emu.modes pane.emulator) mpane
            pure (deliversKey opts report k)
    -- When the pane's copy mode is waiting for a char-search target, this
    -- key IS the target: a single printable char runs the search, anything
    -- else (Escape, Enter, an arrow) cancels it. Returns whether it was
    -- consumed here.
    feedPendingSearch key pane = do
        mmode <- readTVarIO pane.mode
        case mmode of
            Just s | Just _ <- s.copyState.pendingSearch -> do
                if T.length key.name == 1
                    then runCopyModeCommand st pane "apply-search" [key.name]
                    else runCopyModeCommand st pane "cancel-search" []
                pure True
            _ -> pure False

-- | Cursor keys ('\ESC[A' vs '\ESCOA') depend on the pane's DECCKM mode,
-- so re-encode them via the pane's emulator instead of forwarding the raw
-- bytes the client's terminal happened to send.
reencodeCursor :: Maybe Pane -> Key -> IO Key
reencodeCursor mpane key = case (mpane, cursorKeyOf key.name) of
    (Just pane, Just ck) -> do
        enc <- Emu.encodeKey pane.emulator ck
        pure Key { name = key.name, raw = enc }
    _ -> pure key
  where
    cursorKeyOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        "Home"  -> Just Emu.CursorHome
        "End"   -> Just Emu.CursorEnd
        _       -> Nothing

showToast :: ServerState -> Client -> Text -> IO ()
showToast st client t = do
    atomically $ do
        writeTVar client.toast (Just t)
        bumpDirty st
    displayMs <- (.displayTime) <$> clientOptions st client
    void . forkIO $ do
        threadDelay (displayMs * 1000)
        atomically $ do
            cur <- readTVar client.toast
            when (cur == Just t) $ do
                writeTVar client.toast Nothing
                bumpDirty st

clientActivePane :: ServerState -> Client -> IO (Maybe Pane)
clientActivePane st client = atomically $ do
    mv <- clientView st client
    case mv of
        Nothing -> pure Nothing
        Just (_, win) -> activePane win

-- | The session and current window of a client, if both exist.
clientView :: ServerState -> Client -> STM (Maybe (Session, Window))
clientView st client = do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            mwin <- currentWindow sess
            pure ((,) sess <$> mwin)

-- The command engine ---------------------------------------------------------

data Reply = ROutput Text | RErr Text

runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = case parseCommandLine input of
    Left err -> pure [RErr err]
    Right cmds -> runCommands st mclient cmds

runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
runCommands st mclient cmds = concat <$> mapM (runArgv st mclient) cmds

runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
runArgv _ _ [] = pure []
runArgv st mclient (name : args) = do
    -- Attribute every command to its issuing client, so a duplicate or
    -- unexpected control command (e.g. a second restart-server nobody typed)
    -- is traceable to its source.
    logEvent st.logger CommandRun
        { client = maybe (-1) (\c -> rawClient c.id) mclient
        , command = T.unwords (name : args) }
    case Map.lookup name commandTable of
        Nothing -> pure [RErr ("unknown command: " <> name)]
        Just impl -> impl st mclient args
            `catch` \(e :: SomeException) ->
                pure [RErr (name <> ": " <> T.pack (show e))]

type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList $ concatMap expand
    [ (["bind-key", "bind"], cmdBind)
    , (["unbind-key", "unbind"], cmdUnbind)
    , (["set-option", "set"], cmdSet DefaultSession)
    , (["set-window-option", "setw"], cmdSet DefaultWindow)
    , (["show-options", "show", "show-option"], cmdShow)
    , (["source-file", "source"], cmdSourceFile)
    , (["new-window", "neww"], cmdNewWindow)
    , (["select-window", "selectw"], cmdSelectWindow)
    , (["next-window", "next"], cmdNextWindow)
    , (["previous-window", "prev"], cmdPrevWindow)
    , (["last-window", "last"], cmdLastWindow)
    , (["activity-window"], cmdActivityWindow)
    , (["kill-window", "killw"], cmdKillWindow)
    , (["rename-window", "renamew"], cmdRenameWindow)
    , (["move-window", "movew"], cmdMoveWindow)
    , (["split-window", "splitw"], cmdSplitWindow)
    , (["select-pane", "selectp"], cmdSelectPane)
    , (["kill-pane", "killp"], cmdKillPane)
    , (["swap-pane", "swapp"], cmdSwapPane)
    , (["clear-history", "clearhist"], cmdClearHistory)
    , (["break-pane", "breakp"], cmdBreakPane)
    , (["join-pane", "joinp"], cmdJoinPane)
    , (["select-layout", "selectl"], cmdSelectLayout)
    , (["next-layout", "nextl"], cmdNextLayout)
    , (["previous-layout", "prevl"], cmdPreviousLayout)
    , (["resize-pane", "resizep"], cmdResizePane)
    , (["last-pane", "lastp"], cmdLastPane)
    , (["detach-client", "detach"], cmdDetachClient)
    , (["send-prefix"], cmdSendPrefix)
    , (["send-keys", "send"], cmdSendKeys)
    , (["copy-mode"], cmdCopyMode)
    , (["command-prompt"], cmdCommandPrompt)
    , (["choose-tree"], cmdChooseTree)
    , (["choose-window", "choosew"], cmdChooseWindow)
    , (["show-buffer", "showb"], cmdShowBuffer)
    , (["set-buffer", "setb"], cmdSetBuffer)
    , (["list-buffers", "lsb"], cmdListBuffers)
    , (["delete-buffer", "deleteb"], cmdDeleteBuffer)
    , (["save-buffer", "saveb"], cmdSaveBuffer)
    , (["paste-buffer", "pasteb"], cmdPasteBuffer)
    , (["pipe-pane", "pipep"], cmdPipePane)
    , (["new-session", "new"], cmdNewSession)
    , (["attach-session", "attach"], cmdAttachSession)
    , (["kill-session"], cmdKillSession)
    , (["has-session", "has"], cmdHasSession)
    , (["start-server", "start"], cmdStartServer)
    , (["rename-session", "rename"], cmdRenameSession)
    , (["list-sessions", "ls"], cmdListSessions)
    , (["list-windows", "lsw"], cmdListWindows)
    , (["list-panes", "lsp"], cmdListPanes)
    , (["capture-pane", "capturep"], cmdCapturePane)
    , (["resize-window", "resizew"], cmdResizeWindow)
    , (["switch-client", "switchc"], cmdSwitchClient)
    , (["kill-server"], cmdKillServer)
    , (["restart-server"], cmdRestartServer)
    , (["display-message", "display"], cmdDisplayMessage)
    , (["run-shell", "run"], cmdRunShell)
    , (["if-shell", "if"], cmdIfShell)
    ]
  where
    expand (names, impl) = [(n, impl) | n <- names]

-- getopt-style flag parser: @spec@ lists the letters that take a
-- value. Bundled forms work like tmux: @-dsfoo@ is @-d -s foo@.
-- Returns (value flags as ("-s", value), boolean flags as "-d",
-- positional args).
parseArgs :: [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgs spec = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        ("--" : rest) -> (opts, flags, rest)   -- end-of-flags separator
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a
            , not (T.null bundle)
            , not (isNumber a) ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    scanBundle bundle rest = case T.uncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                let val = fromMaybe more (T.stripPrefix "=" more)
                in case (T.null val, rest) of
                    (False, _) -> ([(dash c, val)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, "")], [], [])
            | otherwise ->
                let (opts', flags', rest') = scanBundle more rest
                in (opts', dash c : flags', rest')
    dash c = T.pack ['-', c]
    isNumber a = case TR.signed TR.decimal a of
        Right (_ :: Int, restT) -> T.null restT
        Left _ -> False

targetSession :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Session)
targetSession st mclient mtarget = atomically $ do
    sessions <- readTVar st.sessions
    case mtarget of
        Just t -> do
            let stripped = fromMaybe t (T.stripSuffix ":" t)
            found <- forM (Map.elems sessions) $ \sess -> do
                nm <- readTVar sess.name
                pure $ if nm == stripped
                    || tshow (rawSession sess.id) == stripped
                    || ("$" <> tshow (rawSession sess.id)) == stripped
                    then Just sess else Nothing
            pure (foldr (\m acc -> maybe acc Just m) Nothing found)
        Nothing -> case mclient of
            Just client -> do
                sid <- readTVar client.session
                case Map.lookup sid sessions of
                    Just sess -> pure (Just sess)
                    Nothing -> pure (snd <$> Map.lookupMax sessions)
            Nothing -> pure (snd <$> Map.lookupMax sessions)

withTargetSession
    :: ServerState -> Maybe Client -> Maybe Text
    -> (Session -> IO [Reply]) -> IO [Reply]
withTargetSession st mclient mtarget body = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure [RErr "no such session"]
        Just sess -> body sess

withCurrentWindow
    :: ServerState -> Maybe Client
    -> (Session -> Window -> IO [Reply]) -> IO [Reply]
withCurrentWindow st mclient body = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    view <- case mv of
        Just v -> pure (Just v)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mwin <- atomically (currentWindow sess)
                    pure ((,) sess <$> mwin)
    case view of
        Nothing -> pure [RErr "no current window"]
        Just (sess, win) -> body sess win

-- Command implementations.

cmdBind :: CommandImpl
cmdBind st _ args = do
    let (opts, flags, pos) = parseArgs "TN" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        (keyName : rest)
            | Just key <- parseKeyName keyName
            , not (null rest) -> do
                let cmds = splitBinding rest
                atomically $ modifyTVar' st.keymap $
                    Map.insertWith Map.union table
                        (Map.singleton key.name cmds)
                pure []
        (keyName : _) ->
            pure [RErr ("bind: bad key or command: " <> keyName)]
        _ -> pure [RErr "usage: bind [-n] [-T table] key command..."]

-- A binding's command part: one brace block to re-parse, or argv split
-- on ";" tokens (from escaped semicolons).
splitBinding :: [Text] -> [[Text]]
splitBinding = \case
    [block] | T.any (\c -> c == ' ' || c == ';' || c == '\n') block ->
        case parseConfig block of
            Right cmds -> cmds
            Left _ -> [[block]]
    rest -> filter (not . null) (splitOnSemis rest)
  where
    splitOnSemis xs = case break (== ";") xs of
        (before, []) -> [before]
        (before, _ : after) -> before : splitOnSemis after

cmdUnbind :: CommandImpl
cmdUnbind st _ args = do
    let (opts, flags, pos) = parseArgs "T" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        [keyName] | Just key <- parseKeyName keyName -> do
            atomically $ modifyTVar' st.keymap $
                Map.adjust (Map.delete key.name) table
            pure []
        _ -> pure [RErr "usage: unbind [-n] [-T table] key"]

-- | Whether @set@\/@set-option@ (session) or @setw@\/@set-window-option@
-- (window) invoked the set: the default scope when no @-g@\/@-s@\/@-w@ picks
-- one. See 'cmdSet'.
data SetDefault = DefaultSession | DefaultWindow
    deriving (Eq, Show)

-- | The overlay table a @set-option@ writes into. See 'chooseScope'.
data SetScope
    = SetServer
    | SetGlobalSession
    | SetGlobalWindow
    | SetLocalSession
    | SetLocalWindow
    deriving (Eq, Show)

-- | Route a set to its scope from the command's default, its flags, and the
-- option's class. @-g@ is lenient (it picks the session- or window-global
-- table by the option's class, never erroring, so a real @~/.tmux.conf@'s
-- @set -g mode-keys@ / @set -gw display-time@ both load). An /explicit/ local
-- window scope (@setw@ or @-w@ without @-g@) or @-s@ that contradicts the
-- option's class fails loud, matching tmux's @setw prefix@ rejection.
chooseScope :: SetDefault -> [Text] -> OptionName -> Either Text SetScope
chooseScope def flags name
    | hasS = SetServer <$ validateScope ServerOption name
    | hasG = Right $ case cls of
        WindowOption -> SetGlobalWindow
        ServerOption -> SetServer
        SessionOption -> SetGlobalSession
    | wantWindow = SetLocalWindow <$ validateScope WindowOption name
    | otherwise = Right $ case cls of
        WindowOption -> SetLocalWindow
        ServerOption -> SetServer
        SessionOption -> SetLocalSession
  where
    cls = optionScopeClass name
    hasS = "-s" `elem` flags
    hasG = "-g" `elem` flags
    wantWindow = "-w" `elem` flags || def == DefaultWindow

cmdSet :: SetDefault -> CommandImpl
cmdSet def st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        mode = if "-a" `elem` flags then Append else Assign
        mtarget = lookup "-t" opts
    case pos of
        (nameT : rest) -> do
            curOpts <- currentResolved st mclient mtarget
            case setOptionEntry mode curOpts nameT (T.unwords rest) of
                Left err -> pure [RErr err]
                Right (n, v) -> case chooseScope def flags n of
                    Left err -> pure [RErr err]
                    Right scope -> do
                        etv <- scopeTargetVar st mclient mtarget scope
                        case etv of
                            Left err -> pure [RErr err]
                            Right deltaVar -> writeScoped st deltaVar n v
        [] -> pure [RErr "usage: set [-gsw] [-t target] option value"]

-- | Insert a resolved entry into its scope's overlay, refresh the cached
-- global resolution ('ServerState.options'), and push a changed
-- @history-limit@ into each session's open panes.
writeScoped
    :: ServerState -> TVar OptionsDelta -> OptionName -> OptionValue
    -> IO [Reply]
writeScoped st deltaVar n v = do
    atomically $ do
        modifyTVar' deltaVar (insertDelta n v)
        refreshGlobalOptions st
        bumpDirty st
    when (n == OptHistoryLimit) (applyHistoryLimit st)
    pure []

-- | The options in effect for the target session (or the global chain when
-- there is none): the base an @-a@ append concatenates onto.
currentResolved :: ServerState -> Maybe Client -> Maybe Text -> IO Options
currentResolved st mclient mtarget = do
    msess <- targetSession st mclient mtarget
    atomically $ maybe (resolveGlobal st) (resolveForSession st) msess

-- | The options in effect for a client's current session, so a bare @set@
-- there is honored.
clientOptions :: ServerState -> Client -> IO Options
clientOptions st client = atomically $ do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    maybe (resolveGlobal st) (resolveForSession st) msess

-- | The overlay table a scope writes into, resolving the target session or
-- its current window; a missing target fails loud.
scopeTargetVar
    :: ServerState -> Maybe Client -> Maybe Text -> SetScope
    -> IO (Either Text (TVar OptionsDelta))
scopeTargetVar st mclient mtarget = \case
    SetServer -> pure (Right st.serverOptions)
    SetGlobalSession -> pure (Right st.globalSessionOptions)
    SetGlobalWindow -> pure (Right st.globalWindowOptions)
    SetLocalSession -> do
        msess <- targetSession st mclient mtarget
        pure (maybe (Left "no such session") (Right . (.options)) msess)
    SetLocalWindow -> do
        mwin <- targetCurrentWindow st mclient mtarget
        pure (maybe (Left "no current window") (Right . (.options)) mwin)

-- | The current window of the target session (or the client's).
targetCurrentWindow
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Window)
targetCurrentWindow st mclient mtarget = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure Nothing
        Just sess -> atomically (currentWindow sess)

-- | Recompute the cached global option resolution. Runs in the same
-- transaction as every global-scope delta write so the cache never drifts.
refreshGlobalOptions :: ServerState -> STM ()
refreshGlobalOptions st = writeTVar st.options =<< resolveGlobal st

-- | Push a changed @history-limit@ into every open pane's emulator so the new
-- cap governs existing panes' scrollback immediately, not just panes created
-- afterward. Each session resolves its own limit, so a session-scoped set
-- reaches only that session's panes.
applyHistoryLimit :: ServerState -> IO ()
applyHistoryLimit st = do
    sessMap <- readTVarIO st.sessions
    forM_ (Map.elems sessMap) $ \sess -> do
        limit <- (.historyLimit) <$> atomically (resolveForSession st sess)
        winMap <- readTVarIO sess.windows
        forM_ (Map.elems winMap) $ \win -> do
            paneMap <- readTVarIO win.panes
            forM_ (Map.elems paneMap) $ \pane ->
                Emu.setScrollbackLimit pane.emulator limit

cmdShow :: CommandImpl
cmdShow st _ args = do
    let (_, flags, pos) = parseArgs "t" args
        valueOnly = "-v" `elem` flags
        quiet = "-q" `elem` flags
    opts <- readTVarIO st.options
    case pos of
        [name] -> case lookupOption opts name of
            Just v -> pure [ROutput (if valueOnly then v else name <> " " <> v)]
            Nothing
                | quiet -> pure []
                | otherwise -> pure [RErr ("unknown option: " <> name)]
        _ -> pure [RErr "usage: show-options [-gsvq] name"]

lookupOption :: Options -> Text -> Maybe Text
lookupOption opts name = case name of
    "prefix" -> Just opts.prefix
    "base-index" -> Just (tshow opts.baseIndex)
    "pane-base-index" -> Just (tshow opts.paneBaseIndex)
    "history-limit" -> Just (tshow opts.historyLimit)
    "default-terminal" -> Just opts.defaultTerminal
    "word-separators" -> Just opts.wordSeparators
    "status-position" -> Just (case opts.statusPosition of
        StatusTop -> "top"; StatusBottom -> "bottom")
    "mode-keys" -> Just (case opts.modeKeys of
        KeysVi -> "vi"; KeysEmacs -> "emacs")
    "status-left" -> Just opts.statusLeft
    "status-left-length" -> Just (tshow opts.statusLeftLength)
    "status-right" -> Just opts.statusRight
    "status-right-length" -> Just (tshow opts.statusRightLength)
    "window-status-format" -> Just opts.windowStatusFormat
    "window-status-current-format" -> Just opts.windowStatusCurrentFormat
    "automatic-rename" -> Just (if opts.automaticRename then "on" else "off")
    "automatic-rename-format" -> Just opts.automaticRenameFormat
    _
        | "@" `T.isPrefixOf` name -> Map.lookup name opts.user
        | otherwise -> Nothing

-- | Whether a @set-option@ replaces the option or concatenates onto it.
data SetMode
    = Assign  -- ^ replace the current value outright
    | Append  -- ^ tmux's @-a@: append onto a string option's current value
    deriving (Eq)

-- | Apply a @set-option@. For string-valued options 'Append' concatenates
-- onto the current value (used to build up @status-right@ across several
-- lines). Unknown non-@\@@ options are rejected so a config never looks
-- supported when its behavior is not yet implemented.
setOption :: SetMode -> Options -> Text -> Text -> Either Text Options
setOption mode opts name value = do
    (n, v) <- setOptionEntry mode opts name value
    pure (applyEntry n v opts)

-- | Parse and validate a @set-option@ into the single scoped entry it writes.
-- For string-valued options 'Append' concatenates onto the current value (used
-- to build up @status-right@ across several lines). Unknown non-@\@@ options
-- are rejected so a config never looks supported when its behavior is not yet
-- implemented.
setOptionEntry
    :: SetMode -> Options -> Text -> Text
    -> Either Text (OptionName, OptionValue)
setOptionEntry mode opts name value = case name of
    "prefix" -> case parseKeyName value of
        Just k -> Right (OptPrefix, OVText k.name)
        Nothing -> Left ("bad prefix key: " <> value)
    "base-index" -> withInt OptBaseIndex
    "pane-base-index" -> withInt OptPaneBaseIndex
    "history-limit" -> withInt OptHistoryLimit
    "default-terminal" -> Right (OptDefaultTerminal, OVText value)
    "word-separators" -> Right (OptWordSeparators, OVText value)
    "status-position" -> case value of
        "top" -> Right (OptStatusPosition, OVStatusPosition StatusTop)
        "bottom" -> Right (OptStatusPosition, OVStatusPosition StatusBottom)
        _ -> Left "status-position: top or bottom"
    "mode-keys" -> case value of
        "vi" -> Right (OptModeKeys, OVModeKeys KeysVi)
        "emacs" -> Right (OptModeKeys, OVModeKeys KeysEmacs)
        _ -> Left "mode-keys: vi or emacs"
    "status-left" ->
        Right (OptStatusLeft, OVText (withAppend opts.statusLeft))
    "status-left-length" -> withInt OptStatusLeftLength
    "status-right" ->
        Right (OptStatusRight, OVText (withAppend opts.statusRight))
    "status-right-length" -> withInt OptStatusRightLength
    "window-status-format" ->
        Right (OptWindowStatusFormat,
            OVText (withAppend opts.windowStatusFormat))
    "window-status-current-format" ->
        Right (OptWindowStatusCurrentFormat,
            OVText (withAppend opts.windowStatusCurrentFormat))
    "status-style" -> Right (OptStatusStyle, OVStyle (parseStyle value))
    "window-status-style" ->
        Right (OptWindowStatusStyle, OVStyle (parseStyle value))
    "window-status-current-style" ->
        Right (OptWindowStatusCurrentStyle, OVStyle (parseStyle value))
    "window-status-bell-style" ->
        Right (OptWindowStatusBellStyle, OVStyle (parseStyle value))
    "pane-border-style" ->
        Right (OptPaneBorderStyle, OVStyle (parseStyle value))
    "pane-active-border-style" ->
        Right (OptPaneActiveBorderStyle, OVStyle (parseStyle value))
    "mode-style" -> Right (OptModeStyle, OVStyle (parseStyle value))
    "pane-border-lines" -> case value of
        "single" -> Right (OptPaneBorderLines, OVBorderLines BorderSingle)
        "heavy"  -> Right (OptPaneBorderLines, OVBorderLines BorderHeavy)
        "double" -> Right (OptPaneBorderLines, OVBorderLines BorderDouble)
        "simple" -> Right (OptPaneBorderLines, OVBorderLines BorderSimple)
        _ -> Left "pane-border-lines: single, heavy, double, or simple"
    "pane-border-indicators" -> case value of
        "off"    -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsOff)
        "colour" -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsColour)
        "color"  -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsColour)
        "arrows" -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsArrows)
        "both"   -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsBoth)
        _ -> Left "pane-border-indicators: off, colour, arrows, or both"
    "set-titles" -> withOnOff OptSetTitles
    "escape-time" -> withInt OptEscapeTime
    "display-time" -> withInt OptDisplayTime
    "focus-events" -> withOnOff OptFocusEvents
    "aggressive-resize" -> withOnOff OptAggressiveResize
    "monitor-activity" -> withOnOff OptMonitorActivity
    "automatic-rename" -> withOnOff OptAutomaticRename
    "automatic-rename-format" -> Right (OptAutomaticRenameFormat, OVText value)
    "update-environment" ->
        Right (OptUpdateEnvironment, OVTextList (T.words value))
    "main-pane-width" -> withInt OptMainPaneWidth
    "main-pane-height" -> withInt OptMainPaneHeight
    _
        | "@" `T.isPrefixOf` name -> Right (OptUser name, OVText value)
        | otherwise -> Left ("unimplemented option: " <> name)
  where
    withInt n = case TR.decimal value of
        Right (m, restT) | T.null restT -> Right (n, OVInt m)
        _ -> Left (name <> ": not a number: " <> value)
    withOnOff n = case value of
        "on"  -> Right (n, OVBool True)
        "off" -> Right (n, OVBool False)
        _ -> Left (name <> ": on or off")
    withAppend old = case mode of
        Append -> old <> value
        Assign -> value

cmdSourceFile :: CommandImpl
cmdSourceFile st mclient args = case pos of
    [path] -> do
        p <- expandTilde (T.unpack path)
        exists <- doesFileExist p
        if not exists
            then if "-q" `elem` flags
                then pure []
                else pure [RErr ("no such file: " <> path)]
            else do
                contents <- readConfigUtf8 p
                case parseConfig contents of
                    Left err -> pure [RErr err]
                    Right cmds
                        -- -n: check syntax, do not execute
                        | "-n" `elem` flags -> pure []
                        | otherwise ->
                            concat <$> mapM (runArgv st mclient) cmds
    _ -> pure [RErr "usage: source-file path"]
  where
    (_, flags, pos) = parseArgs "" args

cmdNewWindow :: CommandImpl
cmdNewWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "nct" args
    withTargetSession st mclient Nothing $ \sess -> do
        eff <- readTVarIO sess.lastSize
        srvOpts <- readTVarIO st.options
        environ <- readTVarIO sess.environ
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
            environ (windowArea eff)
        forM_ (lookup "-n" opts) $ \nm -> atomically $ do
            writeTVar win.name nm
            writeTVar win.autoRename False
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let requested = do
                    t <- lookup "-t" opts
                    case TR.decimal t of
                        Right (n, restT) | T.null restT -> Just n
                        _ -> Nothing
                nextFreeFrom n = until (\i -> not (Map.member i ws)) (+ 1) n
                ix = case requested of
                    Just n
                        | "-a" `elem` flags -> nextFreeFrom (n + 1)
                        | otherwise -> n
                    Nothing
                        | "-a" `elem` flags -> nextFreeFrom (cur + 1)
                        | otherwise -> nextFreeFrom srvOpts.baseIndex
                ix' = if Map.member ix ws then nextFreeFrom srvOpts.baseIndex else ix
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
    mres <- resolveWindowTarget st mclient target
    case mres of
        Nothing -> pure [RErr "usage: select-window -t index"]
        Just (sess, ix) -> do
            atomically (switchTo st sess ix)
            pure []

-- Accepts @[session][:window]@ where session may be a name or @$id@ and
-- window may be a number, @$@ for the last window, or omitted to mean
-- the session's current window. A bare token without @:@ is a window
-- spec in the current session (or a session spec if it starts with @$@).
resolveWindowTarget
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe (Session, Int))
resolveWindowTarget st mclient mtarget = case mtarget of
    Nothing -> do
        msess <- targetSession st mclient Nothing
        traverse currentPair msess
    Just t
        | ":" `T.isInfixOf` t -> withColon t
        | "$" `T.isPrefixOf` t -> do
            msess <- targetSession st mclient (Just t)
            traverse currentPair msess
        | otherwise -> do  -- bare window spec in current session
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mix <- parseWinIx sess t
                    pure $ (,) sess <$> mix
  where
    withColon t =
        let (s, rest) = T.break (== ':') t
            w = T.drop 1 rest
        in do
            msess <- targetSession st mclient
                (if T.null s then Nothing else Just s)
            case msess of
                Nothing -> pure Nothing
                Just sess
                    | T.null w -> Just <$> currentPair sess
                    | otherwise -> do
                        mix <- parseWinIx sess w
                        pure $ (,) sess <$> mix
    currentPair sess = (,) sess <$> readTVarIO sess.currentIx
    parseWinIx sess "$" = do
        ws <- readTVarIO sess.windows
        pure (fst <$> Map.lookupMax ws)
    parseWinIx _ w = pure $ case TR.decimal w of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

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

-- Only @-p@ (print to stdout, plain text) is supported so far; escape,
-- range, and hyperlink flags are ignored — enough for the light-touch
-- @capturep -p@ uses in upstream tests.
cmdCapturePane :: CommandImpl
cmdCapturePane st mclient _ = do
    withCurrentWindow st mclient $ \_ win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure []
            Just pane -> do
                scr <- Emu.snapshot pane.emulator
                let rows = V.toList scr.cells
                    rowText r = T.stripEnd . T.concat
                        $ [ c.text | c <- V.toList r ]
                    body = T.intercalate "\n" (map rowText rows)
                pure [ROutput body]

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
cmdKillWindow st mclient _ =
    withCurrentWindow st mclient $ \sess win -> do
        ps <- readTVarIO win.panes
        killPaneLocs st [(sess.id, win, p) | p <- Map.elems ps]
        pure []

cmdRenameWindow :: CommandImpl
cmdRenameWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> do
            mres <- resolveWindowTarget st mclient (lookup "-t" opts)
            case mres of
                Just (sess, ix) -> do
                    ws <- readTVarIO sess.windows
                    forM_ (Map.lookup ix ws) $ \win ->
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
                Nothing -> pure [RErr "no such window"]
        _ -> pure [RErr "usage: rename-window [-t target] name"]

cmdSplitWindow :: CommandImpl
cmdSplitWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "ctlp" args
        orient
            | "-h" `elem` flags = LeftRight
            | otherwise = TopBottom
        placement = if "-b" `elem` flags then Before else After
        -- @-f@: split spans the whole window, not just the active pane.
        full = "-f" `elem` flags
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure [RErr "no active pane"]
            Just active -> do
                eff <- readTVarIO sess.lastSize
                (rects, _) <- atomically (windowArrange (windowArea eff) win)
                let mrect = List.lookup active.id rects
                    wholeRect = sizeRect (windowArea eff)
                    fitRect = if full then Just wholeRect else mrect
                    fits = case (orient, fitRect) of
                        (LeftRight, Just r) -> r.endCol - r.startCol >= 5
                        (TopBottom, Just r) -> r.endRow - r.startRow >= 5
                        _ -> False
                if not fits
                    then pure [RErr "create pane failed: pane too small"]
                    else do
                        pid <- PaneId <$> atomically (freshId st.nextPane)
                        dir <- case lookup "-c" opts of
                            Just d -> do
                                env <- sessionFormatEnv st sess
                                T.unpack <$> expandFormat st env d
                            Nothing -> paneCurrentPath active
                        environ <- readTVarIO sess.environ
                        let shellCmd = maybe "/bin/sh" T.unpack
                                (List.lookup "SHELL" environ)
                        pane <- spawnPane st pid sess.id shellCmd (shellStart mrun)
                            dir environ (windowArea eff)
                        atomically $ do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            modifyTVar' win.layout $ if full
                                then splitFull orient placement pane.id
                                else splitLeaf active.id orient placement pane.id
                            lastA <- readTVar win.activeId
                            writeTVar win.lastActive (Just lastA)
                            writeTVar win.activeId pane.id
                            writeTVar win.zoomed Nothing
                            bumpDirty st
                        startPaneReader st sess.id win pane
                        applySessionSize st sess.id
                        pure []

cmdSelectPane :: CommandImpl
cmdSelectPane st mclient args = do
    let (opts, flags, _) = parseArgs "tT" args
        mdir
            | "-L" `elem` flags = Just DirLeft
            | "-R" `elem` flags = Just DirRight
            | "-U" `elem` flags = Just DirUp
            | "-D" `elem` flags = Just DirDown
            | otherwise = Nothing
        -- The @-t@ pane-index tail: @:.+N@ / @:.-N@ cycle by N (default 1)
        -- and @:.N@ (or a bare number) selects an absolute index. See
        -- 'parsePaneIndex'\/'resolvePaneIndex'.
        mPaneIndex = lookup "-t" opts >>= parsePaneIndex
    case mdir of
        Nothing
            | "-M" `elem` flags -> do
                atomically $ writeTVar st.markedPane Nothing >> bumpDirty st
                pure []
            | "-m" `elem` flags -> do
                mp <- targetPane st mclient (lookup "-t" opts)
                forM_ mp $ \pane -> atomically $
                    writeTVar st.markedPane (Just pane.id) >> bumpDirty st
                pure []
            | "-l" `elem` flags -> cmdLastPane st mclient []
            | Just idx <- mPaneIndex ->
                withCurrentWindow st mclient $ \_ win -> do
                    atomically $ do
                        -- Relative cycling walks layout order; an absolute
                        -- index counts panes in window (creation) order.
                        order <- case idx of
                            IndexRelative _ _ -> layoutPanes <$> readTVar win.layout
                            IndexAbsolute _   -> Map.keys <$> readTVar win.panes
                        active <- readTVar win.activeId
                        forM_ (resolvePaneIndex idx order active) $ \next ->
                            when (next /= active) $ do
                                writeTVar win.lastActive (Just active)
                                writeTVar win.activeId next
                                bumpDirty st
                    pure []
            | otherwise -> pure [RErr "usage: select-pane -L|-R|-U|-D|-l|-t index|:.[+-][N]"]
        Just dir -> withCurrentWindow st mclient $ \sess win -> do
            atomically $ do
                eff <- readTVar sess.lastSize
                lay <- readTVar win.layout
                active <- readTVar win.activeId
                forM_ (directionalTarget (windowArea eff) lay active dir) $ \next -> do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId next
                    -- Leaving a zoomed pane cancels the zoom (bug 5).
                    writeTVar win.zoomed Nothing
                    bumpDirty st
            pure []

cmdLastPane :: CommandImpl
cmdLastPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            mlast <- readTVar win.lastActive
            ps <- readTVar win.panes
            forM_ mlast $ \lastP -> when (Map.member lastP ps) $ do
                cur <- readTVar win.activeId
                writeTVar win.lastActive (Just cur)
                writeTVar win.activeId lastP
                bumpDirty st
        pure []

-- | Kill the target pane: detach it from the model first so the reflow is
-- synchronous with the command, then reap the child behind ('killPaneLocs').
cmdKillPane :: CommandImpl
cmdKillPane st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> do
        mloc <- atomically (locatePane st pane.id)
        forM_ mloc $ \(sid, win) -> killPaneLocs st [(sid, win, pane)]
    pure []

-- | The session and window a live pane belongs to. See 'cmdKillPane'.
locatePane :: ServerState -> PaneId -> STM (Maybe (SessionId, Window))
locatePane st pid = do
    sessions <- readTVar st.sessions
    hits <- forM (Map.toList sessions) $ \(sid, sess) -> do
        ws <- Map.elems <$> readTVar sess.windows
        winHits <- forM ws $ \win -> do
            ps <- readTVar win.panes
            pure [(sid, win) | Map.member pid ps]
        pure (concat winHits)
    pure (listToMaybe (concat hits))

-- | @swap-pane [-s src] [-t dst] [-U|-D] [-d]@: exchange two panes'
-- positions. @src@ defaults to the active pane; without @-d@ the active
-- pane follows to @dst@'s slot, so the config's @splitw … \; swapp -t !
-- \; killp -t !@ edge-move idiom lands the content and kills the emptied
-- slot.
cmdSwapPane :: CommandImpl
cmdSwapPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        keepActive = "-d" `elem` flags
    withCurrentWindow st mclient $ \sess win -> do
        msrc <- targetPane st mclient (lookup "-s" opts)
        mdst <- case lookup "-t" opts of
            Just t -> targetPane st mclient (Just t)
            Nothing
                | "-U" `elem` flags -> siblingPane st win (-1)
                | "-D" `elem` flags -> siblingPane st win 1
                | otherwise -> pure Nothing
        case (msrc, mdst) of
            (Just src, Just dst) | src.id /= dst.id -> do
                atomically $ do
                    ps <- readTVar win.panes
                    when (Map.member src.id ps && Map.member dst.id ps) $ do
                        modifyTVar' win.layout (swapLeaves src.id dst.id)
                        unless keepActive $ do
                            writeTVar win.lastActive (Just src.id)
                            writeTVar win.activeId dst.id
                        writeTVar win.zoomed Nothing
                        bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure []

-- | The pane @step@ positions from the active one in layout order.
siblingPane :: ServerState -> Window -> Int -> IO (Maybe Pane)
siblingPane _ win step = atomically $ do
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    active <- readTVar win.activeId
    let order = layoutPanes lay
    pure $ case List.elemIndex active order of
        Just i | not (null order) ->
            let pid = order !! ((i + step + length order) `mod` length order)
            in Map.lookup pid ps
        _ -> Nothing

-- | @clear-history [-t target]@: drop a pane's scrollback.
cmdClearHistory :: CommandImpl
cmdClearHistory st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> do
        Emu.clearScrollback pane.emulator
        atomically (bumpDirty st)
    pure []

-- | Detach a pane from whichever window holds it, collapsing the layout
-- and dropping the window if it becomes empty. The pane's pty keeps
-- running — this only re-parents it, backing @break-pane@/@join-pane@.
removePaneFromTree :: ServerState -> PaneId -> STM ()
removePaneFromTree st pid = do
    sessions <- readTVar st.sessions
    forM_ (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        forM_ (Map.toList ws) $ \(ix, win) -> do
            ps <- readTVar win.panes
            when (Map.member pid ps) $ do
                writeTVar win.panes (Map.delete pid ps)
                mz <- readTVar win.zoomed
                when (mz == Just pid) $ writeTVar win.zoomed Nothing
                lay <- readTVar win.layout
                case removeLeaf pid lay of
                    Just lay' -> do
                        writeTVar win.layout lay'
                        active <- readTVar win.activeId
                        when (active == pid) $ case layoutPanes lay' of
                            (n : _) -> writeTVar win.activeId n
                            [] -> pure ()
                    Nothing -> do
                        modifyTVar' sess.windows (Map.delete ix)
                        cur <- readTVar sess.currentIx
                        when (cur == ix) $ do
                            ws' <- readTVar sess.windows
                            forM_ (Map.lookupMin ws') $ \(i, _) ->
                                writeTVar sess.currentIx i
                bumpDirty st

-- | Build a fresh single-pane window around an already-running pane.
wrapPaneInWindow :: ServerState -> Pane -> IO Window
wrapPaneInWindow st pane = do
    wid <- atomically (freshId st.nextWindow)
    name <- paneCommandName pane
    nameVar <- newTVarIO name
    layoutVar <- newTVarIO (Leaf pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    lastActiveVar <- newTVarIO Nothing
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar <- newTVarIO emptyDelta
    pure Window
        { id = WindowId wid
        , name = nameVar
        , layout = layoutVar
        , layoutName = layoutNameVar
        , panes = panesVar
        , activeId = activeVar
        , lastActive = lastActiveVar
        , bellFlag = bellVar
        , activity = activityVar
        , zoomed = zoomVar
        , autoRename = autoRenameVar
        , options = optionsVar
        }

-- | A pane's foreground program (from @/proc@) as a display name:
-- normalized by 'commandName', so a NixOS-wrapped @vim@ shows as @vim@
-- everywhere (window titles, @#{pane_current_command}@, the persisted
-- tree). Falls back to @sh@.
paneCommandName :: Pane -> IO Text
paneCommandName pane = do
    mfg <- Hat.Term.Pty.foregroundCommand pane.pty
    raw <- case mfg of
        Just cmd -> pure cmd
        Nothing -> do
            r <- try (TIO.readFile ("/proc/" <> show (Hat.Term.Pty.pid pane.pty) <> "/comm"))
            pure $ case r of
                Right s -> let t = T.strip s in if T.null t then "sh" else t
                Left (_ :: IOException) -> "sh"
    pure (commandName raw)

-- | Recompute the names of every @automatic-rename@ window from its
-- active pane's foreground command, bumping the render generation on any
-- change. Driven by a periodic poll so no-output commands (an idle
-- @less@, a waiting @cat@) still get picked up.
refreshAutoNames :: ServerState -> IO ()
refreshAutoNames st = do
    fmt <- (.automaticRenameFormat) <$> readTVarIO st.options
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM_ sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        forM_ ws $ \(ix, win) -> do
            auto <- readTVarIO win.autoRename
            when auto $ do
                mnew <- autoName st sess ix win fmt
                forM_ mnew $ \newName -> atomically $ do
                    cur <- readTVar win.name
                    when (cur /= newName && not (T.null newName)) $ do
                        writeTVar win.name newName
                        bumpDirty st

-- | Recompute each session's desktop title (see 'composeTitle') from its
-- current window's active pane, broadcasting only on change. Shares the
-- 500ms poll with 'refreshAutoNames' because the same inputs (foreground
-- command, cwd) change without any event. A no-op unless @set-titles@ is
-- on.
refreshTitles :: ServerState -> IORef (Map.Map SessionId Text) -> IO ()
refreshTitles st ref = do
    opts <- readTVarIO st.options
    when opts.setTitles $ do
        homeDir <- maybe "" T.pack <$> lookupEnv "HOME"
        sessions <- readTVarIO st.sessions
        forM_ (Map.toList sessions) $ \(sid, sess) -> do
            (sname, mwin) <- atomically $
                (,) <$> readTVar sess.name <*> currentWindow sess
            forM_ mwin $ \win -> do
                wname <- readTVarIO win.name
                auto <- readTVarIO win.autoRename
                mpane <- atomically (activePane win)
                forM_ mpane $ \pane -> do
                    dir <- paneCurrentPath pane
                    prog <- paneCommandName pane
                    ptitle <- Emu.title pane.emulator
                    let t = composeTitle titleBudget TitleParts
                            { session = sname
                            -- An auto-renamed window just repeats the
                            -- program; only a pinned name adds signal.
                            , window = if auto then "" else wname
                            , path = T.pack dir
                            , home = homeDir
                            -- A title the program set itself (OSC) is
                            -- the most specific component we have.
                            , program = if T.null ptitle then prog else ptitle
                            }
                    prev <- Map.lookup sid <$> readIORef ref
                    unless (prev == Just t) $ do
                        modifyIORef' ref (Map.insert sid t)
                        broadcast st sid (SetTitle t)

-- | Room for the composed desktop title. The title bar's real width is
-- unknowable from here; this keeps the tail visible in any reasonable
-- window.
titleBudget :: Int
titleBudget = 80

-- | The name an @automatic-rename@ window should currently take: the
-- @automatic-rename-format@ expanded against the active pane. The default
-- format is just @#{pane_current_command}@, so it takes a cheap path.
autoName :: ServerState -> Session -> Int -> Window -> Text -> IO (Maybe Text)
autoName st sess ix win fmt = do
    mpane <- atomically (activePane win)
    case mpane of
        Nothing -> pure Nothing
        Just pane
            | fmt == "#{pane_current_command}" -> Just <$> paneCommandName pane
            | otherwise -> do
                pbase <- (.paneBaseIndex) <$> readTVarIO st.options
                env <- paneFormatEnv st sess ix win pbase pane
                Just <$> expandFormat st env fmt

-- | @break-pane [-d] [-t]@: move the active pane into a new window of
-- its own. No-op when it is the window's only pane.
cmdBreakPane :: CommandImpl
cmdBreakPane st mclient args = do
    let (_, flags, _) = parseArgs "t" args
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        ps <- readTVarIO win.panes
        case mactive of
            Just pane | Map.size ps > 1 -> do
                win2 <- wrapPaneInWindow st pane
                atomically $ do
                    removePaneFromTree st pane.id
                    ws <- readTVar sess.windows
                    let ix = until (\i -> not (Map.member i ws)) (+ 1) 0
                    modifyTVar' sess.windows (Map.insert ix win2)
                    unless ("-d" `elem` flags) $ do
                        cur <- readTVar sess.currentIx
                        writeTVar sess.lastIx (Just cur)
                        writeTVar sess.currentIx ix
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "can't break with only one pane"]

-- | @join-pane [-h|-v] [-b] -s src [-t dst]@: move the @src@ pane into
-- the destination window (default: the current one), splitting its
-- active pane. Backs the config's @choose-window 'join-pane -?s "%%"'@.
cmdJoinPane :: CommandImpl
cmdJoinPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        orient | "-h" `elem` flags = LeftRight
               | otherwise = TopBottom
        placement = if "-b" `elem` flags then Before else After
    msrc <- targetPane st mclient (lookup "-s" opts)
    withCurrentWindow st mclient $ \sess dstWin -> do
        dstPanes <- readTVarIO dstWin.panes
        case msrc of
            Just src | not (Map.member src.id dstPanes) -> do
                atomically $ do
                    dstActive <- readTVar dstWin.activeId
                    removePaneFromTree st src.id
                    modifyTVar' dstWin.panes (Map.insert src.id src)
                    modifyTVar' dstWin.layout
                        (splitLeaf dstActive orient placement src.id)
                    writeTVar dstWin.lastActive (Just dstActive)
                    writeTVar dstWin.activeId src.id
                    writeTVar dstWin.zoomed Nothing
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "no source pane"]

-- | @select-layout <name>@: rearrange the current window's panes into a
-- named layout (@main-vertical@, @even-horizontal@, @tiled@, …). The
-- @main-*@ layouts size their main pane from @main-pane-width@/@-height@.
cmdSelectLayout :: CommandImpl
cmdSelectLayout st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (nameT : _) -> case parseLayoutName nameT of
            Just lname -> applyNamedLayout st mclient lname
            Nothing -> applyLayoutString st mclient nameT
        [] -> pure [RErr "usage: select-layout name"]

-- | Reshape the current window to a saved tmux layout string, mapping
-- its geometry onto the window's panes in order (resurrect's restore).
applyLayoutString :: ServerState -> Maybe Client -> Text -> IO [Reply]
applyLayoutString st mclient str =
    withCurrentWindow st mclient $ \sess win -> do
        ok <- atomically $ do
            pids <- layoutPanes <$> readTVar win.layout
            case layoutFromString str pids of
                Just lay -> do
                    writeTVar win.layout lay
                    writeTVar win.layoutName Nothing
                    writeTVar win.zoomed Nothing
                    bumpDirty st
                    pure True
                Nothing -> pure False
        if ok
            then applySessionSize st sess.id >> pure []
            else pure [RErr ("invalid layout: " <> str)]

parseLayoutName :: Text -> Maybe LayoutName
parseLayoutName = \case
    "main-vertical"   -> Just MainVertical
    "main-horizontal" -> Just MainHorizontal
    "even-horizontal" -> Just EvenHorizontal
    "even-vertical"   -> Just EvenVertical
    "tiled"           -> Just Tiled
    _                 -> Nothing

applyNamedLayout :: ServerState -> Maybe Client -> LayoutName -> IO [Reply]
applyNamedLayout st mclient lname =
    withCurrentWindow st mclient $ \sess win ->
        arrangeNamed st sess win lname >> pure []

-- | The split ratio a named layout gives its main pane. @main-pane-width@ and
-- @main-pane-height@ are absolute cell counts (tmux semantics); expressed here
-- as a fraction of the window along the layout's axis, clamped so both the main
-- pane and the rest keep room. Non-main layouts split evenly.
mainPaneRatio :: LayoutName -> Options -> Size -> Rational
mainPaneRatio lname opts area = case lname of
    MainVertical   -> ratioOf opts.mainPaneWidth area.cols
    MainHorizontal -> ratioOf opts.mainPaneHeight area.rows
    _              -> 1 % 2
  where
    clampR r = max (1 % 10) (min (9 % 10) r) :: Rational
    ratioOf num den = clampR (toInteger num % max 1 (toInteger den))

-- | Rearrange one window into a named layout, sizing the @main-*@ pane
-- from @main-pane-width@/@-height@, and remember the name so
-- @next-layout@ can cycle onward from it.
arrangeNamed :: ServerState -> Session -> Window -> LayoutName -> IO ()
arrangeNamed st sess win lname = do
    eff <- readTVarIO sess.lastSize
    opts <- readTVarIO st.options
    let mainRatio = mainPaneRatio lname opts (windowArea eff)
    atomically $ do
        pids <- layoutPanes <$> readTVar win.layout
        unless (null pids) $ do
            writeTVar win.layout (namedLayout lname mainRatio pids)
            writeTVar win.layoutName (Just lname)
            writeTVar win.zoomed Nothing
            bumpDirty st
    applySessionSize st sess.id

-- | @next-layout@ (default @<prefix> Space@): rearrange the current
-- window into the next of tmux's five named layouts, cycling from the
-- last one applied. @previous-layout@ walks the cycle the other way.
cmdNextLayout :: CommandImpl
cmdNextLayout = cycleLayout nextLayoutName

cmdPreviousLayout :: CommandImpl
cmdPreviousLayout = cycleLayout previousLayoutName

cycleLayout :: (Maybe LayoutName -> LayoutName) -> CommandImpl
cycleLayout step st mclient _ =
    withCurrentWindow st mclient $ \sess win -> do
        cur <- readTVarIO win.layoutName
        arrangeNamed st sess win (step cur)
        pure []

-- | @move-window -s src -t dst@: renumber (or relocate) a window to the
-- destination index, possibly in another session. Restore replays this
-- to place windows at their saved indices.
cmdMoveWindow :: CommandImpl
cmdMoveWindow st mclient args = do
    let (opts, _, _) = parseArgs "st" args
    msrc <- resolveWindowTarget st mclient (lookup "-s" opts)
    mdst <- resolveWindowTarget st mclient (lookup "-t" opts)
    case (msrc, mdst) of
        (Just (srcSess, srcIx), Just (dstSess, dstIx)) -> do
            res <- atomically $ do
                sws <- readTVar srcSess.windows
                case Map.lookup srcIx sws of
                    Nothing -> pure (Right ())  -- nothing to move
                    Just win
                        | srcSess.id == dstSess.id, srcIx == dstIx ->
                            pure (Right ())  -- already there
                        | otherwise -> do
                            dws <- readTVar dstSess.windows
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
        _ -> pure [RErr "usage: move-window -s src -t dst"]
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

cmdResizePane :: CommandImpl
cmdResizePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        delta = case pos of
            (n : _) | Right (v, restT) <- TR.decimal n, T.null restT -> v
            _ -> 1
    if "-Z" `elem` flags
        then zoomTarget st mclient (lookup "-t" opts) >> pure []
        else do
            let mdir
                    | "-L" `elem` flags = Just DirLeft
                    | "-R" `elem` flags = Just DirRight
                    | "-U" `elem` flags = Just DirUp
                    | "-D" `elem` flags = Just DirDown
                    | otherwise = Nothing
            case mdir of
                Nothing ->
                    pure [RErr "usage: resize-pane -L|-R|-U|-D [n] | -Z"]
                Just dir -> withCurrentWindow st mclient $ \sess win -> do
                    atomically $ do
                        eff <- readTVar sess.lastSize
                        active <- readTVar win.activeId
                        modifyTVar' win.layout
                            (resizeSplit active dir delta
                                (sizeRect (windowArea eff)))
                        bumpDirty st
                    applySessionSize st sess.id
                    pure []

-- | Toggle zoom on the caller's current window. With a @-t@ target the
-- targeted pane becomes active first and the toggle keys off it, so
-- @resize-pane -t ! -Z@ zooms the alternate pane even while another pane is
-- already zoomed (as the config's @Z@ binding intends). See 'nextZoom'.
zoomTarget :: ServerState -> Maybe Client -> Maybe Text -> IO ()
zoomTarget st mclient mtok = do
    mtarget <- targetPane st mclient mtok
    void . withCurrentWindow st mclient $ \sess win -> do
        atomically $ do
            ps <- readTVar win.panes
            forM_ mtarget $ \pane -> when (Map.member pane.id ps) $ do
                active <- readTVar win.activeId
                when (active /= pane.id) $ do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId pane.id
            mz <- readTVar win.zoomed
            newActive <- readTVar win.activeId
            writeTVar win.zoomed (nextZoom mz newActive)
            bumpDirty st
        applySessionSize st sess.id
        pure []

-- | The zoom state after toggling zoom on a target pane: unzoom only when
-- that pane is the one already zoomed, otherwise zoom it. Keying off the
-- target (not merely whether some pane is zoomed) is what lets @resize-pane
-- -t ! -Z@ zoom the alternate pane even while another pane is zoomed. See
-- 'zoomTarget'.
nextZoom :: Maybe PaneId -> PaneId -> Maybe PaneId
nextZoom mz target
    | mz == Just target = Nothing
    | otherwise         = Just target

cmdDetachClient :: CommandImpl
cmdDetachClient _ mclient _ = do
    forM_ mclient $ \client -> send client DetachOk
    pure []

cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient _ = do
    forM_ mclient $ \client -> do
        opts <- readTVarIO st.options
        forM_ (parseKeyName opts.prefix) $ \key -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> Hat.Term.Pty.writePty pane.pty key.raw
    pure []

cmdSendKeys :: CommandImpl
cmdSendKeys st mclient args = do
    let (opts, flags, pos) = parseArgs "tN" args
        literal = "-l" `elem` flags
        modeCmd = "-X" `elem` flags
    mpicker <- maybe (pure Nothing) (readTVarIO . (.picker)) mclient
    case (mpicker, mclient) of
        -- An open chooser owns send-keys: they drive its navigation/search
        -- (this is how the config's @… \; send-keys /@ enters search).
        (Just pk, Just client) | not modeCmd -> do
            handlePickerInput st client pk
                (concatMap (tokenizeKeys . argBytes literal) pos)
            pure []
        _ -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            forM_ mpane $ \pane ->
                if modeCmd
                    then case pos of
                        (name : cmdArgs) -> runCopyModeCommand st pane name cmdArgs
                        [] -> pure ()
                    else Hat.Term.Pty.writePty pane.pty
                        (B.concat (map (argBytes literal) pos))
            pure []
  where
    argBytes True a = TE.encodeUtf8 a
    argBytes False a = case parseKeyName a of
        Just k -> k.raw
        Nothing -> TE.encodeUtf8 a

runCopyModeCommand :: ServerState -> Pane -> Text -> [Text] -> IO ()
runCopyModeCommand st pane name cmdArgs = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure ()  -- not in copy mode; -X is a no-op
        Just pm
            -- A digit key builds the @[count]@ prefix rather than running
            -- a motion; @0@ with no count pending is @start-of-line@.
            | name == "digit", Just d <- readDigit cmdArgs ->
                atomically $ do
                    writeTVar pane.mode
                        (Just (reMode (CopyMode.pushDigit d state)))
                    bumpDirty st
            | otherwise -> case Map.lookup name CopyMode.handlers of
                Nothing -> pure ()
                Just h -> do
                    -- Motions repeat [count] times; yanks never do. Every
                    -- command clears the pending count.
                    let count
                            | name `elem` ["copy-selection", "copy-pipe"] = 1
                            | otherwise = min 1000 (maybe 1 (max 1) state.numPrefix)
                    result <- applyN h (state { numPrefix = Nothing }) count
                    result' <- traverse (scrollPaneToCursor pane) result
                    atomically $ do
                        writeTVar pane.mode (reMode <$> result')
                        bumpDirty st
          where
            state = pm.copyState
            reMode s = pm { copyState = s }
  where
    readDigit (a : _) = case TR.decimal a of
        Right (d, rest) | T.null rest, d >= 0, d <= 9 -> Just d
        _ -> Nothing
    readDigit [] = Nothing
    -- Run a handler @n@ times, threading the state and stopping early if
    -- it exits copy mode (@Nothing@).
    applyN _ s 0 = pure (Just s)
    applyN h s n = do
        r <- h st pane s cmdArgs
        case r of
            Nothing -> pure Nothing
            Just s' -> applyN h s' (n - 1)

-- | Re-center a pane's copy-mode viewport on its cursor after a motion,
-- over the pane's frozen snapshot (a no-op when not in copy mode).
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    mmode <- readTVarIO pane.mode
    pure $ case mmode of
        Just pm -> CopyMode.scrollToCursor pm.frozen.fgHsize pm.frozen.fgSy s
        Nothing -> s

-- | Resolve the pane a command should act on from its @-t target@.
-- @!@ is the current window's last-active pane, @~@/@{marked}@ the
-- marked pane, @%N@ a pane by id anywhere; otherwise the current pane
-- of the caller's window.
targetPane :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPane st mclient mtok = case parsePaneTarget mtok of
    PaneById n -> atomically (findPaneById st n)
    PaneMarked -> atomically $ do
        mpid <- readTVar st.markedPane
        maybe (pure Nothing) (findPaneById st . rawPane) mpid
    tgt -> do
        mwin <- currentWindowOf st mclient
        case mwin of
            Nothing -> pure Nothing
            Just win -> atomically $ do
                ps <- readTVar win.panes
                case tgt of
                    PaneLast -> do
                        ml <- readTVar win.lastActive
                        pure (ml >>= (`Map.lookup` ps))
                    _ -> do
                        a <- readTVar win.activeId
                        pure (Map.lookup a ps)

-- | The window a command acts in: the caller's current window, or (for
-- a clientless control command) the current window of the most-recent
-- session.
currentWindowOf :: ServerState -> Maybe Client -> IO (Maybe Window)
currentWindowOf st mclient = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    case mv of
        Just (_, win) -> pure (Just win)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> atomically (currentWindow sess)

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        quit = "-q" `elem` flags
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane
            | quit -> do
                atomically $ do
                    writeTVar pane.mode Nothing
                    bumpDirty st
                pure []
            | otherwise -> do
                scr <- Emu.snapshot pane.emulator
                frozen <- CopyMode.freezeGrid pane.emulator
                srvOpts <- readTVarIO st.options
                let table = case srvOpts.modeKeys of
                        KeysVi -> "copy-mode-vi"
                        KeysEmacs -> "copy-mode"
                    startRow = frozen.fgHsize + scr.cursor.row
                    startCol = scr.cursor.col
                    state = CopyModeState
                        { cursorRow = startRow
                        , cursorCol = startCol
                        , selection = Nothing
                        , keyTable = table
                        , viewportOffY = 0
                        , numPrefix = Nothing
                        , pendingSearch = Nothing
                        , lastSearch = Nothing
                        , lastQuery = Nothing
                        }
                atomically $ do
                    writeTVar pane.mode (Just PaneMode
                        { frozen = frozen, copyState = state })
                    bumpDirty st
                pure []

-- | Open the interactive command prompt on the invoking client.
-- | @command-prompt [-I initial] [-p prompt] [template]@. Opens the line
-- editor; @-I@ pre-fills it (format-expanded, so @#W@ is the window name),
-- and a @template@ has the submitted line spliced in for @%%@. Backs the
-- @,@ rename binding: @command-prompt -I "#W" "rename-window '%%'"@.
cmdCommandPrompt :: CommandImpl
cmdCommandPrompt st mclient args = do
    let (opts, _flags, pos) = parseArgs "Ip" args
        tmpl = T.unwords pos
        pfx = case lookup "-p" opts of
            Just p -> p
            Nothing
                | T.null tmpl -> ":"
                | otherwise -> "(" <> T.takeWhile (/= ' ') tmpl <> ") "
    forM_ mclient $ \client -> do
        initial <- case lookup "-I" opts of
            Nothing -> pure ""
            Just raw -> do
                env <- clientPromptEnv st client
                expandFormat st env raw
        atomically $ do
            writeTVar client.prompt (Just (Prompt.promptFor pfx initial tmpl))
            bumpDirty st
    pure []

-- | The format environment of a client's current window, for expanding a
-- @command-prompt -I@ initial string. Empty if the client has no window.
clientPromptEnv :: ServerState -> Client -> IO FormatEnv
clientPromptEnv st client = do
    mv <- atomically (clientView st client)
    case mv of
        Nothing -> pure Map.empty
        Just (sess, win) -> do
            ix <- readTVarIO sess.currentIx
            windowFormatEnv st sess ix win

-- | Open a chooser overlay on the invoking client.
openPicker :: ServerState -> Client -> Text -> PickerFill -> [PickerNode] -> IO ()
openPicker st client titleText fill picked = atomically $ do
    writeTVar client.picker $ Just PickerState
        { title = titleText
        , roots = picked
        , cursor = 0
        , query = ""
        , search = ""
        , mode = Browsing
        , fill = fill
        }
    bumpDirty st

-- | @choose-tree [-GswZ]@: a filterable tree of every session, its
-- windows and their panes; Enter switches to the chosen one. @-s@ opens
-- with sessions collapsed (sessions only), @-w@ with windows expanded but
-- panes collapsed, and neither fully expanded. The config opens it with
-- @… \; send-keys /@ to jump straight into search.
cmdChooseTree :: CommandImpl
cmdChooseTree st mclient args = do
    let (_, flags, _) = parseArgs "" args
        sessionsOnly = "-s" `elem` flags
        windowsOnly  = "-w" `elem` flags
        windowsExp = if sessionsOnly then Collapsed else Expanded
        panesExp   = if sessionsOnly || windowsOnly then Collapsed else Expanded
        fill = if "-Z" `elem` flags then FillWindow else PaneRegion
    forM_ mclient $ \client -> do
        picked <- buildTreeNodes st windowsExp panesExp
        openPicker st client "choose a window" fill picked
    pure []

-- Panes are shown under their window for visual context, but tmux can't name
-- them, so they carry no meaningful search text; the picker marks them
-- (via 'PreviewPane') as non-matching so search never targets them.
buildTreeNodes :: ServerState -> Expansion -> Expansion -> IO [PickerNode]
buildTreeNodes st windowsExp panesExp = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM sessions $ \sess -> do
        sname <- readTVarIO sess.name
        ws <- Map.toAscList <$> readTVarIO sess.windows
        winNodes <- forM ws $ \(ix, win) -> do
            wname <- readTVarIO win.name
            apid <- readTVarIO win.activeId
            ordered <- Map.elems <$> readTVarIO win.panes
            let winCmd = "switch-client -t " <> sname
                    <> " ; select-window -t " <> sname <> ":" <> tshow ix
                paneNodes =
                    [ PickerNode
                        { label = "pane " <> tshow pix
                            <> (if pane.id == apid then "*" else "")
                        , command = winCmd <> " ; select-pane -t " <> tshow pix
                        , preview = Just (PreviewPane pane.id)
                        , children = []
                        , expanded = Collapsed }
                    | (pix, pane) <- zip [0 :: Int ..] ordered ]
            pure PickerNode
                { label = tshow ix <> ":" <> wname
                , command = winCmd
                , preview = Just (PreviewWindow win.id)
                , children = Picker.windowChildren paneNodes
                , expanded = panesExp }
        pure PickerNode
            { label = sname
            , command = "switch-client -t " <> sname
            , preview = Just (PreviewSession sess.id)
            , children = winNodes
            , expanded = windowsExp }

-- | @choose-window <template>@: a list of the current session's windows;
-- selecting one runs @template@ with each @%%@ replaced by that window's
-- active pane id, so @choose-window 'join-pane -hs \"%%\"'@ joins it here.
cmdChooseWindow :: CommandImpl
cmdChooseWindow st mclient args = do
    let (_, _, pos) = parseArgs "" args
    case (mclient, pos) of
        (Just client, template : _) -> do
            picked <- buildWindowItems st client template
            openPicker st client "choose a window" PaneRegion picked
            pure []
        _ -> pure [RErr "usage: choose-window template"]

buildWindowItems :: ServerState -> Client -> Text -> IO [PickerNode]
buildWindowItems st client template = do
    sid <- readTVarIO client.session
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            forM ws $ \(ix, win) -> do
                wname <- readTVarIO win.name
                apid <- readTVarIO win.activeId
                let target = "%" <> tshow (rawPane apid)
                pure $ Picker.leaf (tshow ix <> ":" <> wname)
                    (T.replace "%%" target template)

cmdShowBuffer :: CommandImpl
cmdShowBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    bufs <- readTVarIO st.buffers
    pure $ case bufferBody (lookup "-b" opts) bufs of
        Nothing -> [RErr "no buffers"]
        Just body -> [ROutput body]

cmdSetBuffer :: CommandImpl
cmdSetBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "bn" args
        appendMode = "-a" `elem` flags
        mname = lookup "-b" opts
        body = T.unwords pos
    if null pos
        then pure [RErr "usage: set-buffer [-a] [-b name] data"]
        else atomically $ do
            bufs <- readTVar st.buffers
            case mname of
                Just name -> do
                    let existing = lookupBuffer name bufs
                        newBody = case (appendMode, existing) of
                            (True, Just prev) -> prev <> body
                            _ -> body
                        others = Seq.filter ((/= name) . fst) bufs
                    writeTVar st.buffers ((name, newBody) Seq.<| others)
                Nothing -> do
                    n <- readTVar st.nextBuffer
                    writeTVar st.nextBuffer (n + 1)
                    let name = "buffer" <> T.pack (show n)
                    writeTVar st.buffers ((name, body) Seq.<| bufs)
            bumpDirty st
            pure []

cmdListBuffers :: CommandImpl
cmdListBuffers st _ _ = do
    bufs <- readTVarIO st.buffers
    pure . map row $ toList' bufs
  where
    row (name, body) =
        ROutput (name <> ": " <> tshow (T.length body) <> " bytes")
    toList' s = case Seq.viewl s of
        Seq.EmptyL -> []
        x Seq.:< xs -> x : toList' xs

cmdDeleteBuffer :: CommandImpl
cmdDeleteBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    atomically $ do
        bufs <- readTVar st.buffers
        writeTVar st.buffers (dropBuffer (lookup "-b" opts) bufs)
        pure []

-- | Write the top (or named) buffer to a file. @-a@ appends; the path
-- may start with @~/@.
cmdSaveBuffer :: CommandImpl
cmdSaveBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "b" args
        appendMode = "-a" `elem` flags
    case pos of
        [] -> pure [RErr "usage: save-buffer [-a] [-b name] path"]
        (rawPath : _) -> do
            bufs <- readTVarIO st.buffers
            case bufferBody (lookup "-b" opts) bufs of
                Nothing -> pure [RErr "no buffers"]
                Just body -> do
                    path <- expandTilde (T.unpack rawPath)
                    let write = if appendMode then TIO.appendFile else TIO.writeFile
                    r <- try (write path body)
                    pure $ case r of
                        Left (e :: IOException) -> [RErr (T.pack (show e))]
                        Right () -> []

-- | Paste the top (or named) buffer into a pane's pty. @-d@ deletes the
-- buffer afterwards, @-p@ wraps it in bracketed-paste markers, @-r@
-- turns carriage returns into newlines.
cmdPasteBuffer :: CommandImpl
cmdPasteBuffer st mclient args = do
    let (opts, flags, _) = parseArgs "bt" args
        del = "-d" `elem` flags
        bracketed = "-p" `elem` flags
        crToNl = "-r" `elem` flags
        mname = lookup "-b" opts
    bufs <- readTVarIO st.buffers
    case bufferBody mname bufs of
        Nothing -> pure [RErr "no buffers"]
        Just body0 -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            case mpane of
                Nothing -> pure [RErr "no target pane"]
                Just pane -> do
                    let body = if crToNl
                            then T.map (\c -> if c == '\r' then '\n' else c) body0
                            else body0
                        payload
                            | bracketed = "\ESC[200~" <> body <> "\ESC[201~"
                            | otherwise = body
                    Hat.Term.Pty.writePty pane.pty (TE.encodeUtf8 payload)
                    when del $ atomically $ do
                        cur <- readTVar st.buffers
                        writeTVar st.buffers (dropBuffer mname cur)
                    pure []

-- | @pipe-pane [-IOo] [-t target] [command]@. With no command (or @-o@
-- while already piping) it stops the pane's pipe. Otherwise it spawns
-- @sh -c command@: @-O@ (the default) feeds pane output to the process's
-- stdin, @-I@ feeds the process's stdout back into the pane.
cmdPipePane :: CommandImpl
cmdPipePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        hasI = "-I" `elem` flags
        hasO = "-O" `elem` flags
        outputTap = if hasO || not hasI  -- default direction is -O
            then OutputTapped else OutputUntapped
        stdinFeed = if hasI then StdinFed else StdinUnfed
        toggle = "-o" `elem` flags
        cmd = T.strip (T.unwords pos)
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane -> do
            wasPiping <- isJust <$> readTVarIO pane.pipe
            stopPipe pane
            if T.null cmd || (toggle && wasPiping)
                then pure []
                else startPipe pane (T.unpack cmd) outputTap stdinFeed >> pure []

-- | @-O@: whether pane output is tapped into the process's stdin.
data OutputTap = OutputTapped | OutputUntapped
    deriving (Eq)

-- | @-I@: whether the process's stdout is fed back into the pane.
data StdinFeed = StdinFed | StdinUnfed
    deriving (Eq)

-- Spawn the pipe subprocess and record it on the pane.
startPipe :: Pane -> String -> OutputTap -> StdinFeed -> IO ()
startPipe pane cmd outputTap stdinFeed = do
    (mIn, mOut, _, ph) <- createProcess (shell cmd)
        { std_in  = if tapOn then CreatePipe else Inherit
        , std_out = if feedOn then CreatePipe else Inherit
        }
    rtid <- case (feedOn, mOut) of
        (True, Just hout) -> Just <$> forkIO (pumpPipeOutput pane hout)
        _ -> pure Nothing
    atomically $ writeTVar pane.pipe $ Just PipeHandle
        { process = ph
        , toStdin = if tapOn then mIn else Nothing
        , reader = rtid
        }
  where
    tapOn = outputTap == OutputTapped
    feedOn = stdinFeed == StdinFed

-- Read the process's stdout and write it into the pane's pty (@-I@).
pumpPipeOutput :: Pane -> Handle -> IO ()
pumpPipeOutput pane hout = loop `catch` \(_ :: SomeException) -> pure ()
  where
    loop = do
        chunk <- B8.hGetSome hout 4096
        unless (B8.null chunk) $ do
            Hat.Term.Pty.writePty pane.pty chunk
            loop

-- Feed a chunk of pane output to the pipe subprocess (@-O@).
forwardToPipe :: Pane -> B.ByteString -> IO ()
forwardToPipe pane bs = do
    mp <- readTVarIO pane.pipe
    forM_ mp $ \ph -> forM_ ph.toStdin $ \hdl ->
        (B8.hPut hdl bs >> hFlush hdl)
            `catch` \(_ :: SomeException) -> pure ()

-- Stop and reap any pipe subprocess on the pane.
stopPipe :: Pane -> IO ()
stopPipe pane = do
    mp <- atomically $ do
        m <- readTVar pane.pipe
        writeTVar pane.pipe Nothing
        pure m
    forM_ mp $ \ph -> do
        forM_ ph.reader killThread
        forM_ ph.toStdin $ \hdl ->
            hClose hdl `catch` \(_ :: SomeException) -> pure ()
        terminateProcess ph.process `catch` \(_ :: SomeException) -> pure ()
        void . forkIO $
            void (waitForProcess ph.process)
                `catch` \(_ :: SomeException) -> pure ()

-- | The top buffer, or a named one.
bufferBody :: Maybe Text -> Seq (Text, Text) -> Maybe Text
bufferBody mname bufs = case mname of
    Just name -> lookupBuffer name bufs
    Nothing -> case bufs of
        Seq.Empty -> Nothing
        (_, body) Seq.:<| _ -> Just body

-- | Drop the top buffer, or a named one.
dropBuffer :: Maybe Text -> Seq (Text, Text) -> Seq (Text, Text)
dropBuffer mname bufs = case mname of
    Just name -> Seq.filter ((/= name) . fst) bufs
    Nothing -> case bufs of
        Seq.Empty -> bufs
        _ Seq.:<| rest -> rest

lookupBuffer :: Text -> Seq (Text, Text) -> Maybe Text
lookupBuffer name = go
  where
    go s = case Seq.viewl s of
        Seq.EmptyL -> Nothing
        (n, b) Seq.:< rest
            | n == name -> Just b
            | otherwise -> go rest

cmdNewSession :: CommandImpl
cmdNewSession st mclient args = do
    let (opts, flags, pos) = parseArgs "sctnxy" args
        mname = lookup "-s" opts
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    dup <- case mname of
        Nothing -> pure False
        Just nm -> atomically $ do
            sessions <- readTVar st.sessions
            names <- mapM (\s -> readTVar s.name) (Map.elems sessions)
            pure (nm `elem` names)
    if dup
        then pure [RErr ("duplicate session: " <> fromMaybe "" mname)]
        else do
            (environ, dir, baseSz) <- case mclient of
                Just c -> do
                    csz <- readTVarIO c.size
                    pure (c.env, T.unpack c.cwd, csz)
                Nothing -> do
                    -- Config-loaded or otherwise clientless: inherit the
                    -- server process env so shells find PATH, SHELL, etc.
                    procEnv <- getEnvironment
                    pure ( [(T.pack k, T.pack v) | (k, v) <- procEnv]
                         , "/"
                         , Size { rows = 24, cols = 80 } )
            let dir' = maybe dir T.unpack (lookup "-c" opts)
                parseInt t = case TR.decimal t of
                    Right (n, rest) | T.null rest -> Just n
                    _ -> Nothing
                sz = baseSz
                    { cols = fromMaybe baseSz.cols (parseInt =<< lookup "-x" opts)
                    , rows = fromMaybe baseSz.rows (parseInt =<< lookup "-y" opts)
                    }
            sess <- createSession st mname mrun environ dir' sz
            atomically $ do
                writeTVar st.everAttached True
                forM_ (lookup "-n" opts) $ \wname -> do
                    ws <- readTVar sess.windows
                    forM_ (Map.elems ws) $ \w -> do
                        writeTVar w.name wname
                        writeTVar w.autoRename False
            unless ("-d" `elem` flags) $
                forM_ mclient $ \client -> switchClientTo st client sess
            pure []

switchClientTo :: ServerState -> Client -> Session -> IO ()
switchClientTo st client sess = do
    old <- readTVarIO client.session
    atomically $ do
        when (old /= sess.id) $ do
            writeTVar client.lastSession (Just old)
            writeTVar st.lastSession (Just old)
            writeTVar client.session sess.id
        writeTVar st.lastActiveSession (Just sess.id)
        markActive st client
        writeTVar client.needsFull True
        bumpDirty st
    applySessionSize st old
    applySessionSize st sess.id

-- @-c@ re-anchors the session's default working directory for new
-- windows, so it is useful (and valid) even without a client to attach.
cmdAttachSession :: CommandImpl
cmdAttachSession st mclient args = do
    let (opts, _, _) = parseArgs "tc" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        forM_ (lookup "-c" opts) $ \d -> do
            env <- sessionFormatEnv st sess
            dir <- T.unpack <$> expandFormat st env d
            atomically $ do
                writeTVar sess.startCwd dir
                bumpDirty st
        case mclient of
            Just client -> switchClientTo st client sess >> pure []
            Nothing
                | isJust (lookup "-c" opts) -> pure []
                | otherwise -> pure [RErr "no client to attach"]

cmdKillSession :: CommandImpl
cmdKillSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        locs <- atomically $ do
            ws <- Map.elems <$> readTVar sess.windows
            fmap concat . forM ws $ \win -> do
                ps <- Map.elems <$> readTVar win.panes
                pure [(sess.id, win, p) | p <- ps]
        killPaneLocs st locs
        pure []

cmdHasSession :: CommandImpl
cmdHasSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    msess <- targetSession st mclient (lookup "-t" opts)
    pure $ case msess of
        Just _ -> []
        Nothing -> [RErr $ "can't find session: "
            <> fromMaybe "" (lookup "-t" opts)]

-- The server is necessarily running by the time this executes.
cmdStartServer :: CommandImpl
cmdStartServer _ _ _ = pure []

cmdRenameSession :: CommandImpl
cmdRenameSession st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
            dup <- atomically $ do
                sessions <- readTVar st.sessions
                names <- mapM (\s -> readTVar s.name)
                    (filter (\s -> s.id /= sess.id) (Map.elems sessions))
                pure (nm `elem` names)
            if dup
                then pure [RErr ("duplicate session: " <> nm)]
                else do
                    atomically $ do
                        writeTVar sess.name nm
                        bumpDirty st
                    pure []
        _ -> pure [RErr "usage: rename-session [-t target] name"]

cmdListSessions :: CommandImpl
cmdListSessions st _ args = do
    let (opts, _, _) = parseArgs "F" args
    sessions <- Map.elems <$> readTVarIO st.sessions
    lines' <- case lookup "-F" opts of
        Just fmt -> forM sessions $ \sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env fmt
        Nothing -> forM sessions $ \sess -> atomically $ do
            nm <- readTVar sess.name
            ws <- readTVar sess.windows
            pure (nm <> ": " <> tshow (Map.size ws) <> " windows")
    pure (map ROutput lines')

cmdListWindows :: CommandImpl
cmdListWindows st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    sessions <- if allSessions
        then Map.elems <$> readTVarIO st.sessions
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            pure (maybe [] pure msess)
    fmap (map ROutput . concat) . forM sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        cur <- readTVarIO sess.currentIx
        forM ws $ \(ix, win) -> case mfmt of
            Just fmt -> do
                env <- windowFormatEnv st sess ix win
                expandFormat st env fmt
            Nothing -> atomically $ do
                nm <- readTVar win.name
                ps <- readTVar win.panes
                let mark = if ix == cur then "*" else ""
                pure $ tshow ix <> ": " <> nm <> mark
                    <> " (" <> tshow (Map.size ps) <> " panes)"

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = do
    base <- sessionFormatEnv st sess
    eff <- readTVarIO sess.lastSize
    (wname, lay, cur, mlast, bell, act, auto, zoom) <- atomically $ (,,,,,,,)
        <$> readTVar win.name <*> readTVar win.layout
        <*> readTVar sess.currentIx <*> readTVar sess.lastIx
        <*> readTVar win.bellFlag <*> readTVar win.activity
        <*> readTVar win.autoRename <*> readTVar win.zoomed
    ps <- readTVarIO win.panes
    let flags = windowFlags WindowFlagState
            { flagCurrent = ix == cur
            , flagLast = Just ix == mlast
            , flagBell = bell
            , flagActivity = act
            , flagZoomed = isJust zoom
            }
    pure $ Map.union (Map.fromList
        [ ("window_index", tshow ix)
        , ("window_name", wname)
        , ("window_layout", emitLayout (sizeRect (windowArea eff)) lay)
        , ("window_active", if ix == cur then "1" else "0")
        , ("window_flags", flags)
        , ("window_panes", tshow (Map.size ps))
        , ("automatic_rename", if auto then "1" else "0")
        ]) base

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = do
    wenv <- windowFormatEnv st sess wix win
    dir <- paneCurrentPath pane
    cmd <- paneCommandName pane
    scr <- Emu.snapshot pane.emulator
    hsize <- Emu.scrollbackLength pane.emulator
    active <- readTVarIO win.activeId
    sz <- readTVarIO pane.size
    pure $ Map.union (Map.fromList
        [ ("pane_id", "%" <> tshow (rawPane pane.id))
        , ("pane_index", tshow pix)
        , ("pane_pid", tshow (Hat.Term.Pty.pid pane.pty))
        , ("pane_current_path", T.pack dir)
        , ("pane_current_command", cmd)
        , ("pane_active", if pane.id == active then "1" else "0")
        , ("cursor_x", tshow scr.cursor.col)
        , ("cursor_y", tshow scr.cursor.row)
        , ("history_size", tshow hsize)
        , ("pane_width", tshow sz.cols)
        , ("pane_height", tshow sz.rows)
        , ("session_grouped", "0")  -- hat has no session groups
        ]) wenv

cmdListPanes :: CommandImpl
cmdListPanes st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    -- Which (session, window-index, window) triples to list panes from:
    -- -a covers every window of every session; otherwise the target
    -- session's current window.
    targets <- if allSessions
        then do
            sessions <- Map.elems <$> readTVarIO st.sessions
            fmap concat . forM sessions $ \sess -> do
                ws <- Map.toAscList <$> readTVarIO sess.windows
                pure [ (sess, ix, win) | (ix, win) <- ws ]
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            case msess of
                Nothing -> pure []
                Just sess -> do
                    cur <- readTVarIO sess.currentIx
                    mwin <- atomically (currentWindow sess)
                    pure [ (sess, cur, win) | win <- maybe [] pure mwin ]
    pbase <- (.paneBaseIndex) <$> readTVarIO st.options
    fmap (map ROutput . concat) . forM targets $ \(sess, wix, win) -> do
        ps <- Map.elems <$> readTVarIO win.panes
        forM (zip [pbase ..] ps) $ \(pix, pane) -> case mfmt of
            Just fmt -> do
                env <- paneFormatEnv st sess wix win pix pane
                expandFormat st env fmt
            Nothing -> do
                sz <- readTVarIO pane.size
                pure $ "%" <> tshow (rawPane pane.id) <> ": ["
                    <> tshow sz.cols <> "x" <> tshow sz.rows <> "]"

cmdSwitchClient :: CommandImpl
cmdSwitchClient st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
    case mclient of
        Nothing -> pure [RErr "no client"]
        Just client
            | "-l" `elem` flags -> do
                mlast <- readTVarIO client.lastSession
                sessions <- readTVarIO st.sessions
                case mlast >>= (`Map.lookup` sessions) of
                    Nothing -> pure [RErr "no last session"]
                    Just sess -> switchClientTo st client sess >> pure []
            | otherwise ->
                withTargetSession st mclient (lookup "-t" opts) $ \sess ->
                    switchClientTo st client sess >> pure []

cmdKillServer :: CommandImpl
cmdKillServer st mclient _ = do
    -- Flag first: pane readers race us into closePane once the ptys go,
    -- and the shutdown path must know this drain is a kill, not the last
    -- window closing (which drops the store instead).
    atomically $ writeTVar st.preserveStore True
    saveNow st  -- capture the tree before we tear it down
    sessions <- readTVarIO st.sessions
    forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
    forM_ mclient $ \client -> send client Exited
    panes <- atomically $ do
        sess <- readTVar st.sessions
        fmap concat . forM (Map.elems sess) $ \s -> do
            ws <- readTVar s.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
    forM_ panes hangupPane
    atomically $ do
        writeTVar st.sessions Map.empty
        writeTVar st.everAttached True
    pure []

-- | @restart-server [path]@: reload the server binary in place while every
-- pane's program keeps running. Serializes the live tree and its inherited fds
-- to a handover file, drops the clients (they reconnect with @hat@), then
-- re-execs @path@ (default: the on-PATH @hat@, see 'resolveReloadTarget'),
-- which re-adopts the tree ('resumeServer'). The pane pty masters and the
-- listening socket survive the exec; clients' accepted sockets are
-- close-on-exec, so they drop and the users reattach. A missing target is
-- reported before anything is torn down, so a typo'd path is a harmless error
-- rather than a half-dropped server.
cmdRestartServer :: CommandImpl
cmdRestartServer st mclient args = do
    -- Fail loud rather than reload on top of an in-flight reload/restore:
    -- capturing a half-rebuilt tree and re-exec'ing through it is how a
    -- second restart-server strands the live programs. The restore clears
    -- this gate the moment the tree is whole again.
    restoring <- readTVarIO st.restoring
    if restoring
        then pure [RErr "restart-server: a reload is already in progress; try again shortly"]
        else cmdRestartServer' st mclient args

cmdRestartServer' :: CommandImpl
cmdRestartServer' st mclient args = do
    let (_, _, pos) = parseArgs "" args
    target <- case pos of
        (p : _) -> pure (T.unpack p)    -- explicit binary path (deterministic)
        []      -> resolveReloadTarget  -- default: the on-PATH hat
    exists <- doesFileExist target
    if not exists
        then pure [RErr ("restart-server: no such binary: " <> T.pack target)]
        else do
            logEvent st.logger ServerReloading { target = target }
            (cleanup, tree) <- captureReload st
            let blobPath = st.sockPath <> ".reload"
            B.writeFile blobPath (encodeHandover cleanup tree)
            keepOpenAcrossExec cleanup
            sessions <- readTVarIO st.sessions
            forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
            forM_ mclient $ \client -> send client Exited
            mconfig <- readTVarIO st.serverConfig
            let argv = ["--server", st.sockPath]
                    <> maybe [] (: []) mconfig
                    <> ["--reload-handover", blobPath]
            -- The self-exec replaces this image, so 'withLogger's flush-on-exit
            -- never runs; drain the queue now or the reload trace is lost.
            flushLogger st.logger
            _ <- executeFile target False argv Nothing
            pure []  -- unreachable: executeFile replaces this image

-- | The binary a no-argument @restart-server@ re-execs: @hat@ as resolved on
-- @PATH@. That is a stable profile\/system symlink the kernel follows at exec
-- time, so after an upgrade @hat restart-server@ picks up the newly-installed
-- version while keeping the pane programs alive — the point of the feature. The
-- handover's era gate keeps that safe across versions: a matching era adopts
-- the tree, a changed one falls back to a clean restart ('cleanupInherited').
-- 'getExecutablePath' would not do: it resolves @\/proc\/self\/exe@ to the
-- immutable Nix store path this process launched from — the OLD build — so it
-- can never see an upgrade. It is only the last-resort fallback when @hat@ is
-- not on @PATH@. A caller that needs determinism (tests) or a specific build
-- passes the path as @restart-server@'s argument, bypassing this entirely.
resolveReloadTarget :: IO FilePath
resolveReloadTarget = do
    onPath <- findExecutable "hat"
    maybe getExecutablePath pure onPath

-- Clear close-on-exec on the fds the reload must carry into the new image:
-- the listening socket and every pane's pty master. They are not marked
-- close-on-exec today, but a libc that set the flag would otherwise slam them
-- shut on the exec and hang up every program.
keepOpenAcrossExec :: ReloadCleanup -> IO ()
keepOpenAcrossExec cleanup = do
    clear cleanup.listenFd
    forM_ cleanup.live $ \(fd, _pid) -> clear fd
  where
    clear fd = PIO.setFdOption (Fd (fromIntegral fd)) PIO.CloseOnExec False
        `catch` \(_ :: IOException) -> pure ()

-- Release fds a reload inherited but cannot use — an incompatible or corrupt
-- payload. Closing a pane master hangs its child up (and SIGHUP makes sure),
-- so the incoming image starts fresh without orphaning the old processes or
-- leaking the old socket.
cleanupInherited :: ReloadCleanup -> IO ()
cleanupInherited cleanup = do
    forM_ cleanup.live $ \(fd, pid) -> do
        signalProcess sigHUP (fromIntegral pid)
            `catch` \(_ :: IOException) -> pure ()
        PIO.closeFd (Fd (fromIntegral fd))
            `catch` \(_ :: IOException) -> pure ()
    PIO.closeFd (Fd (fromIntegral cleanup.listenFd))
        `catch` \(_ :: IOException) -> pure ()

cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args = do
    let (_, flags, pos) = parseArgs "t" args
        raw = T.unwords pos
    msess <- targetSession st mclient Nothing
    text <- case msess of
        Nothing -> pure raw
        Just sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env raw
    if "-p" `elem` flags
        then pure [ROutput text]
        else do
            forM_ mclient $ \client -> showToast st client text
            pure []

cmdRunShell :: CommandImpl
cmdRunShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
        cmdText = T.unwords pos
    void . forkIO $ do
        (code, out, errOut) <- readCreateProcessWithExitCode
            (shell (T.unpack cmdText)) { close_fds = True } ""
        let firstLine = T.strip . T.takeWhile (/= '\n') . T.pack
        case code of
            ExitSuccess ->
                forM_ mclient $ \client ->
                    unless (null out) $ showToast st client (firstLine out)
            ExitFailure n ->
                forM_ mclient $ \client ->
                    showToast st client $
                        "run-shell exited " <> tshow n <> ": "
                        <> firstLine (out <> errOut)
    pure []

cmdIfShell :: CommandImpl
cmdIfShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (cond0 : thenCmd : rest) -> do
            -- tmux expands #{...} in the condition before running it, so
            -- @if-shell '[ "#{@pane-theme}" = dark ]' …@ works.
            msess <- targetSession st mclient Nothing
            cond <- case msess of
                Just sess -> do
                    env <- sessionFormatEnv st sess
                    expandFormat st env cond0
                Nothing -> pure cond0
            (code, _, _) <- readCreateProcessWithExitCode
                (shell (T.unpack cond)) { close_fds = True } ""
            let chosen = case (code, rest) of
                    (ExitSuccess, _) -> Just thenCmd
                    (ExitFailure _, [elseCmd]) -> Just elseCmd
                    _ -> Nothing
            case chosen of
                Nothing -> pure []
                Just cmdText -> runCommandText st mclient cmdText
        _ -> pure [RErr "usage: if-shell condition command [command]"]

-- Control clients ------------------------------------------------------------

controlLoop :: ServerState -> Client -> IO ()
controlLoop st client = do
    m <- recvMessage client.sock
    case m of
        Just (Known (Command cmds)) -> do
            replies <- runCommands st (Just client) cmds
            forM_ replies $ \case
                ROutput out -> send client (Message out)
                RErr e -> send client (ServerError e)
            -- Report done only once any layout change the command made has
            -- been reconciled into the panes, so a caller that immediately
            -- inspects a pane's size never races the pty resize.
            awaitReconciled st
            send client CommandDone
            controlLoop st client
        Just (Known Detach) -> pure ()
        Just (Malformed _) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop st client

