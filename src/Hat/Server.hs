-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients, streams frame diffs at them, and runs the command engine
-- that configs, bindings, and @hat <command>@ all share.
module Hat.Server
    ( runServer
    , resumeServer  -- ^ the reload re-exec re-enters here with a handover file
    , captureReloadScreen  -- ^ exported for the reload-screen round-trip test
    , ScrollbackCarry (..)  -- ^ exported for the reload-screen round-trip test
    , replayPane           -- ^ exported for the reload-screen round-trip test
    , reloadSchemePush     -- ^ exported for the reload scheme re-push test
    , rebuildReloadSession -- ^ exported for the reload session-size test
    , captureSize          -- ^ exported for the oversized-capture adopt test
    , cmdRestartServer     -- ^ exported for the reload-in-progress guard test
    , cmdRestart           -- ^ exported for the restart failure-abort test
    , ReloadScope (..)     -- ^ exported for the restart farewell test
    , reloadFarewell       -- ^ exported for the restart farewell test
    , runCommands          -- ^ exported for the restart dispatch test
    , restartClientAction  -- ^ exported for the restart-client no-op test
    , RestartClientOutcome (..)
    , cmdRestartClient     -- ^ exported for the restart-client delivery test
    , Reply (..)           -- ^ exported for the reload-in-progress guard test
    , setOption  -- ^ exported for the config-load burn-down test
    , SetMode (..)  -- ^ exported for the config-load burn-down test
    , chooseScope  -- ^ exported for the scope-routing test
    , SetScope (..)
    , SetDefault (..)
    , listingLines  -- ^ exported for the show-options listing test
    , finallyReady  -- ^ exported for the startup-gate test
    , startupGate, StartupGate (..)  -- ^ exported for the startup-gate test
    , phaseAfterConfig  -- ^ exported for the startup-gate test
    , welcome  -- ^ exported for the handshake test
    , readConfigUtf8  -- ^ exported for the config-encoding test
    , cmdAttachSession  -- ^ exported for the session re-anchor test
    , cmdSourceFile  -- ^ exported for the reload tilde-expansion test
    , PaneStart (..)  -- ^ exported for the restore-argv test
    , SpawnOrigin (..)  -- ^ exported for the restore-argv test
    , restoreRun      -- ^ exported for the restore-argv test
    , shellLine       -- ^ exported for the typed-command test
    , defaultRestoreCommands  -- ^ exported for the restore-argv test
    , pickActivityTarget  -- ^ exported for the activity-jump test
    , pickAttachSession  -- ^ exported for the attach-to-last-active test
    , persistDecision  -- ^ exported for the store-pinning test
    , PersistDecision (..)
    , StorePin (..)
    , uniquifySessionNames  -- ^ exported for the snapshot-restore naming test
    , snapshotHistoryLimit  -- ^ exported for the snapshot-limit option test
    , windowFlags  -- ^ exported for the window-flags test
    , WindowFlagState (..)
    , defaultKeymap  -- ^ exported for the copy-mode binding test
    , applySessionSize  -- ^ exported for the aggressive-resize test
    , reencodeCursor    -- ^ exported for the cursor-key encoding test
    , awaitReconciled  -- ^ exported for the reconcile-barrier test
    , awaitReconcileTick  -- ^ exported for the command-batch gate test
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
    , escTiming  -- ^ exported for the escape-time effect test
    , toastDeadline  -- ^ exported for the display-time effect test
    , toastExpired   -- ^ exported for the display-time effect test
    , sessionSpawnEnv  -- ^ exported for the spawn-env merge test
    , cmdSetEnvironment   -- ^ exported for the environment command tests
    , cmdShowEnvironment  -- ^ exported for the environment command tests
    , cmdListClients      -- ^ exported for the attached-clients listing test
    , refreshSessionEnv   -- ^ exported for the attach -E test
    , nextZoom  -- ^ exported for the zoom-alternate-pane test
    , zoomTarget  -- ^ exported for the solo-pane zoom test
    , nextFreeWindowIndex  -- ^ exported for the base-index window-numbering test
    , placeWindow  -- ^ exported for the new-window placement tests
    , Insert (..)
    , Replace (..)
    , WindowPlacement (..)
    , applyShifts
    , selectNamed
    , CaptureOpts (..)  -- ^ exported for the capture-pane grid-dump tests
    , CaptureRow (..)
    , captureBounds
    , captureText
    , cmdCapturePane
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (link, race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, bracket_, catch, displayException,
     finally, throwIO, try)
import Control.Monad (filterM, forM, forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.Socket as N
import System.Directory
    (doesFileExist, findExecutable, removeFile,
     renameFile)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import qualified System.Posix.IO as PIO
import System.Posix.Process (executeFile)
import System.Posix.Signals (sigHUP, signalProcess)
import System.Posix.Types (Fd (..))
import System.Process
    (CreateProcess (..), StdStream (..), proc,
     readCreateProcess, readCreateProcessWithExitCode, shell,
     withCreateProcess)

import Hat.Command.Parser (parseCommandLine, parseConfig)
import Hat.Geometry
import Hat.Log
import Hat.Server.Environ
    ( EnvVisibility (..)
    , environFromPairs, environSet, environUpdate )
import Hat.Model
import Hat.Model.Options
import Hat.Path (expandTilde, hatPath, render, (</:>))
import Hat.Server.Reload
    (Handover (..), ReloadCleanup (..), ReloadModes (..), ReloadPane (..)
    , ReloadScreen (..), ReloadSession (..), ReloadState (..), ReloadWindow (..)
    , decodeHandover, encodeHandover)
import qualified Hat.Term.Pty
import Hat.Server.Command.Bind (cmdBind, cmdUnbind)
import Hat.Server.Command.Buffer
import Hat.Server.Command.Interact
import Hat.Server.Command.Layout
import Hat.Server.Command.Option
import Hat.Server.Command.Pane
import Hat.Server.Command.Session
import Hat.Server.Command.Window
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.ClientIO (broadcast, send)
import Hat.Server.ColorScheme
    ( ColorScheme (..), WatcherFault (..), applyPalette, parseSchemeLine
    , reapMonitor, schemeReport, watcherFault, withRegisteredMonitor )
import Hat.Server.FormatEnv (paneFormatEnv, refreshAutoNames)
import Hat.Server.WindowStruct (WindowStruct (..), windowStruct)
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.Keys
import Hat.Server.Layout
import Hat.Server.LayoutString (layoutFromString, layoutSize)
import Hat.Server.Locate
import Hat.Server.Pane
import qualified Hat.Server.Picker as Picker
import qualified Hat.Server.Prompt as Prompt
import Hat.Server.Render
import Hat.Server.Snapshot
import Hat.Server.Resize
import qualified Hat.Server.Target as Target
import Hat.Server.Title (TitleParts (..), composeTitle)
import Hat.Server.View
    (WindowFlagState (..), expandFormat, renderLoop, sessionFormatEnv,
     windowFlags)
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
        -- the config is still running; 'startupGate' says who is served in
        -- each phase, and the idle-exit waits for Ready.
        -- Armed before the accept loop can serve anyone.
        atomically $ writeTVar st.startupPhase LoadingConfig
        _ <- forkIO $ finallyReady st $
            (do loadConfig st mconfig
                atomically $ writeTVar st.startupPhase
                    (phaseAfterConfig (persistOn || isJust mreload))
                case mreload of
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
        titlesRef <- newIORef Map.empty
        -- Background daemons run under 'withDaemons': each is bracketed by
        -- 'withAsync' (all torn down when the serve loop returns — no leaked
        -- threads, and the persist mirror stops before the store is dropped)
        -- and 'link'ed, so an unexpected fault re-raises here rather than
        -- vanishing. Each daemon catches its own expected failures, so a link
        -- fires only on a genuine bug.
        -- Keep status-line clocks fresh, at the resolved @status-interval@
        -- (the fastest across sessions); 0 disables the periodic redraw
        -- (re-polled each second so a later set re-enables it).
        let clockDaemon = forever $ do
                miv <- atomically (statusRefreshInterval st)
                case miv of
                    Nothing -> threadDelay 1_000_000
                    Just iv -> do
                        threadDelay (iv * 1_000_000)
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
                -- retire it. The tree drained (every window closed), so the
                -- next start must be pristine — unless kill-server asked to
                -- keep the tree for a restore.
                preserve <- readTVarIO st.preserveStore
                unless preserve $ forM_ mstore (retireStore st)
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

-- | Run the startup action (config load + restore), then land the phase at
-- 'Ready' — always, even if it throws. A phase stuck short of Ready parks
-- every attach forever on 'ensureSession'\'s retry, so the landing must be
-- structural (a @finally@), never a line a crash can skip.
finallyReady :: ServerState -> IO a -> IO a
finallyReady st act =
    act `finally` atomically (writeTVar st.startupPhase Ready)

-- | The phase after the config has drained: straight to 'Ready' unless a
-- persisted tree or reload handover still has to be rebuilt.
phaseAfterConfig :: Bool -> StartupPhase
phaseAfterConfig hasRestoreWork = if hasRestoreWork then Restoring else Ready

-- | What 'startupGate' decides for a command batch.
data StartupGate = Proceed | Hold
    deriving (Eq, Show)

-- | Whether a client's command batch may run in the given startup phase.
-- During 'LoadingConfig' only the client that spawned the server is held —
-- it raced the config for the right to create the first session (upstream
-- if-shell-TERM.sh), while a client the config itself spawned (a nested
-- @hat run@ in an @if-shell@ condition) must be served or config load
-- deadlocks on its own child. 'Restoring' holds everyone: a command must
-- see the whole restored tree, not one mid-rebuild. A reload batch
-- (@restart-server@/@restart@) always proceeds — 'cmdReload' must REJECT
-- an in-flight reload, and holding it here would turn that reject into a
-- silent wait.
startupGate :: StartupPhase -> Autostart -> [[Text]] -> StartupGate
startupGate phase origin cmds
    | any isReload cmds = Proceed
    | otherwise = case (phase, origin) of
        (Ready, _) -> Proceed
        (Restoring, _) -> Hold
        (LoadingConfig, Autostarted) -> Hold
        (LoadingConfig, Joined) -> Proceed
  where
    isReload (name : _) = name `elem` ["restart-server", "restart"]
    isReload []         = False

-- | Park a command batch until 'startupGate' lets it through.
awaitStartup :: ServerState -> Autostart -> [[Text]] -> IO ()
awaitStartup st origin cmds = atomically $ do
    phase <- readTVar st.startupPhase
    case startupGate phase origin cmds of
        Proceed -> pure ()
        Hold -> retry

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
    , idlePhase    :: StartupPhase
    , idleSessions :: Int
    , idleClients  :: Int
    , idlePanes    :: Int
    }

-- | Whether the server may exit: it has served at least one client, startup
-- has fully landed at 'Ready', and no sessions, clients, or live panes are
-- left. The live-pane term is what keeps a drained server alive until every
-- child it spawned has been reaped — since 'cmdKillPane' detaches a pane
-- from the model before its child is reaped, an empty session map no longer
-- implies the children are gone, and exiting first would orphan a
-- SIGHUP-ignoring child mid-'reapPane'. See 'waitIdle'.
serverIdle :: IdleInputs -> Bool
serverIdle i =
    i.idleAttached && i.idleServed && i.idlePhase == Ready
        && i.idleSessions == 0 && i.idleClients == 0 && i.idlePanes == 0

-- Exit once every session is gone, every attached client has drained and
-- disconnected (so nobody's final Exited message is cut off), and every
-- pane's child has been reaped (so a drain never orphans one). See 'serverIdle'.
waitIdle :: ServerState -> IO ()
waitIdle st = atomically $ do
    inputs <- IdleInputs
        <$> readTVar st.everAttached
        <*> readTVar st.served
        <*> readTVar st.startupPhase
        <*> (Map.size <$> readTVar st.sessions)
        <*> (Map.size <$> readTVar st.clients)
        <*> readTVar st.livePanes
    check (serverIdle inputs)



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
-- teardown stays clean. At server shutdown the daemon's cancel unwinds
-- 'withCreateProcess', terminating the monitor subprocess; across a reload the
-- execve escapes that cleanup, so 'cmdRestartServer' reaps the registered
-- monitor first (see 'withRegisteredMonitor', 'reapMonitor').
watchColorScheme :: ServerState -> IO ()
watchColorScheme st = do
    atomically (readTVar st.startupPhase >>= check . (/= LoadingConfig))
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
                { std_out = CreatePipe, close_fds = True } $ \_ mout _ ph ->
            -- Register the child so a reload's pre-exec 'reapMonitor' can kill
            -- it (the execve escapes this bracket's cleanup); deregistered
            -- when this scope ends, so the registry never outlives the child.
            withRegisteredMonitor st.monitorRegistry ph $
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

-- | The color-scheme report a reload must re-push into an adopted pane so a
-- surviving app reacts to the OS scheme immediately, not only on its next
-- change. Only a pane whose app held the ?2031 subscription gets one, and only
-- when the server already knows the scheme; otherwise 'Nothing' (nothing to
-- push). The subscription itself is re-armed separately, by 'replayPane'. See
-- 'adoptPane'.
reloadSchemePush :: ReloadModes -> Maybe ColorScheme -> Maybe B.ByteString
reloadSchemePush rm mscheme
    | rm.colorReport = schemeReport <$> mscheme
    | otherwise      = Nothing

-- Reload: capture the live tree with its inherited handles, and rebuild it
-- in the re-exec'd image by adopting them ------------------------------------

-- | Capture the running tree into the two halves of a handover: the evolving
-- 'ReloadState' (the same structure 'captureSnapshot' records, plus each
-- pane's live pty master fd and child pid), and the version-independent
-- 'ReloadCleanup' core (the listening socket fd and the flat list of every
-- pane's (master fd, child pid), so a version-mismatched reload can hang the
-- inherited processes up cleanly rather than orphan them).
-- | Whether a reload's handover carries the panes' scrollback; see
-- 'captureReloadScreen'.
data ScrollbackCarry = KeepScrollback | DropScrollback
    deriving (Eq, Show)

captureReload :: ScrollbackCarry -> ServerState -> IO (ReloadCleanup, ReloadState)
captureReload carry st = do
    (sess, laName, lsName, mfd) <- atomically $ do
        sessMap <- readTVar st.sessions
        laId    <- readTVar st.lastActiveSession
        laName  <- traverse (readTVar . (.name)) (laId >>= (`Map.lookup` sessMap))
        lsId    <- readTVar st.lastSession
        lsName  <- traverse (readTVar . (.name)) (lsId >>= (`Map.lookup` sessMap))
        mfd     <- readTVar st.listenFd
        pure (Map.elems sessMap, laName, lsName, mfd)
    rsessions <- mapM (captureReloadSession carry) sess
    let tree = ReloadState rsessions laName lsName
        liveHandles =
            [ (p.masterFd, p.childPid)
            | s <- rsessions, w <- s.windows, p <- w.panes ]
        cleanup = ReloadCleanup
            { listenFd = fromMaybe (-1) mfd, live = liveHandles }
    pure (cleanup, tree)

captureReloadSession :: ScrollbackCarry -> Session -> IO ReloadSession
captureReloadSession carry s = do
    (nm, cwd, curIx, winHist, wstructs) <- atomically $ do
        nm    <- readTVar s.name
        cwd   <- readTVar s.startCwd
        curIx <- readTVar s.currentIx
        winHist <- readTVar s.windowHist
        eff   <- readTVar s.lastSize
        ws    <- Map.toAscList <$> readTVar s.windows
        wstructs <- mapM (windowStruct eff) ws
        pure (nm, cwd, curIx, winHist, wstructs)
    rwins <- mapM (captureReloadWindow carry) wstructs
    pure (ReloadSession nm (T.pack cwd) curIx winHist rwins)

captureReloadWindow :: ScrollbackCarry -> WindowStruct -> IO ReloadWindow
captureReloadWindow carry ws = do
    rpanes <- forM ws.wsPanes $ \pane -> do
        dir <- paneCurrentPath pane
        let Fd fd = Hat.Term.Pty.masterFd pane.pty
        ms <- Emu.modes pane.emulator
        sc <- captureReloadScreen carry pane.emulator
        pure (ReloadPane (T.pack dir) (fromIntegral fd)
                (fromIntegral (Hat.Term.Pty.pid pane.pty)) (reloadModesOf ms) sc)
    pure (ReloadWindow ws.wsIx ws.wsName ws.wsLayout ws.wsActive
            ws.wsLastActive ws.wsAutoRename rpanes)

-- | Freeze a pane's emulator into the reload payload: its live grid and cursor,
-- its alt-screen flag, and its scrollback (oldest line first). 'adoptPane'
-- replays this back into the fresh emulator after a reload. 'DropScrollback'
-- skips the scrollback entirely, so the reload doubles as a memory cleanup.
captureReloadScreen :: ScrollbackCarry -> Emu.Emulator -> IO ReloadScreen
captureReloadScreen carry emu = do
    scr <- Emu.snapshot emu
    m   <- Emu.modes emu
    pen <- Emu.currentPen emu
    sb  <- case carry of
        DropScrollback -> pure []
        KeepScrollback -> do
            len <- Emu.scrollbackLength emu
            catMaybes <$> mapM (Emu.scrollbackLine emu) [0 .. len - 1]
    pure ReloadScreen
        { altScreen     = m.altScreen
        , cursorRow     = scr.cursor.row
        , cursorCol     = scr.cursor.col
        , cursorVisible = scr.cursorVisible
        , rows          = map V.toList (V.toList scr.cells)
        , scrollback    = map V.toList sb
        , pen           = pen
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
    -- crashes the process (e.g. a native abort in the emulator) leaves the exact
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
-- pty and child rather than spawning; each pane's captured screen is replayed
-- into its fresh emulator ('adoptPane').
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
        -- The captured window area (every window was captured at the same
        -- effective size), so the reconcile tick that runs before any client
        -- attaches finds the adopted panes already at their layout size.
        let sz = fromMaybe Size { rows = 24, cols = 80 }
                (listToMaybe (mapMaybe (layoutSize . (.layout)) wins))
        built <- forM wins $ \rwin -> do
            (win, panes) <- rebuildReloadWindow st sz rwin
            pure (rwin.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member rsess.currentIx winMap = rsess.currentIx
                  | otherwise = maybe rsess.currentIx fst (Map.lookupMin winMap)
            winHist = List.nub
                [l | l <- rsess.windowHist, l /= curIx, Map.member l winMap]
        nameVar    <- newTVarIO rsess.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        windowHistVar <- newTVarIO winHist
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO (environFromPairs env)
        cwdVar     <- newTVarIO (T.unpack rsess.startCwd)
        optionsVar <- newTVarIO emptyDelta
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, windowHist = windowHistVar
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
        paneHistPids = List.nub
            [ pids !! o | o <- rwin.paneHist
            , o >= 0, o < length pids, pids !! o /= activePid ]
    nameVar       <- newTVarIO rwin.name
    layoutVar     <- newTVarIO lay
    layoutNameVar <- newTVarIO Nothing
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    paneHistVar   <- newTVarIO paneHistPids
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    autoRenameVar <- newTVarIO rwin.autoRename
    optionsVar    <- newTVarIO emptyDelta
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , paneHist = paneHistVar, bellFlag = bellVar
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
    -- state whose later reflow-resize aborts inside the emulator ("screen_resize
    -- failed to update cursor position", the 2026-07-28 field crash). The
    -- reconcile loop then resizes toward the layout as for any live pane.
    let esz = fromMaybe sz (captureSize rp.screen)
    emu <- Emu.newEmulator esz histLimit
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "replaying" }
    -- Seed the scrollback first, then paint the live grid on top of it, so the
    -- restored viewport sits above the reseeded history. replayBytes also re-arms
    -- the app's ?2031/?1004/mouse subscriptions and re-enters the alt screen when
    -- the program was in it, so a later exit reverts cleanly.
    let (replayBytes, replaySb) = replayPane esz rp
    Emu.seedScrollback emu replaySb
    _ <- Emu.feed emu replayBytes
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "ready" }
    sizeVar   <- newTVarIO esz
    deadVar   <- newTVarIO False
    modeVar   <- newTVarIO Nothing
    pipeVar   <- newTVarIO Nothing
    readerVar <- newTVarIO Nothing
    optionsVar <- newTVarIO emptyDelta
    let pane = Pane
            { id = pid, pty = pty, emulator = emu, size = sizeVar
            , dead = deadVar, startCwd = T.unpack rp.cwd, mode = modeVar
            , options = optionsVar
            , pipe = pipeVar, readerTid = readerVar, pendingInput = Nothing }
    -- A surviving app that held the ?2031 subscription never re-emits it across
    -- the reload, so re-push the current scheme once — otherwise it renders the
    -- pre-reload scheme until the OS scheme next changes (bug f3).
    scheme <- readTVarIO st.colorScheme
    forM_ (reloadSchemePush rp.modes scheme) (Hat.Term.Pty.writePty pty)
    pure pane

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
replayPane :: Size -> ReloadPane -> (B.ByteString, [V.Vector Cell.Cell])
replayPane sz rp =
    ( Emu.modeReplayBytes (emuModesOf rp.modes)
        <> Emu.restoreBytes restoreModes rp.screen.pen (screenOf sz rp.screen)
    , map V.fromList rp.screen.scrollback )
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

-- Configuration --------------------------------------------------------

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
        Just (Known (ClientHello h)) -> case negotiate h.protoVersion of
            Right _ -> welcome st conn h
            Left e  -> sendMessage conn (ServerError e)
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: ServerState -> N.Socket -> Hello -> IO ()
welcome st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            -- After Welcome: a client validates its one greeting strictly,
            -- then skips unknown tags.
            sendMessage conn (ServerVersion protocolVersion)
            atomically $ writeTVar client.ready True
            controlLoop st client `finally` removeClient st client
        AttachIntent setupCmds -> do
            -- Register early so the setup commands (new-session,
            -- attach-session -t) act on a live client and can switch it.
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            -- Gate here, not only in ensureSession: setup commands run
            -- directly, so an autostarting `hat new` would otherwise race
            -- its own server's config load (upstream if-shell-TERM.sh).
            awaitStartup st client.autostart setupCmds
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
                    sendMessage conn (ServerVersion protocolVersion)
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
            writeTVar client.sessionHist [g]
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
    sessionHistVar <- newTVarIO []
    keyVar <- newIORef NoPrefix
    escVar <- newIORef NoEscPending
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    cursorColourVar <- newIORef ""
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    readyVar <- newTVarIO False
    focusVar <- newTVarIO True
    envImportVar <- newTVarIO ImportEnv
    pure Client
        { id = ClientId cid
        , role = case h.intent of
            AttachIntent {} -> Attached
            ControlIntent   -> Control
        , autostart = if h.autostarted then Autostarted else Joined
        , sock = conn
        , wireLevel = min protocolVersion h.protoVersion
        , sendLock = sendLock
        , size = sizeVar
        , lastActive = activeVar
        , session = sessVar
        , sessionHist = sessionHistVar
        , ready = readyVar
        , keyState = keyVar
        , escState = escVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , lastCursorColour = cursorColourVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , picker = pickerVar
        , outerFocused = focusVar
        , envImport = envImportVar
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

-- | On attach, import the @update-environment@ vars from the attaching
-- client's env into the session ('environUpdate'), so panes spawned
-- afterward see fresh values (e.g. a new @DISPLAY@ after reconnecting
-- over @ssh -X@). An @attach-session -E@ marks the client to skip this.
refreshSessionEnv :: ServerState -> Session -> Client -> IO ()
refreshSessionEnv st sess client = readTVarIO client.envImport >>= \case
    SkipEnvImport -> pure ()
    ImportEnv -> do
        vars <- (.updateEnvironment) <$> readTVarIO st.options
        atomically $ modifyTVar' sess.environ (environUpdate vars client.env)

ensureSession :: ServerState -> Client -> IO Session
ensureSession st client = do
    -- Let startup land first, so we attach to the configured, restored tree
    -- rather than racing it and creating a redundant fresh session.
    atomically $ readTVar st.startupPhase >>= \p -> when (p /= Ready) retry
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

-- Input ---------------------------------------------------------------------

inputLoop :: ServerState -> Client -> IO ()
inputLoop st client = loop
  where
    loop = do
        -- A lone trailing ESC held by escape-time races the next chunk: if
        -- nothing arrives within the window, flush it as the Escape key.
        held <- readIORef client.escState
        m <- case held of
            NoEscPending -> Right <$> recvMessage client.sock
            EscPending -> do
                opts <- clientOptions st client
                timer <- registerDelay (opts.escapeTime * 1000)
                race (atomically (readTVar timer >>= check)) (recvMessage client.sock)
        case m of
            Left () -> flushHeldEscape st client >> loop
            Right Nothing -> pure ()
            Right (Just (Malformed err)) -> logEvent st.logger ProtocolError
                { client = rawClient client.id, err = T.pack err }
            Right (Just (UnknownTag _)) -> loop
            Right (Just (Known msg)) -> case msg of
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
-- escape-time coalescing: 0 forwards a lone trailing ESC as Escape at once
-- ('EscImmediate'); a non-zero value holds it ('EscBuffered') for 'inputLoop'
-- to coalesce with the next chunk or flush on timeout.
escTiming :: Options -> EscTiming
escTiming opts = if opts.escapeTime <= 0 then EscImmediate else EscBuffered

-- | Tokenize a chunk under the client's escape-time, coalescing any ESC held
-- from the previous chunk, then route the resulting keys. A fresh lone
-- trailing ESC is left in 'escState' for 'inputLoop' to resolve.
handleKeys :: ServerState -> Client -> B.ByteString -> IO ()
handleKeys st client bs = do
    opts <- clientOptions st client
    held <- readIORef client.escState
    let toks = feedKeys (escTiming opts) held bs
    writeIORef client.escState toks.escPending
    runKeys st client toks.escKeys

-- | The escape-time timer fired: forward the held lone ESC as Escape and clear
-- the buffer. A no-op if the ESC was already coalesced by an arriving chunk.
flushHeldEscape :: ServerState -> Client -> IO ()
flushHeldEscape st client = do
    held <- readIORef client.escState
    writeIORef client.escState NoEscPending
    runKeys st client (flushEscape held)

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
runKeys :: ServerState -> Client -> [Key] -> IO ()
runKeys st client keys = do
    opts <- clientOptions st client
    km <- readTVarIO st.keymap
    let loop kst [] = writeIORef client.keyState kst
        loop kst (k0 : rest) = do
            dismissToast st client
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
                        RunCommands cmds -> withCommandBatch st $
                            forM_ cmds $ \argv -> do
                            replies <- runArgv st (Just client) argv
                            forM_ replies $ \case
                                ROutput out -> showToast st client out
                                RErr e -> showToast st client ("error: " <> e)
                    loop kst' rest
    st0 <- readIORef client.keyState
    loop st0 keys
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
                -- Neither command can fail, so the replies are empty.
                void $ if T.length key.name == 1
                    then runCopyModeCommand st pane "apply-search" [key.name]
                    else runCopyModeCommand st pane "cancel-search" []
                pure True
            _ -> pure False

-- | Forward a recognized key as the pane's advertised terminal expects, not
-- as the outer terminal's incidental bytes. The arrows are DECCKM-dependent
-- (@\ESC[A@ vs @\ESCOA@), so the pane's emulator encodes them; every other
-- named key forwards its tmux-256color terminfo bytes from 'namedKeys'.
-- Unrecognized keys keep their raw bytes.
reencodeCursor :: Maybe Pane -> Key -> IO Key
reencodeCursor mpane key = case arrowOf key.name of
    Just ck | Just pane <- mpane -> do
        enc <- Emu.encodeKey pane.emulator ck
        pure key { raw = enc }
    _ | Just canon <- lookup key.name namedKeys -> pure key { raw = canon }
    _ -> pure key
  where
    arrowOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        _       -> Nothing

showToast :: ServerState -> Client -> Text -> IO ()
showToast st client t = do
    opts <- clientOptions st client
    now <- getMonotonicTimeNSec
    let toast = Toast { text = t, deadline = toastDeadline opts now }
    atomically $ do
        writeTVar client.toast (Just toast)
        bumpDirty st
    forM_ toast.deadline $ \deadline -> void . forkIO $ do
        -- round up so the wakeup lands past the deadline
        threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
        expireToast st client

-- | @display-time@: the monotonic instant (ns) a toast shown at @shownAt@
-- times out, or 'Nothing' for @0@ = until a key is pressed.
toastDeadline :: Options -> Word64 -> Maybe Word64
toastDeadline opts shownAt
    | opts.displayTime <= 0 = Nothing
    | otherwise = Just (shownAt + fromIntegral opts.displayTime * 1000000)

-- | Whether a toast's deadline has passed at monotonic instant @now@.
toastExpired :: Word64 -> Toast -> Bool
toastExpired now toast = maybe False (<= now) toast.deadline

-- | Clear the toast once its deadline has passed; a fresher toast (a later
-- deadline, or none) survives an older toast's timer.
expireToast :: ServerState -> Client -> IO ()
expireToast st client = do
    now <- getMonotonicTimeNSec
    atomically $ do
        cur <- readTVar client.toast
        forM_ cur $ \toast -> when (toastExpired now toast) $ do
            writeTVar client.toast Nothing
            bumpDirty st

-- | Clear the toast on a key press — the only dismissal under
-- @display-time 0@.
dismissToast :: ServerState -> Client -> IO ()
dismissToast st client = atomically $ do
    cur <- readTVar client.toast
    forM_ cur $ \_ -> do
        writeTVar client.toast Nothing
        bumpDirty st

-- The command engine ---------------------------------------------------------

runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = case parseCommandLine input of
    Left err -> pure [RErr err]
    Right cmds -> runCommands st mclient cmds

runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
runCommands st mclient cmds =
    withCommandBatch st (concat <$> mapM (runArgv st mclient) cmds)

-- | Run a user action's whole command sequence as one batch: 'reconcileLoop'
-- holds off sizing panes until every command has committed (see
-- 'awaitReconcileTick'). Nestable — an @if-shell@ that runs more commands just
-- deepens the count; only the outermost exit reopens reconciliation.
withCommandBatch :: ServerState -> IO a -> IO a
withCommandBatch st = bracket_
    (atomically (modifyTVar' st.commandDepth (+ 1)))
    (atomically (modifyTVar' st.commandDepth (subtract 1)))

runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
runArgv _ _ [] = pure []
runArgv st mclient (name : args) = do
    -- Attribute every command to its issuing client, so a duplicate or
    -- unexpected control command (e.g. a second restart-server nobody typed)
    -- is traceable to its source.
    logEvent st.logger CommandRun
        { client = maybe (-1) (\c -> rawClient c.id) mclient
        , command = T.unwords (name : args) }
    case envAssignment (name : args) of
        Just (vis, n, v) -> [] <$ atomically
            (modifyTVar' st.globalEnviron (environSet vis n v))
        Nothing -> case Map.lookup name commandTable of
            Nothing -> pure [RErr ("unknown command: " <> name)]
            Just impl -> impl st mclient args
                `catch` \(e :: SomeException) ->
                    pure [RErr (name <> ": " <> T.pack (show e))]

-- | The config assignment forms (tmux's environ_put): a bare @NAME=value@
-- line sets a global environment variable, @%hidden NAME=value@ a hidden
-- one. Anything not shaped exactly like these stays a command lookup, so a
-- typo still fails loud.
envAssignment :: [Text] -> Maybe (EnvVisibility, Text, Text)
envAssignment = \case
    [w]            -> assign EnvVisible w
    ["%hidden", w] -> assign EnvHidden w
    _              -> Nothing
  where
    assign vis w = case T.breakOn "=" w of
        (n, rest)
            | Just v <- T.stripPrefix "=" rest
            , isVarName n -> Just (vis, n, v)
        _ -> Nothing
    isVarName n = case T.uncons n of
        Just (c, cs) -> (isAlpha c || c == '_')
            && T.all (\x -> isAlphaNum x || x == '_') cs
        Nothing -> False

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList $ concatMap expand
    [ (["bind-key", "bind"], cmdBind)
    , (["unbind-key", "unbind"], cmdUnbind)
    , (["set-option", "set"], cmdSet DefaultSession)
    , (["set-window-option", "setw"], cmdSet DefaultWindow)
    , (["show-options", "show", "show-option"], cmdShow)
    , (["set-environment", "setenv"], cmdSetEnvironment)
    , (["show-environment", "showenv"], cmdShowEnvironment)
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
    , (["link-window", "linkw"], cmdLinkWindow)
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
    , (["list-clients", "lsc"], cmdListClients)
    , (["list-sessions", "ls"], cmdListSessions)
    , (["list-windows", "lsw"], cmdListWindows)
    , (["list-panes", "lsp"], cmdListPanes)
    , (["capture-pane", "capturep"], cmdCapturePane)
    , (["resize-window", "resizew"], cmdResizeWindow)
    , (["switch-client", "switchc"], cmdSwitchClient)
    , (["kill-server"], cmdKillServer)
    , (["list-snapshots"], cmdListSnapshots)
    , (["restore-snapshot"], cmdRestoreSnapshot)
    , (["restart-server"], cmdRestartServer)
    , (["restart-client"], cmdRestartClient)
    , (["restart"], cmdRestart)
    , (["display-message", "display"], cmdDisplayMessage)
    , (["run-shell", "run"], cmdRunShell)
    , (["if-shell", "if"], cmdIfShell)
    ]
  where
    expand (names, impl) = [(n, impl) | n <- names]

-- Command implementations.

-- | Whether @set@\/@set-option@ (session) or @setw@\/@set-window-option@
-- (window) invoked the set: the default scope when no @-g@\/@-s@\/@-w@ picks
-- one. See 'cmdSet'.
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

-- | @restart-server [-C] [path]@: reload the server binary in place. See
-- 'cmdReload'; dropped clients exit and the users reattach.
cmdRestartServer :: CommandImpl
cmdRestartServer = cmdReload ServerOnly

-- | @restart [-C] [path]@: @restart-server@ with the attached clients told to
-- re-exec too ('reloadFarewell'), so one command upgrades both halves.
cmdRestart :: CommandImpl
cmdRestart = cmdReload ServerAndClients

-- | Which halves a reload restarts: the server alone (@restart-server@), or
-- the server and every attached client (@restart@). See 'reloadFarewell'.
data ReloadScope = ServerOnly | ServerAndClients
    deriving (Eq, Show)

-- | The command name a reload reports its errors under.
reloadName :: ReloadScope -> Text
reloadName ServerOnly = "restart-server"
reloadName ServerAndClients = "restart"

-- | The farewell 'cmdReload' sends each client it is about to drop: under
-- 'ServerAndClients' an attached client re-execs in place ('RestartClient',
-- handled like @restart-client@); everyone else exits.
reloadFarewell :: ReloadScope -> ClientRole -> ServerToClient
reloadFarewell ServerAndClients Attached = RestartClient
reloadFarewell _ _ = Exited

-- | Reload the server binary in place while every pane's program keeps
-- running. @-C@ drops all scrollback across the reload (a memory cleanup).
-- Serializes the live tree and its inherited fds to a handover file, sends
-- every client its 'reloadFarewell', then re-execs @path@ (default: the
-- on-PATH @hat@, see 'resolveReloadTarget'), which re-adopts the tree
-- ('resumeServer'). The pane pty masters and the listening socket survive the
-- exec, so a farewelled client that reattaches immediately just waits in the
-- accept backlog; the accepted sockets are close-on-exec, so dropped clients
-- hang up cleanly. A missing target is reported before anything is torn down,
-- so a typo'd path is a harmless error rather than a half-dropped server.
cmdReload :: ReloadScope -> CommandImpl
cmdReload scope st mclient args = do
    -- Fail loud rather than reload on top of an in-flight startup or
    -- reload: capturing a half-rebuilt tree and re-exec'ing through it is
    -- how a second restart-server strands the live programs. Startup lands
    -- at Ready the moment the tree is whole again.
    phase <- readTVarIO st.startupPhase
    if phase /= Ready
        then pure [RErr (reloadName scope <> ": startup or reload still in progress; try again shortly")]
        else cmdReload' scope st mclient args

cmdReload' :: ReloadScope -> CommandImpl
cmdReload' scope st mclient args = do
    let (_, flags, pos) = parseArgs "" args
        carry = if "-C" `elem` flags then DropScrollback else KeepScrollback
    case filter (/= "-C") flags of
      (f : _) -> pure [RErr (reloadName scope <> ": unknown flag: " <> f)]
      [] -> do
        target <- case pos of
            (p : _) -> pure (T.unpack p)    -- explicit binary path (deterministic)
            []      -> resolveReloadTarget  -- default: the on-PATH hat
        exists <- doesFileExist target
        if not exists
            then pure [RErr (reloadName scope <> ": no such binary: " <> T.pack target)]
            else do
                logEvent st.logger ServerReloading { target = target }
                (cleanup, tree) <- captureReload carry st
                let blobPath = st.sockPath <> ".reload"
                B.writeFile blobPath (encodeHandover cleanup tree)
                keepOpenAcrossExec cleanup
                sessions <- readTVarIO st.sessions
                forM_ (Map.keys sessions) $ \sid -> do
                    cs <- atomically (sessionClients st sid)
                    forM_ cs $ \c -> send c (reloadFarewell scope c.role)
                forM_ mclient $ \client ->
                    send client (reloadFarewell scope client.role)
                mconfig <- readTVarIO st.serverConfig
                let argv = ["--server", st.sockPath]
                        <> maybe [] (: []) mconfig
                        <> ["--reload-handover", blobPath]
                -- The execve replaces this image atomically, so no bracket
                -- unwinds: the gsettings monitor child would be orphaned (14 such
                -- orphans accrued on the dev box across upgrades). Reap it here,
                -- while we can still signal it.
                reapMonitor st.monitorRegistry
                -- The self-exec replaces this image, so 'withLogger's
                -- flush-on-exit never runs; drain the queue now or the
                -- reload trace is lost.
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

-- | @restart-client@: tell the currently-attached client to re-exec itself in
-- place, keeping its session. A no-op when the command is not issued from an
-- attached client — a bare @hat restart-client@ from a shell reaches the server
-- as a control connection, so there is nothing rendering to restart.
cmdRestartClient :: CommandImpl
cmdRestartClient _ mclient _ =
    case restartClientAction ((.role) <$> mclient) of
        NoAttachedClient -> pure []
        RestartAttached  -> do
            forM_ mclient $ \client -> send client RestartClient
            pure []

-- | The outcome of 'cmdRestartClient': restart the issuing client, or do
-- nothing because no attached client issued the command.
data RestartClientOutcome = RestartAttached | NoAttachedClient
    deriving (Eq, Show)

-- | Decide 'cmdRestartClient' from the issuing client's role: only an
-- 'Attached' client restarts; a 'Control' connection (or none) is the no-op.
restartClientAction :: Maybe ClientRole -> RestartClientOutcome
restartClientAction = \case
    Just Attached -> RestartAttached
    _             -> NoAttachedClient

-- | @display-message [-p] [-t target] message@. The target is a pane
-- (tmux's @CMD_FIND_PANE@ with @CANFAIL@): the message expands in the
-- resolved pane's format environment, and an unresolvable target
-- degrades to the plain text instead of erroring.
cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        raw = T.unwords pos
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    text <- case res of
        Left _ -> pure raw
        Right (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            env <- paneFormatEnv st sess wix win pix pane
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
            awaitStartup st client.autostart cmds
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


-- | @kill-server [-a]@: drain every session (a normal shutdown). @-a@ keeps
-- the store's saved tree; a bare kill drops it. See 'saveNow'.
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
            case mpane of
                Nothing -> pure []
                Just pane
                    | modeCmd -> case pos of
                        (name : cmdArgs) -> runCopyModeCommand st pane name cmdArgs
                        [] -> pure []
                    | otherwise -> [] <$ Hat.Term.Pty.writePty pane.pty
                        (B.concat (map (argBytes literal) pos))
  where
    argBytes True a = TE.encodeUtf8 a
    argBytes False a = case parseKeyName a of
        Just k -> k.raw
        Nothing -> TE.encodeUtf8 a

runCopyModeCommand :: ServerState -> Pane -> Text -> [Text] -> IO [Reply]
runCopyModeCommand st pane name cmdArgs = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure []  -- not in copy mode; -X is a no-op
        Just pm
            -- A digit key builds the @[count]@ prefix rather than running
            -- a motion; @0@ with no count pending is @start-of-line@.
            | name == "digit", Just d <- readDigit cmdArgs -> do
                atomically $ do
                    writeTVar pane.mode
                        (Just (reMode (CopyMode.pushDigit d state)))
                    bumpDirty st
                pure []
            | otherwise -> case Map.lookup name CopyMode.handlers of
                Nothing -> pure []
                Just h -> do
                    -- Motions repeat [count] times; yanks never do. Every
                    -- command clears the pending count.
                    let count
                            | name `elem` ["copy-selection", "copy-pipe"] = 1
                            | otherwise = min 1000 (maybe 1 (max 1) state.numPrefix)
                    result <- applyN h (state { numPrefix = Nothing }) count
                    case result of
                        -- A failed command leaves the mode untouched.
                        Left err -> pure [RErr err]
                        Right r -> do
                            r' <- traverse (scrollPaneToCursor pane) r
                            atomically $ do
                                writeTVar pane.mode (reMode <$> r')
                                bumpDirty st
                            pure []
          where
            state = pm.copyState
            reMode s = pm { copyState = s }
  where
    readDigit (a : _) = case TR.decimal a of
        Right (d, rest) | T.null rest, d >= 0, d <= 9 -> Just d
        _ -> Nothing
    readDigit [] = Nothing
    -- Run a handler @n@ times, threading the state and stopping early if
    -- it errors or exits copy mode.
    applyN _ s 0 = pure (Right (Just s))
    applyN h s n = do
        r <- h st pane s cmdArgs
        case r of
            Left err -> pure (Left err)
            Right Nothing -> pure (Right Nothing)
            Right (Just s') -> applyN h s' (n - 1)

-- | Re-center a pane's copy-mode viewport on its cursor after a motion,
-- over the pane's frozen snapshot (a no-op when not in copy mode).
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    mmode <- readTVarIO pane.mode
    pure $ case mmode of
        Just pm -> CopyMode.scrollToCursor pm.frozen.fgHsize pm.frozen.fgSy s
        Nothing -> s
