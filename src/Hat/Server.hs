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
    , ReloadRequest (..)   -- ^ exported for the reload argument-parse test
    , parseReloadArgs      -- ^ exported for the reload argument-parse test
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
    , dispatch  -- ^ exported for the handshake test
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
    , WindowFate (..)
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
    , Renumbered (..)   -- ^ exported for the move-window -r renumbering tests
    , renumberSession
    , cmdMoveWindow     -- ^ exported for the move-window -r wiring test
    , CaptureOpts (..)  -- ^ exported for the capture-pane grid-dump tests
    , CaptureRow (..)
    , captureBounds
    , captureText
    , cmdCapturePane
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (link, race, withAsync)
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, catch, displayException,
     throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Socket qualified as N
import System.Directory
    (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import System.Posix.IO qualified as PIO
import System.Process
    (CreateProcess (..), StdStream (..), proc,
     readCreateProcess, withCreateProcess)

import Hat.Command.Parser (parseConfig)
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Path (hatPath, render, (</:>))
import Hat.Server.Reload (Handover (..), ReloadCleanup (..))
import Hat.Server.Command.Layout
import Hat.Server.Command.Option
import Hat.Server.Command.Pane
import Hat.Server.Command.Session
import Hat.Server.Command.Window
import Hat.Server.Command.Types
    (Reply (..))
import Hat.Server.ClientIO (broadcast)
import Hat.Server.ColorScheme
    ( ColorScheme (..), WatcherFault (..), applyPalette, parseSchemeLine
    , watcherFault, withRegisteredMonitor )
import Hat.Server.FormatEnv
    (WindowFlagState (..), refreshAutoNames, windowFlags)
import Hat.Server.Handover
import Hat.Server.Conn
import Hat.Server.Dispatch
import Hat.Server.HookMonitor (monitorLoop)
import Hat.Server.Startup
import Hat.Server.Toast
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.Pane
import Hat.Server.Snapshot
import Hat.Server.Resize
import Hat.Server.Title (TitleParts (..), composeTitle)
import Hat.Transport.Socket (ensureSocketDir, listenOn)
import Hat.Term.Emulator qualified as Emu
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
    installHooks st
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
                , monitorLoop st                  -- set-hook -B format monitors
                ] <> [ persistLoop st p | p <- maybe [] pure mstore ]
        -- Last-resort trace: an exception escaping the accept loop or a linked
        -- daemon takes the process down (e.g. a reload's resume fault), and
        -- otherwise vanishes to stderr with nothing in the log. Record its
        -- 'displayException' (backtrace included) and flush before it dies,
        -- then re-raise so the process still exits.
        r <- withDaemons daemons (race (acceptLoop dispatch st lsock) (waitIdle st))
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

-- Configuration --------------------------------------------------------

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
