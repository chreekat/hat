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
    (IOException, SomeException, bracket, catch, displayException,
     finally, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Socket as N
import System.Directory
    (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import qualified System.Posix.IO as PIO
import System.Process
    (CreateProcess (..), StdStream (..), proc,
     readCreateProcess, withCreateProcess)

import Hat.Command.Parser (parseConfig)
import Hat.Geometry
import Hat.Log
import Hat.Server.Environ
    ( environUpdate )
import Hat.Model
import Hat.Model.Options
import Hat.Path (hatPath, render, (</:>))
import Hat.Server.Reload (Handover (..), ReloadCleanup (..))
import qualified Hat.Term.Pty
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.Command.Layout
import Hat.Server.Command.Option
import Hat.Server.Command.Pane
import Hat.Server.Command.Session
import Hat.Server.Command.Window
import Hat.Server.Command.Types
    (Reply (..))
import Hat.Server.ClientIO (broadcast, send)
import Hat.Server.ColorScheme
    ( ColorScheme (..), WatcherFault (..), applyPalette, parseSchemeLine
    , watcherFault, withRegisteredMonitor )
import Hat.Server.FormatEnv (refreshAutoNames)
import Hat.Server.Handover
import Hat.Server.Dispatch
import Hat.Server.Overlay
import Hat.Server.Startup
import Hat.Server.Toast
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.Keys
import Hat.Server.Locate
import Hat.Server.Pane
import Hat.Server.Render
import Hat.Server.Snapshot
import Hat.Server.Resize
import Hat.Server.Title (TitleParts (..), composeTitle)
import Hat.Server.View
    (WindowFlagState (..), renderLoop, windowFlags)
import Hat.Transport.Socket (ensureSocketDir, listenOn)
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
        (Just pk, _) -> handlePickerInput dispatch st client pk (tokenizeKeys bs)
        (_, Just pr) -> handlePromptInput dispatch st client pr bs
        _            -> handleKeys st client bs

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
                            forM_ cmds $ \argv ->
                                toastReplies st client
                                    =<< runArgv st (Just client) argv
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
