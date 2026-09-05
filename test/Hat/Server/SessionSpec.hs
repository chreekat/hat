module Hat.Server.SessionSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import System.Timeout (timeout)
import Data.Map.Strict qualified as Map
import Network.Socket
    ( Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socket
    , socketPair, touchSocket )
import System.Mem (performGC)
import System.Posix.IO (closeFd)
import System.Posix.Process (getProcessID)
import System.Posix.Terminal (openPseudoTerminal)
import Test.Hspec

import Data.Set qualified as Set
import Data.Vector qualified as V
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Client (nestsOwnServer)
import Hat.Model
import Hat.Model.Options
    (Options (..), OptionName (..), OptionValue (..)
    , defaultOptions, emptyDelta, insertDelta, singletonDelta)
import Hat.Server
    ( DetachResult (..), WindowFate (..), SessionFate (..), IdleInputs (..), serverIdle
    , Reply (..), RestartClientOutcome (..), restartClientAction
    , ReloadScope (..), reloadFarewell
    , applySessionSize, attentionSeen, awaitReconciled, awaitReconcileTick
    , cmdAttachSession, cmdListClients, cmdMoveWindow, cmdRestartClient
    , cmdRestart
    , deliversKey, detachPane, detachPaneCurrent, detachPanes, markActivity
    , markBell, nextZoom, noteOuterFocus, pickActivityTarget, pickAttachSession
    , dispatch, reencodeKey, refreshSessionEnv, removePaneFromTree
    , welcome, windowActivity
    , zoomTarget )
import Hat.Server.Command.Layout (cmdBreakPane, cmdJoinPane)
import Hat.Server.Command.Pane (cmdClearHistory)
import Hat.Server.Command.Session
    (cmdKillSession, cmdNewSession, cmdRenameSession)
import Hat.Server.Command.Window (cmdRenameWindow)
import Hat.Server.Environ
    ( EnvEntry (..), EnvVisibility (..), emptyEnviron, environFind
    , environSet )
import Hat.Server.Snapshot (cmdListSnapshots, cmdRestoreSnapshot)
import Hat.Term.Pty qualified as Pty
import Hat.Server.Keys (EscPending (NoEscPending), Key (..), PrefixState (NoPrefix))
import Hat.Transport.Wire
    ( Autostart (..), ClientToServer (..), Hello (..), Inbound (..), Intent (..)
    , ServerToClient (..), protocolVersion, recvMessage, sendMessage )
import Hat.Server.Layout (Layout (..), Orientation (LeftRight))
import Hat.Server.Render (blankFrame)
import Hat.Server.FormatEnv (paneModeEnv, sessionFormatEnv)
import Hat.Server.View (awaitRenderable, renderOnce, statusCells)

-- A bare session with the given id inserted into an existing server.
addSession :: ServerState -> Int -> IO Session
addSession st n = do
    sess <- Session (SessionId n)
        <$> newTVarIO "s"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO emptyEnviron
        <*> newTVarIO "/"
        <*> newTVarIO emptyDelta
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId n) sess)
    pure sess

-- A bare session with a known default working directory, inserted into a
-- fresh server so 'cmdAttachSession' can target it.
seedSession :: FilePath -> IO (ServerState, Session)
seedSession start = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-sessionspec.sock" Nothing
    sess <- Session (SessionId 0)
        <$> newTVarIO "work"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO emptyEnviron
        <*> newTVarIO start
        <*> newTVarIO emptyDelta
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId 0) sess)
    pure (st, sess)

-- A bare window inserted into a session at the given index, so bell/activity
-- flagging can be exercised against a real session tree.
addWindow :: Session -> Int -> IO Window
addWindow sess n = do
    win <- Window (WindowId n)
        <$> newTVarIO "w"
        <*> newTVarIO (Leaf (PaneId n))
        <*> newTVarIO Nothing
        <*> newTVarIO Map.empty
        <*> newTVarIO (PaneId n)
        <*> newTVarIO []
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO 0
        <*> newTVarIO Nothing
        <*> newTVarIO True
        <*> newTVarIO emptyDelta
    atomically $ modifyTVar' sess.windows (Map.insert n win)
    pure win

-- A client attached to the given session with a fixed size and activity
-- stamp (larger stamp = more recently active). 'applySessionSize' only reads
-- a client's size/lastActive/session and writes needsFull, so the socket is a
-- bare fd that is never driven.
addClient :: ServerState -> SessionId -> Size -> Int -> IO Client
addClient st sid sz stamp = do
    sock <- socket AF_UNIX Stream defaultProtocol
    lock <- newMVar ()
    client <- Client (ClientId stamp) Attached Joined sock protocolVersion lock
        <$> newTVarIO sz
        <*> newTVarIO stamp
        <*> newTVarIO sid
        <*> newTVarIO []
        <*> newTVarIO True
        <*> newIORef NoPrefix
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame sz)
        <*> newIORef (Pos 0 0, True)
        <*> newIORef ""
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO True     -- outerFocused
        <*> newTVarIO ImportEnv
        <*> pure []
        <*> pure ""
    atomically $ modifyTVar' st.clients (Map.insert client.id client)
    pure client

-- A client whose socket is one half of a socketpair, so a test can read what
-- the server sends it. 'ready' is armed so 'send' does not drop the message.
wiredClient :: ServerState -> ClientRole -> IO (Client, Socket)
wiredClient st clientRole = wiredClientEnv st clientRole []

-- 'wiredClient' with a chosen client environment, for update-environment
-- import tests.
wiredClientEnv
    :: ServerState -> ClientRole -> [(Text, Text)] -> IO (Client, Socket)
wiredClientEnv st clientRole clientEnv = do
    (server, peer) <- socketPair AF_UNIX Stream defaultProtocol
    lock <- newMVar ()
    client <- Client (ClientId 1) clientRole Joined server protocolVersion lock
        <$> newTVarIO (Size 24 80)
        <*> newTVarIO 0
        <*> newTVarIO (SessionId 0)
        <*> newTVarIO []
        <*> newTVarIO True    -- ready
        <*> newIORef NoPrefix
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame (Size 24 80))
        <*> newIORef (Pos 0 0, True)
        <*> newIORef ""
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO True
        <*> newTVarIO ImportEnv
        <*> pure clientEnv
        <*> pure ""
    atomically $ modifyTVar' st.clients (Map.insert client.id client)
    pure (client, peer)

-- A model-only pane for exercising 'detachPane', which touches a pane's
-- id and dead flag but never its pty or emulator (those stay with the
-- reader thread's reap). The unused fields trap on access.
stubPane :: Int -> IO Pane
stubPane n = do
    sizeV <- newTVarIO (Size { rows = 24, cols = 80 })
    deadV <- newTVarIO False
    modeV <- newTVarIO Nothing
    pipeV <- newTVarIO Nothing
    tidV  <- newTVarIO Nothing
    optsV <- newTVarIO emptyDelta
    pure Pane
        { id = PaneId n
        , pty = error "stubPane: pty is never touched by detachPane"
        , emulator = error "stubPane: emulator is never touched by detachPane"
        , size = sizeV
        , dead = deadV
        , startCwd = "/"
        , mode = modeV
        , options = optsV
        , pipe = pipeV
        , readerTid = tidV
        , pendingInput = Nothing
        }

-- A 'stubPane' whose pty is a real pty pair adopted around this very process,
-- for paths that probe the pty (break-pane's window-name sniff). The release
-- closes only the slave: 'closePty' would SIGHUP the adopted pid — us.
adoptedPane :: Int -> IO (Pane, IO ())
adoptedPane n = do
    (master, slave) <- openPseudoTerminal
    ptyH <- Pty.adopt master =<< getProcessID
    p <- stubPane n
    pure (p { pty = ptyH }, closeFd slave)

-- An open copy mode over a blank frozen 24x80 grid with @hsize@ scrollback
-- lines, viewing the bottom.
copyModeAt :: Int -> PaneMode
copyModeAt hsize = PaneMode
    { frozen = FrozenGrid
        { fgHsize = hsize, fgSy = 24, fgSx = 80, fgRows = V.empty }
    , copyState = CopyModeState
        { cursorRow = hsize
        , cursorCol = 0
        , selection = Nothing
        , keyTable = "copy-mode"
        , viewportOffY = 0
        , numPrefix = Nothing
        , pendingSearch = Nothing
        , lastSearch = Nothing
        , lastQuery = Nothing
        }
    }

spec :: Spec
spec = do
    -- User options resolve per scope: a session-local @set @v@ is visible
    -- to formats and shadows the global value (upstream format-strings.sh).
    describe "user options in formats" $
        it "a session-local @option shadows the global for #{@v}" $ do
            (st, sess) <- seedSession "/"
            atomically $ do
                modifyTVar' st.options $ \o ->
                    o { user = Map.singleton "@v" "global" }
                modifyTVar' sess.options
                    (insertDelta (OptUser "@v") (OVText "local"))
            env <- sessionFormatEnv st sess
            Map.lookup "@v" env `shouldBe` Just "local"

    describe "session format counts" $
        it "session_attached and session_windows report live counts" $ do
            (st, sess) <- seedSession "/"
            _ <- addWindow sess 0
            _ <- addWindow sess 1
            detached <- sessionFormatEnv st sess
            Map.lookup "session_attached" detached `shouldBe` Just "0"
            Map.lookup "session_windows" detached `shouldBe` Just "2"
            _ <- addClient st sess.id (Size { rows = 24, cols = 80 }) 1
            attached <- sessionFormatEnv st sess
            Map.lookup "session_attached" attached `shouldBe` Just "1"

    describe "pane_in_mode" $
        it "is 1 while copy mode is open, 0 otherwise" $ do
            p <- stubPane 0
            env0 <- paneModeEnv p
            lookup "pane_in_mode" env0 `shouldBe` Just "0"
            lookup "pane_mode" env0 `shouldBe` Just ""
            atomically $ writeTVar p.mode (Just (copyModeAt 3))
            env1 <- paneModeEnv p
            lookup "pane_in_mode" env1 `shouldBe` Just "1"
            lookup "pane_mode" env1 `shouldBe` Just "copy-mode"

    describe "copy-mode position indicator" $
        it "stamps [scroll/history] on the pane's top-right row" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            p <- stubPane 0
            atomically $ do
                modifyTVar' win.panes (Map.insert p.id p)
                writeTVar p.mode (Just (copyModeAt 3))
                -- status off: statusCells would touch the stub pane
                modifyTVar' st.options $ \o -> o { statusLines = 0 }
            (client, _peer) <- wiredClient st Attached
            renderOnce st client
            frame <- readIORef client.lastFrame
            let row0 = T.pack (concatMap Cell.cluster (V.toList (frame V.! 0)))
            row0 `shouldSatisfy` T.isInfixOf "[0/3]"

    describe "clear-history" $
        it "empties the target pane's scrollback" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            p <- stubPane 0
            atomically $ modifyTVar' win.panes
                (Map.insert p.id p { emulator = emu })
            _ <- Emu.feed emu (mconcat (replicate 20 "line\r\n"))
            hsize <- Emu.scrollbackLength emu
            hsize `shouldSatisfy` (> 0)
            [] <- cmdClearHistory st Nothing []
            Emu.scrollbackLength emu `shouldReturn` 0

    -- Any client at or above the floor is welcomed and told the server's
    -- version (after Welcome, which old clients validate strictly).
    describe "handshake" $
        it "welcomes a future-version client and reports its own version" $ do
            (st, _) <- seedSession "/"
            (server, client) <- socketPair AF_UNIX Stream defaultProtocol
            let h = Hello
                    { protoVersion = protocolVersion + 1
                    , term = "xterm", env = [], size = Size 24 80
                    , cwd = "/", intent = ControlIntent
                    , autostarted = False }
            withAsync (welcome dispatch st server h) $ \_ -> do
                Just (Known (Welcome _)) <- recvMessage client
                Just (Known (ServerVersion v)) <- recvMessage client
                v `shouldBe` protocolVersion
                sendMessage client Detach

    -- restart-client tells the attached client to re-exec in place; issued
    -- from a bare control connection there is nothing rendering, so it is a
    -- no-op. The role decides which.
    describe "restart-client" $ do
        it "is a restart for an attached client, a no-op for a control one" $ do
            restartClientAction (Just Attached) `shouldBe` RestartAttached
            restartClientAction (Just Control) `shouldBe` NoAttachedClient
            restartClientAction Nothing `shouldBe` NoAttachedClient

        it "sends the issuer its own session to rejoin (81)" $ do
            (st, _) <- seedSession "/"
            (client, peer) <- wiredClient st Attached
            [] <- cmdRestartClient st (Just client) []
            recvMessage peer `shouldReturn` Just (Known (RestartClientTo "work"))

        it "sends nothing when a control connection issues it" $ do
            (st, _) <- seedSession "/"
            (client, peer) <- wiredClient st Control
            [] <- cmdRestartClient st (Just client) []
            -- Nothing is queued, so a bounded read finds no frame. The GC
            -- must not close the server-side socket mid-read (its finalizer
            -- would turn "no message" into EOF): force a collection to make
            -- that hazard deterministic, and hold the socket open past the
            -- read with touchSocket.
            performGC
            got <- timeout 100_000
                (recvMessage peer :: IO (Maybe (Inbound ServerToClient)))
            touchSocket client.sock
            got `shouldBe` Nothing

    -- `restart` (bug 4f) composes restart-server with a client restart: the
    -- reload's farewell tells attached clients to re-exec instead of exit.
    describe "restart" $ do
        it "re-execs attached clients and exits everyone else (4f)" $ do
            reloadFarewell ServerAndClients Attached (Just "beta")
                `shouldBe` RestartClientTo "beta"
            reloadFarewell ServerAndClients Attached Nothing
                `shouldBe` RestartClient
            reloadFarewell ServerAndClients Control (Just "beta") `shouldBe` Exited
            reloadFarewell ServerOnly Attached (Just "beta") `shouldBe` Exited
            reloadFarewell ServerOnly Control Nothing `shouldBe` Exited

        it "aborts the client restart when the reload is rejected (4f)" $ do
            (st, _) <- seedSession "/"
            (client, peer) <- wiredClient st Attached
            errs <- cmdRestart st (Just client) ["/no/such/hat"]
            [e | RErr e <- errs] `shouldSatisfy`
                any (T.isInfixOf "no such binary")
            -- Nothing is queued (see the restart-client no-op test for the
            -- GC/touchSocket choreography).
            performGC
            got <- timeout 100_000
                (recvMessage peer :: IO (Maybe (Inbound ServerToClient)))
            touchSocket client.sock
            got `shouldBe` Nothing

    describe "aggressive-resize sizing" $ do
        let big = Size { rows = 50, cols = 200 }
            small = Size { rows = 24, cols = 80 }
            -- an attached client's window area is its viewport minus the status row
            pane s = s { rows = s.rows - 1 }
        it "follows the most-recently-active client under aggressive-resize" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta OptAggressiveResize (OVBool True))
            _ <- addClient st (SessionId 0) big 2      -- bigger client is newer
            _ <- addClient st (SessionId 0) small 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` pane big
        it "carves no status row when status is off" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalSessionOptions
                (singletonDelta OptStatus (OVStatusLines 0))
            _ <- addClient st (SessionId 0) small 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` small

    describe "attach-session -c" $
        -- The feature behind the author's @M-c@ binding: re-anchor where
        -- new windows start. Pure server state — set by the command, read
        -- by new-window — so a state check suffices, no client needed.
        it "re-anchors the session's default working directory" $ do
            (st, sess) <- seedSession "/start"
            _ <- cmdAttachSession st Nothing ["-c", "/re/anchored"]
            readTVarIO sess.startCwd `shouldReturn` "/re/anchored"

    -- attach-session -E: the attach leaves the session environment alone
    -- (no update-environment import); a plain attach imports.
    describe "attach-session -E" $ do
        let seedEnv st sess = do
                atomically $ writeTVar st.options
                    defaultOptions { updateEnvironment = ["MYVAR"] }
                atomically $ writeTVar sess.environ
                    (environSet EnvVisible "MYVAR" "old" emptyEnviron)
        it "skips the update-environment import" $ do
            (st, sess) <- seedSession "/"
            seedEnv st sess
            (client, _peer) <- wiredClientEnv st Attached [("MYVAR", "new")]
            [] <- cmdAttachSession st (Just client) ["-E", "-t", "work"]
            refreshSessionEnv st sess client
            env <- readTVarIO sess.environ
            environFind "MYVAR" env
                `shouldBe` Just (EnvEntry (Just "old") EnvVisible)
        it "a plain attach still imports" $ do
            (st, sess) <- seedSession "/"
            seedEnv st sess
            (client, _peer) <- wiredClientEnv st Attached [("MYVAR", "new")]
            [] <- cmdAttachSession st (Just client) ["-t", "work"]
            refreshSessionEnv st sess client
            env <- readTVarIO sess.environ
            environFind "MYVAR" env
                `shouldBe` Just (EnvEntry (Just "new") EnvVisible)

    describe "new-session" $
        it "creates a named session with its first window; a duplicate name is refused" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-newsess.sock" Nothing
            [] <- cmdNewSession st Nothing
                ["-d", "-s", "alpha", "-n", "init", "sleep 30"]
            [sess] <- Map.elems <$> readTVarIO st.sessions
            [win] <- Map.elems <$> readTVarIO sess.windows
            [pane] <- Map.elems <$> readTVarIO win.panes
            let reap = do
                    _ <- cmdKillSession st Nothing ["-t", "alpha"]
                    () <$ timeout 2_000_000 (Pty.waitExit pane.pty)
            flip finally reap $ do
                readTVarIO sess.name `shouldReturn` "alpha"
                readTVarIO win.name `shouldReturn` "init"
                cmdNewSession st Nothing ["-d", "-s", "alpha"]
                    `shouldReturn` [RErr "duplicate session: alpha"]

    describe "rename-session" $
        it "renames the -t session and refuses a duplicate name" $ do
            (st, sess) <- seedSession "/"   -- "work"
            -- "s" is the newest session, so it would be the default target:
            -- "work" being renamed instead pins that -t chooses.
            other <- addSession st 1
            cmdRenameSession st Nothing ["-t", "work", "s"]
                `shouldReturn` [RErr "duplicate session: s"]
            [] <- cmdRenameSession st Nothing ["-t", "work", "renamed"]
            readTVarIO sess.name `shouldReturn` "renamed"
            readTVarIO other.name `shouldReturn` "s"

    describe "rename-window" $
        it "pins the given name and turns automatic-rename off" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            p <- stubPane 0
            atomically $ modifyTVar' win.panes (Map.insert p.id p)
            [] <- cmdRenameWindow st Nothing ["pinned"]
            readTVarIO win.name `shouldReturn` "pinned"
            readTVarIO win.autoRename `shouldReturn` False

    describe "snapshot commands without persistence" $
        it "list-snapshots and restore-snapshot fail loudly, never silently (bb)" $ do
            (st, _) <- seedSession "/"
            cmdListSnapshots st Nothing []
                `shouldReturn` [RErr "list-snapshots: persistence is disabled"]
            cmdRestoreSnapshot st Nothing ["1"]
                `shouldReturn` [RErr "restore-snapshot: persistence is disabled"]

    describe "list-clients" $
        it "lists attached clients only" $ do
            (st, _) <- seedSession "/"
            _ <- addClient st (SessionId 0) (Size 24 80) 2  -- attached
            _ <- wiredClient st Control
            cmdListClients st Nothing ["-F", "x"]
                `shouldReturn` [ROutput "x"]

    -- Attaching from a pane of a *different* server must be allowed (the
    -- upstream environ-update test drives one hat from inside another);
    -- only nesting a client inside its own server is refused.
    describe "attach nesting guard" $ do
        it "refuses only the enclosing server's own socket" $ do
            nestsOwnServer "/tmp/a.sock,123,0" "/tmp/a.sock" `shouldBe` True
            nestsOwnServer "/tmp/b.sock,123,0" "/tmp/a.sock" `shouldBe` False
        it "an empty TMUX value never refuses" $
            nestsOwnServer "" "/tmp/a.sock" `shouldBe` False

    describe "scoped option resolution" $ do
        it "a session overlay shadows the global chain for that session only" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-scopespec.sock" Nothing
            atomically $ writeTVar st.globalSessionOptions
                (singletonDelta OptPrefix (OVText "C-b"))
            sessA <- addSession st 0
            sessB <- addSession st 1
            atomically $ writeTVar sessA.options
                (singletonDelta OptPrefix (OVText "C-a"))
            a <- atomically (resolveForSession st sessA)
            b <- atomically (resolveForSession st sessB)
            a.prefix `shouldBe` "C-a"   -- session A's own set
            b.prefix `shouldBe` "C-b"   -- B falls through to the global

    describe "window_active_clients" $ do
        it "counts a client viewing a window linked into another session" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-eyes.sock" Nothing
            sessA <- addSession st 0
            sessB <- addSession st 1
            shared <- addWindow sessA 1
            _ <- addWindow sessA 2
            atomically $ do
                modifyTVar' sessB.windows (Map.insert 1 shared)
                writeTVar sessA.currentIx 2   -- A looks at its own window
                writeTVar sessB.currentIx 1   -- B looks at the shared one
                modifyTVar' st.options $ \o ->
                    o { windowStatusFormat = "#I#{?window_active_clients,<eyes>,}" }
            _ <- addClient st sessB.id (Size { rows = 24, cols = 80 }) 1
            row <- statusCells st sessA 80
            T.pack (concatMap Cell.cluster (V.toList row))
                `shouldSatisfy` T.isInfixOf "1<eyes>"

    describe "move-window -r" $
        it "renumbers the -t session alone, current and last-window following" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-renumber.sock" Nothing
            sessA <- addSession st 0
            sessB <- addSession st 1
            mapM_ (addWindow sessA) [0, 5]
            mapM_ (addWindow sessB) [0, 3, 7]
            atomically $ do
                writeTVar sessB.name "other"
                writeTVar sessB.currentIx 3
                writeTVar sessB.windowHist [7, 0]
            [] <- cmdMoveWindow st Nothing ["-r", "-t", "other"]
            wsB <- readTVarIO sessB.windows
            map (fmap (.id)) (Map.toList wsB) `shouldBe`
                [(0, WindowId 0), (1, WindowId 3), (2, WindowId 7)]
            readTVarIO sessB.currentIx `shouldReturn` 1
            readTVarIO sessB.windowHist `shouldReturn` [2, 0]
            wsA <- readTVarIO sessA.windows
            Map.keys wsA `shouldBe` [0, 5]

    describe "current-window attention flags" $ do
        let small = Size { rows = 24, cols = 80 }
        -- tmux shows no bell/activity marker on the window you are looking
        -- at. A bell in the current window must leave no marker — the sticky
        -- marker that only cleared on switch-away-and-back was the bug.
        -- "Looking at it" means an attached client whose outer terminal is
        -- focused; see the outer-focus cases below.
        it "leaves no bell marker on the current window with a focused client" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            _  <- addWindow sess 1
            c  <- addClient st sess.id small 1
            atomically $ writeTVar c.outerFocused True
            atomically $ writeTVar sess.currentIx 0
            atomically $ markBell st sess.id w0
            readTVarIO w0.bellFlag `shouldReturn` False

        it "raises the bell marker on a background window" $ do
            (st, sess) <- seedSession "/"
            _  <- addWindow sess 0
            w1 <- addWindow sess 1
            c  <- addClient st sess.id small 1
            atomically $ writeTVar c.outerFocused True
            atomically $ writeTVar sess.currentIx 0
            atomically $ markBell st sess.id w1
            readTVarIO w1.bellFlag `shouldReturn` True

        -- The bug: a client is attached but its OUTER terminal lost focus
        -- (the user switched to another OS window). The hat-current window is
        -- no longer being watched, so activity/bell there must still flag.
        it "raises the bell marker on the current window when no viewer is focused" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            c  <- addClient st sess.id small 1
            atomically $ writeTVar c.outerFocused False
            atomically $ writeTVar sess.currentIx 0
            atomically $ markBell st sess.id w0
            readTVarIO w0.bellFlag `shouldReturn` True

        it "raises the activity marker on the current window when no viewer is focused" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta OptMonitorActivity (OVBool True))
            w0 <- addWindow sess 0
            c  <- addClient st sess.id small 1
            atomically $ writeTVar c.outerFocused False
            atomically $ writeTVar sess.currentIx 0
            atomically $ markActivity st sess.id w0
            readTVarIO w0.activity `shouldReturn` True

        -- One outer-focused viewer is enough to count the window as seen,
        -- even when another attached client is unfocused.
        it "counts the current window as seen if any viewer is focused" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            focused   <- addClient st sess.id small 1
            unfocused <- addClient st sess.id small 2
            atomically $ writeTVar focused.outerFocused True
            atomically $ writeTVar unfocused.outerFocused False
            atomically $ writeTVar sess.currentIx 0
            atomically $ markBell st sess.id w0
            readTVarIO w0.bellFlag `shouldReturn` False

        -- The full cycle: FocusOut lets the current window flag, FocusIn (the
        -- user looking again) clears the marker on the window it now views.
        it "FocusIn clears the current window's markers, FocusOut lets them flag" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            c  <- addClient st sess.id small 1
            atomically $ writeTVar sess.currentIx 0
            noteOuterFocus st c (Key { name = "FocusOut", raw = "\ESC[O" })
            readTVarIO c.outerFocused `shouldReturn` False
            atomically $ markBell st sess.id w0
            readTVarIO w0.bellFlag `shouldReturn` True
            noteOuterFocus st c (Key { name = "FocusIn", raw = "\ESC[I" })
            readTVarIO c.outerFocused `shouldReturn` True
            readTVarIO w0.bellFlag `shouldReturn` False

    describe "attentionSeen" $ do
        -- The pure rule behind the outer-focus gating: a session-current
        -- window is "seen" (suppress its marker) only when some attached
        -- viewer's outer terminal is focused.
        it "is seen when current and a viewer is focused" $
            attentionSeen True True `shouldBe` True
        it "is not seen when current but no viewer is focused" $
            attentionSeen True False `shouldBe` False
        it "is never seen when not current" $ do
            attentionSeen False True `shouldBe` False
            attentionSeen False False `shouldBe` False

    describe "pickActivityTarget" $ do
        -- The @<leader> a@ jump: an activity-marked window wins, chosen in
        -- the same cyclic next-window order used by @next-window -a@; with no
        -- activity it degrades to @last-window@.
        let order = [0, 1, 2, 3]

        it "jumps to the next activity window after the current, cyclically" $
            -- current is 2; activity on 0 and 1; the cyclic scan wraps past
            -- 3 and lands on 0 (the first flagged one after the current).
            pickActivityTarget order 2 (Set.fromList [0, 1]) (Just 3)
                `shouldBe` Just 0

        it "prefers the nearest activity window ahead of the current" $
            -- current is 0; activity on 2 and 3; 2 comes first in the scan.
            pickActivityTarget order 0 (Set.fromList [2, 3]) (Just 1)
                `shouldBe` Just 2

        it "prioritizes activity over the last-active fallback" $
            -- last-active is 1, but window 3 carries activity, so it wins.
            pickActivityTarget order 0 (Set.fromList [3]) (Just 1)
                `shouldBe` Just 3

        it "falls back to last-window when no window has activity" $
            pickActivityTarget order 2 Set.empty (Just 1) `shouldBe` Just 1

        it "has nothing to do with neither activity nor a last window" $
            pickActivityTarget order 2 Set.empty Nothing `shouldBe` Nothing

    describe "pickAttachSession" $ do
        -- A fresh client attaches to the last-active session (set from the
        -- snapshot on restore), not just the lowest-id one, so a reboot
        -- returns to the session that was focused.
        let sessions = Map.fromList [(1, "a"), (2, "b"), (3, "c")]
                :: Map.Map Int String

        it "attaches to the last-active session when it still exists" $
            pickAttachSession (Just 2) sessions `shouldBe` Just (2, "b")

        it "falls back to the lowest-id session when none is marked" $
            pickAttachSession Nothing sessions `shouldBe` Just (1, "a")

        it "falls back to the lowest-id session when the marked one is gone" $
            pickAttachSession (Just 9) sessions `shouldBe` Just (1, "a")

        it "has nothing to attach to when there are no sessions" $
            pickAttachSession (Just 2) (Map.empty :: Map.Map Int String)
                `shouldBe` Nothing

    describe "nextZoom" $ do
        -- The author's @Z@ binding runs @resize-pane -t ! -Z@ to zoom the
        -- alternate pane. Toggle must key off the *target*, not on whether
        -- any zoom exists: zooming the alternate while a different pane is
        -- already zoomed must land on the alternate, not just unzoom.
        let a = PaneId 1
            b = PaneId 2

        it "zooms the target when nothing is zoomed" $
            nextZoom Nothing b `shouldBe` Just b

        it "unzooms when the target is the zoomed pane" $
            nextZoom (Just b) b `shouldBe` Nothing

        it "zooms the alternate when another pane is already zoomed" $
            -- bug 36: pane a is zoomed; Z targets alternate b. It must zoom
            -- b, not unzoom because *some* pane was zoomed.
            nextZoom (Just a) b `shouldBe` Just b

    describe "zoomTarget" $ do
        let windowWithPanes ns = do
                (st, sess) <- seedSession "/"
                win <- addWindow sess 0
                mapM_ (\n -> do
                    p <- stubPane n
                    atomically $ modifyTVar' win.panes (Map.insert p.id p)) ns
                pure (st, win)

        it "leaves a solo pane unzoomed and the screen untouched (bug 9)" $ do
            (st, win) <- windowWithPanes [0]
            gen <- readTVarIO st.dirty
            zoomTarget st Nothing Nothing
            readTVarIO win.zoomed `shouldReturn` Nothing
            readTVarIO st.dirty `shouldReturn` gen

        it "zooms the active pane when the window has siblings" $ do
            (st, win) <- windowWithPanes [0, 1]
            zoomTarget st Nothing Nothing
            readTVarIO win.zoomed `shouldReturn` Just (PaneId 0)

    describe "detachPane" $ do
        -- A pane's removal from the model is one atomic transaction,
        -- decoupled from the child's OS teardown (see 'reapPane'). Both the
        -- killing command and the reader thread's finally may attempt it;
        -- the dead-guard makes exactly one win, for ANY interleaving.
        let twoPaneWindow = do
                (st, sess) <- seedSession "/"
                win <- addWindow sess 0
                pa <- stubPane 1
                pb <- stubPane 2
                atomically $ do
                    writeTVar win.layout
                        (Split LeftRight 0.5 (Leaf pa.id) (Leaf pb.id))
                    writeTVar win.panes
                        (Map.fromList [(pa.id, pa), (pb.id, pb)])
                    writeTVar win.activeId pa.id
                pure (st, sess, win, pa, pb)

        it "removes the pane and reactivates a survivor in one transaction" $ do
            (st, _, win, pa, pb) <- twoPaneWindow
            r <- atomically (detachPane st (SessionId 0) win pa)
            r `shouldBe` Detached WindowSurvives SessionSurvives
            readTVarIO win.layout `shouldReturn` Leaf pb.id
            (Map.member pa.id <$> readTVarIO win.panes) `shouldReturn` False
            readTVarIO win.activeId `shouldReturn` pb.id

        it "no-ops for the loser of a detach race" $ do
            -- Without the dead-guard a second detach would see the pane's
            -- leaf already gone and wrongly collapse the whole window.
            (st, sess, win, pa, _) <- twoPaneWindow
            _  <- atomically (detachPane st (SessionId 0) win pa)
            r2 <- atomically (detachPane st (SessionId 0) win pa)
            r2 `shouldBe` AlreadyDetached
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` True

        it "collapses the window when its last pane detaches" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            _   <- addWindow sess 1     -- another window survives
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (Leaf pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
            r <- atomically (detachPane st (SessionId 0) win pa)
            r `shouldBe` Detached WindowRemoved SessionSurvives
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` False

        it "reports the session emptied by its last pane" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (Leaf pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
            r <- atomically (detachPane st (SessionId 0) win pa)
            r `shouldBe` Detached WindowRemoved SessionEmptied
            (Map.member (SessionId 0) <$> readTVarIO st.sessions)
                `shouldReturn` False

        it "pops the window history so a new last-window remains after a close" $ do
            (st, sess) <- seedSession "/"
            _  <- addWindow sess 0
            _  <- addWindow sess 1
            w2 <- addWindow sess 2
            pa <- stubPane 2
            atomically $ do
                writeTVar w2.layout (Leaf pa.id)
                writeTVar w2.panes (Map.singleton pa.id pa)
                writeTVar sess.currentIx 2
                writeTVar sess.windowHist [1, 0]  -- visited 0, then 1, then 2
            _ <- atomically (detachPane st (SessionId 0) w2 pa)
            readTVarIO sess.currentIx `shouldReturn` 1
            readTVarIO sess.windowHist `shouldReturn` [0]

        it "pops the pane history so a new last-pane remains after a close" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            [pa, pb, pc] <- mapM stubPane [1, 2, 3]
            atomically $ do
                writeTVar win.layout
                    (Split LeftRight 0.5 (Leaf pa.id)
                        (Split LeftRight 0.5 (Leaf pb.id) (Leaf pc.id)))
                writeTVar win.panes
                    (Map.fromList [(p.id, p) | p <- [pa, pb, pc]])
                writeTVar win.activeId pc.id
                writeTVar win.paneHist [pb.id, pa.id]
            _ <- atomically (detachPane st (SessionId 0) win pc)
            readTVarIO win.activeId `shouldReturn` pb.id
            readTVarIO win.paneHist `shouldReturn` [pa.id]

    describe "detachPanes" $ do
        -- kill-window/-session detach a whole set of panes in one
        -- transaction, so the window (and an emptied session) collapses
        -- synchronously with the command — never waiting on the children.
        let paneIn win n = do
                p <- stubPane n
                atomically $ modifyTVar' win.panes (Map.insert p.id p)
                pure p

        it "collapses a window when all its panes detach at once, session surviving" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            _  <- addWindow sess 1
            pa <- paneIn w0 1
            pb <- paneIn w0 2
            atomically $ writeTVar w0.layout
                (Split LeftRight 0.5 (Leaf pa.id) (Leaf pb.id))
            results <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w0, pb)]
            [ sid | ((sid, _), Detached _ SessionEmptied) <- results ]
                `shouldBe` []
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` False
            (Map.member 1 <$> readTVarIO sess.windows) `shouldReturn` True

        it "empties the session when every pane in it detaches at once" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            w1 <- addWindow sess 1
            pa <- paneIn w0 1
            pb <- paneIn w1 2
            atomically $ do
                writeTVar w0.layout (Leaf pa.id)
                writeTVar w1.layout (Leaf pb.id)
            results <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w1, pb)]
            [ sid | ((sid, _), Detached _ SessionEmptied) <- results ]
                `shouldBe` [SessionId 0]
            (Map.member (SessionId 0) <$> readTVarIO st.sessions)
                `shouldReturn` False

    describe "detachPaneCurrent" $
        -- A pane's reader captures nothing about where it lives: break-pane
        -- re-parents a live pane into a new window, so teardown must collapse
        -- the pane's CURRENT window, not the split it was spawned in. Detaching
        -- from the spawn window would strand the broken-out pane forever.
        it "detaches a re-parented pane from its current window" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            pa <- stubPane 1
            pb <- stubPane 2
            atomically $ do
                writeTVar w0.layout
                    (Split LeftRight 0.5 (Leaf pa.id) (Leaf pb.id))
                writeTVar w0.panes (Map.fromList [(pa.id, pa), (pb.id, pb)])
                writeTVar w0.activeId pa.id
            -- Break pa out into its own window, as cmdBreakPane does: drop it
            -- from w0 first (the new window is not in the tree yet), then place
            -- it in w1. pa now lives only in w1.
            atomically (removePaneFromTree st pa.id)
            w1 <- addWindow sess 1
            atomically $ do
                writeTVar w1.layout (Leaf pa.id)
                writeTVar w1.panes (Map.singleton pa.id pa)
                writeTVar w1.activeId pa.id
            (msid, r) <- atomically (detachPaneCurrent st pa)
            msid `shouldBe` Just (SessionId 0)
            r `shouldBe` Detached WindowRemoved SessionSurvives
            -- w1 (pa's current window) collapsed; w0 survives with pb.
            (Map.member 1 <$> readTVarIO sess.windows) `shouldReturn` False
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` True
            (Map.member pb.id <$> readTVarIO w0.panes) `shouldReturn` True

    describe "break-pane" $
        -- The move half; the teardown half is 'detachPaneCurrent' above.
        it "moves the active pane into a fresh window of its own" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            (pa, release) <- adoptedPane 1
            pb <- stubPane 2
            atomically $ do
                writeTVar w0.layout
                    (Split LeftRight 0.5 (Leaf pa.id) (Leaf pb.id))
                writeTVar w0.panes (Map.fromList [(pa.id, pa), (pb.id, pb)])
                writeTVar w0.activeId pa.id
            flip finally release $ do
                [] <- cmdBreakPane st Nothing []
                ws <- readTVarIO sess.windows
                Map.keys ws `shouldBe` [0, 1]
                Just w1 <- pure (Map.lookup 1 ws)
                readTVarIO w1.layout `shouldReturn` Leaf pa.id
                (Map.keys <$> readTVarIO w1.panes) `shouldReturn` [pa.id]
                readTVarIO w0.layout `shouldReturn` Leaf pb.id
                readTVarIO w0.activeId `shouldReturn` pb.id
                readTVarIO sess.currentIx `shouldReturn` 1

    describe "join-pane" $
        it "splits the -s pane into the current window, closing its emptied window" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0      -- current; holds pane 0
            w1 <- addWindow sess 1      -- source; holds pane 1
            pa <- stubPane 0
            pc <- stubPane 1
            atomically $ do
                modifyTVar' w0.panes (Map.insert pa.id pa)
                modifyTVar' w1.panes (Map.insert pc.id pc)
            [] <- cmdJoinPane st Nothing ["-h", "-s", "%1"]
            (Map.member 1 <$> readTVarIO sess.windows) `shouldReturn` False
            (Map.keys <$> readTVarIO w0.panes) `shouldReturn` [pa.id, pc.id]
            readTVarIO w0.layout `shouldReturn`
                Split LeftRight 0.5 (Leaf pa.id) (Leaf pc.id)
            readTVarIO w0.activeId `shouldReturn` pc.id

    describe "serverIdle" $ do
        let idle = IdleInputs
                { idleAttached = True, idleServed = True, idlePhase = Ready
                , idleSessions = 0, idleClients = 0, idlePanes = 0 }
        it "is idle once served, drained, and every pane reaped" $
            serverIdle idle `shouldBe` True
        it "stays busy while a pane is still being reaped" $
            -- bug 98: a drained server must outlive its children's reap, so a
            -- SIGHUP-ignoring child is SIGKILLed rather than orphaned on exit.
            serverIdle (idle { idlePanes = 1 }) `shouldBe` False
        it "stays busy until it has served a client" $ do
            serverIdle (idle { idleServed = False }) `shouldBe` False
            serverIdle (idle { idleAttached = False }) `shouldBe` False
        it "stays busy until startup lands at Ready" $ do
            serverIdle (idle { idlePhase = LoadingConfig }) `shouldBe` False
            serverIdle (idle { idlePhase = Restoring }) `shouldBe` False
        it "stays busy while any session or client remains" $ do
            serverIdle (idle { idleSessions = 1 }) `shouldBe` False
            serverIdle (idle { idleClients = 1 }) `shouldBe` False

    describe "awaitReconciled" $ do
        let freshState = do
                lg <- newLogger "/dev/null"
                newServerState Map.empty lg "/tmp/hat-reconcilespec.sock" Nothing

        it "returns at once when reconcile has caught up to the dirty tick" $ do
            st <- freshState
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 5
            settled <- timeout 1_000_000 (awaitReconciled st)
            settled `shouldBe` Just ()

        it "blocks while reconcile lags the dirty tick, then returns once it catches up" $ do
            st <- freshState
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 2
            -- The barrier must not report a stale generation settled: a
            -- control command that returned here would race an unresized child.
            early <- timeout 100_000 (awaitReconciled st)
            early `shouldBe` Nothing
            -- Once reconcileLoop publishes the generation, the barrier lifts.
            atomically $ writeTVar st.reconciled 5
            caught <- timeout 1_000_000 (awaitReconciled st)
            caught `shouldBe` Just ()

    describe "awaitReconcileTick (command-batch gate)" $ do
        let freshState = do
                lg <- newLogger "/dev/null"
                newServerState Map.empty lg "/tmp/hat-batchgate.sock" Nothing
            -- The generation reconcileLoop would size panes for now, or
            -- 'Nothing' if it would block. 'orElse' reads the STM gate without
            -- relying on timing.
            tick st lastGen =
                atomically
                    ((Just <$> awaitReconcileTick st lastGen) `orElse` pure Nothing)

        it "sizes a fresh generation when no command batch is open" $ do
            st <- freshState
            atomically (writeTVar st.dirty 5)
            tick st (-1) `shouldReturn` Just 5

        -- Bug 66: reconcile must not size a pane to a command batch's
        -- intermediate layout, only to the layout it settles on.
        it "defers sizing while a command batch holds the layout mid-change" $ do
            st <- freshState
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.commandDepth 1
            tick st (-1) `shouldReturn` Nothing
            atomically (writeTVar st.commandDepth 0)
            tick st (-1) `shouldReturn` Just 5

    describe "awaitRenderable (render gate)" $ do
        let freshState = do
                lg <- newLogger "/dev/null"
                newServerState Map.empty lg "/tmp/hat-rendergate.sock" Nothing
            sz = Size { rows = 24, cols = 80 }
            -- Whether the render loop would paint now, or block for a fresh,
            -- reconciled generation. 'orElse' turns a blocking retry into
            -- 'Nothing' with no reliance on timing.
            renderable st client lastGen =
                atomically
                    ((Just <$> awaitRenderable st client lastGen)
                        `orElse` pure Nothing)

        it "paints a fresh generation once reconcile has sized it" $ do
            st <- freshState
            client <- addClient st (SessionId 0) sz 1
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 5
                writeTVar client.needsFull False
            renderable st client (-1) `shouldReturn` Just 5

        it "does not paint while reconcile lags the dirty tick" $ do
            st <- freshState
            client <- addClient st (SessionId 0) sz 1
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 2
                writeTVar client.needsFull False
            -- Painting here would flash the old grid into the new geometry:
            -- the layout (zoom/unzoom/split) has changed but the emulators
            -- behind it are not yet resized.
            renderable st client (-1) `shouldReturn` Nothing

        it "holds a forced full redraw until its generation is reconciled" $ do
            st <- freshState
            client <- addClient st (SessionId 0) sz 1
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 2
                writeTVar client.needsFull True
            renderable st client (-1) `shouldReturn` Nothing

        it "repaints on needsFull once the generation is reconciled" $ do
            st <- freshState
            client <- addClient st (SessionId 0) sz 1
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 5
                writeTVar client.needsFull True
            renderable st client 5 `shouldReturn` Just 5

        it "idles when nothing is new and no full redraw is pending" $ do
            st <- freshState
            client <- addClient st (SessionId 0) sz 1
            atomically $ do
                writeTVar st.dirty 5
                writeTVar st.reconciled 5
                writeTVar client.needsFull False
            renderable st client 5 `shouldReturn` Nothing

    describe "windowActivity (output repaint gating)" $ do
        let sz = Size { rows = 24, cols = 80 }
            -- A session with two windows and a client watching window 0.
            watchedPair = do
                (st, sess) <- seedSession "/"
                front <- addWindow sess 0
                back <- addWindow sess 1
                client <- addClient st (SessionId 0) sz 1
                pure (st, front, back, client)

        it "repaints when the window is on an attached client's screen" $ do
            (st, front, _, _) <- watchedPair
            gen <- readTVarIO st.dirty
            windowActivity st (SessionId 0) front
            readTVarIO st.dirty `shouldNotReturn` gen

        it "leaves the screen untouched for a background window" $ do
            (st, _, back, _) <- watchedPair
            gen <- readTVarIO st.dirty
            windowActivity st (SessionId 0) back
            readTVarIO st.dirty `shouldReturn` gen

        it "repaints a monitored background window only when its flag rises" $ do
            (st, _, back, _) <- watchedPair
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta OptMonitorActivity (OVBool True))
            gen <- readTVarIO st.dirty
            windowActivity st (SessionId 0) back
            gen' <- readTVarIO st.dirty
            gen' `shouldNotBe` gen
            windowActivity st (SessionId 0) back
            readTVarIO st.dirty `shouldReturn` gen'

        it "keeps a chooser's live preview fresh" $ do
            (st, _, back, client) <- watchedPair
            atomically $ writeTVar client.picker $ Just PickerState
                { title = "choose", roots = [], cursor = 0
                , query = "", search = "", mode = Browsing, fill = PaneRegion }
            gen <- readTVarIO st.dirty
            windowActivity st (SessionId 0) back
            readTVarIO st.dirty `shouldNotReturn` gen

    describe "deliversKey (focus-event gating)" $ do
        let focusIn = Key { name = "FocusIn", raw = "\ESC[I" }
            typed   = Key { name = "a", raw = "a" }
            withFocus on = defaultOptions { focusEvents = on }

        it "always delivers non-focus keys, whatever the gating" $ do
            deliversKey (withFocus False) False typed `shouldBe` True
            deliversKey (withFocus True)  True  typed `shouldBe` True

        it "drops a focus report when focus-events is off" $
            deliversKey (withFocus False) True focusIn `shouldBe` False

        it "drops a focus report when the pane never enabled ?1004" $
            deliversKey (withFocus True) False focusIn `shouldBe` False

        it "delivers a focus report only when focus-events is on and ?1004 set" $
            deliversKey (withFocus True) True focusIn `shouldBe` True

        -- bug 64: a sequence hat cannot name is dropped, never forwarded --
        -- the poison-stopper for a pane that enabled no key protocol.
        it "drops a sequence it could not name" $
            deliversKey (withFocus True) True
                Key { name = "Unknown", raw = "\ESC[97~" } `shouldBe` False

    describe "reencodeKey (what the pane receives)" $ do
        -- hat advertises TERM=tmux-256color (khome=\E[1~, kend=\E[4~), so Home
        -- and End must forward those bytes whatever xterm-ish form the outer
        -- terminal sent -- else a pager reads a bare H/F and less opens help.
        let reraw form = (.raw) <$> reencodeKey Nothing (Key "Home" form)
        it "normalizes Home to khome (\\E[1~) from any incoming form" $ do
            reraw "\ESC[H"  `shouldReturn` "\ESC[1~"
            reraw "\ESCOH"  `shouldReturn` "\ESC[1~"
            reraw "\ESC[1~" `shouldReturn` "\ESC[1~"
        it "normalizes End to kend (\\E[4~)" $
            ((.raw) <$> reencodeKey Nothing (Key "End" "\ESCOF"))
                `shouldReturn` "\ESC[4~"
        it "normalizes F1-F4 to the SS3 kf1..kf4 forms from the legacy CSI" $ do
            -- bug fad
            ((.raw) <$> reencodeKey Nothing (Key "F1" "\ESC[11~"))
                `shouldReturn` "\ESCOP"
            ((.raw) <$> reencodeKey Nothing (Key "F4" "\ESC[14~"))
                `shouldReturn` "\ESCOS"
        it "leaves an unrecognized sequence's raw bytes alone" $
            -- bug fad
            ((.raw) <$> reencodeKey Nothing (Key "Unknown" "\ESC[97~"))
                `shouldReturn` "\ESC[97~"

        -- bug 64: a modified character key is spelled by the pane's own key
        -- protocol state, so what the app asked for is what it receives.
        it "encodes a modified key through the pane's key protocol" $ do
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            let cEnter = Key "C-Enter" "\r"
            ((.raw) <$> reencodeKey (Just emu) cEnter) `shouldReturn` "\r"
            _ <- Emu.feed emu "\ESC[>4;2m"
            ((.raw) <$> reencodeKey (Just emu) cEnter)
                `shouldReturn` "\ESC[27;5;13~"
