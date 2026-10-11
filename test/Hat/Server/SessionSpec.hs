module Hat.Server.SessionSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
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
    (ArrayKey (..), Options (..), OptionValue (..)
    , defaultOptions, emptyDelta, insertDelta, lookupDelta, singletonDelta)
import Hat.Server
    ( DetachResult (..), WindowFate (..), SessionFate (..), IdleInputs (..)
    , IdleVerdict (..), serverIdle
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
import Hat.Server.ClientSession
    ( DetachOnDestroy (..), SessionChoice (..), destroyDestination )
import Hat.Server.Conn (handleInput)
import Hat.Server.Dispatch (runArgv)
import Hat.Server.Command.Pane (cmdClearHistory)
import Hat.Server.Pane (deadNoticeBytes, syncTargets)
import Hat.Server.Command.Session
    ( DestroyUnattached (..), checkUnattached, cmdKillSession, cmdNewSession
    , cmdRenameSession, destroysUnattached )
import Hat.Server.Command.Window (cmdRenameWindow, cmdResizeWindow, cmdUnlinkWindow)
import Hat.Server.Environ
    ( EnvEntry (..), EnvVisibility (..), emptyEnviron, environFind
    , environSet )
import Hat.Server.Snapshot (cmdListSnapshots, cmdRestoreSnapshot)
import Hat.Term.Pty qualified as Pty
import Hat.Server.Keys
    ( EscPending (NoEscPending), ExtendedFormat (..), ExtendedKeys (..), Key (..)
    , KeyEncoding (..), initialKeyState )
import Hat.Transport.Wire
    ( Autostart (..), ClientToServer (..), Hello (..), Inbound (..), Intent (..)
    , ServerToClient (..), newReadEnd, protocolVersion, recvMessage
    , sendMessage )
import Hat.Server.Floating (noFloats)
import Hat.Server.Layout (Orientation (LeftRight), layoutPanes, singlePane)
import Hat.TestSupport (nodeOf)
import Hat.Server.Render (blankFrame)
import Hat.Server.FormatEnv (paneModeEnv, sessionFormatEnv)
import Hat.Server.ControlTypes (ControlState (..), newControlState)
import Hat.Server.Resize
    (AreaClaim (..), paneSizeTargets, pickArea, windowArea)
import Hat.Server.View (awaitRenderable, renderOnce, statusCells)

-- A bare session with the given id inserted into an existing server.
addSession :: ServerState -> Int -> IO Session
addSession st n = do
    sess <- Session (SessionId n) 0 0
        <$> newTVarIO "s"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO emptyEnviron
        <*> newTVarIO "/"
        <*> newTVarIO emptyDelta
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO 0
        <*> newTVarIO NotHeld
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId n) sess)
    pure sess

-- A bare session with a known default working directory, inserted into a
-- fresh server so 'cmdAttachSession' can target it.
seedSession :: FilePath -> IO (ServerState, Session)
seedSession start = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-sessionspec.sock" Nothing
    sess <- Session (SessionId 0) 0 0
        <$> newTVarIO "work"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO emptyEnviron
        <*> newTVarIO start
        <*> newTVarIO emptyDelta
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO 0
        <*> newTVarIO NotHeld
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId 0) sess)
    pure (st, sess)

-- A bare window inserted into a session at the given index, so bell/activity
-- flagging can be exercised against a real session tree.
addWindow :: Session -> Int -> IO Window
addWindow sess n = do
    win <- Window (WindowId n)
        <$> newTVarIO "w"
        <*> newTVarIO (singlePane (PaneId n))
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
        <*> newTVarIO Nothing
        <*> newTVarIO noFloats
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
        <*> newIORef initialKeyState
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame sz)
        <*> newIORef V.empty
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef (Pos 0 0, True)
        <*> newIORef Nothing
        <*> newIORef 0        -- lastMouseMode
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing  -- displayPanes
        <*> newTVarIO True     -- outerFocused
        <*> newTVarIO ImportEnv
        <*> pure []
        <*> pure ""
        <*> pure NoStdin
        <*> pure Nothing
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
        <*> newIORef initialKeyState
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame (Size 24 80))
        <*> newIORef V.empty
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef (Pos 0 0, True)
        <*> newIORef Nothing
        <*> newIORef 0        -- lastMouseMode
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing  -- displayPanes
        <*> newTVarIO True
        <*> newTVarIO ImportEnv
        <*> pure clientEnv
        <*> pure ""
        <*> pure NoStdin
        <*> pure Nothing
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
    homeV <- newTVarIO Nothing
    optsV <- newTVarIO emptyDelta
    exitedV <- newTVarIO Nothing
    inputOffV <- newTVarIO False
    alternateV <- newTVarIO False
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
        , home = homeV
        , pendingInput = Nothing
        , start = FreshShell
        , exited = exitedV
        , kind = ProgramPane
        , inputOff = inputOffV
        , onAlternate = alternateV
        }

-- The pane's cached location ('Pane.home'), projected to ids for assertions.
homeOf :: Pane -> IO (Maybe (SessionId, WindowId))
homeOf p = fmap (\(sid, w) -> (sid, w.id)) <$> readTVarIO p.home

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
        , scrollExit = False, hidePosition = False
        }
    }

spec :: Spec
spec = do
    describe "deadNoticeBytes" $
        it "scrolls a line and draws the styled notice, cursor hidden" $ do
            let sz = Size { rows = 3, cols = 8 }
            e <- Emu.newEmulator sz 100
            _ <- Emu.feed e "a\r\nb\r\nc"
            _ <- Emu.feed e (deadNoticeBytes sz "x#[fg=red]dead#[bold]!too long")
            scr <- Emu.snapshot e
            let glyphs row = [ ch | Cell.Cell { content = Cell.Glyph ch _ _ }
                                        <- V.toList row ]
            map glyphs (V.toList scr.cells)
                `shouldBe` ["b       ", "c       ", "xdead!to"]
            let styleAt col = ((scr.cells V.! 2) V.! col).style
            (styleAt 0).fg `shouldBe` Cell.DefaultColor
            (styleAt 1).fg `shouldBe` Cell.Indexed 1
            ((styleAt 5).fg, (styleAt 5).bold) `shouldBe` (Cell.Indexed 1, True)
            snd <$> Emu.cursorState e `shouldReturn` False

    -- User options resolve per scope: a session-local @set @v@ is visible
    -- to formats and shadows the global value (upstream format-strings.sh).
    describe "user options in formats" $
        it "a session-local @option shadows the global for #{@v}" $ do
            (st, sess) <- seedSession "/"
            atomically $ do
                modifyTVar' st.options $ \o ->
                    o { user = Map.singleton "@v" "global" }
                modifyTVar' sess.options
                    (insertDelta "@v" (OVScalar "local"))
            env <- sessionFormatEnv st sess
            Map.lookup "@v" env `shouldBe` Just "local"

    describe "session format counts" $ do
        it "session_created and session_restores expose identity metadata" $ do
            (st, sess) <- seedSession "/"
            env <- sessionFormatEnv st sess
            Map.lookup "session_created" env `shouldBe` Just "0"
            Map.lookup "session_restores" env `shouldBe` Just "0"

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
            emu <- Emu.newEmulator Size { rows = 24, cols = 80 } 100
            p0 <- stubPane 0
            let p = p0 { emulator = emu }
            atomically $ do
                modifyTVar' win.panes (Map.insert p.id p)
                writeTVar p.mode (Just (copyModeAt 3))
                -- status off: statusCells would touch the stub pane
                writeTVar st.globalSessionOptions
                    (singletonDelta "status" (OVScalar "off"))
            (client, _peer) <- wiredClient st Attached
            renderOnce st client
            frame <- readIORef client.lastFrame
            let row0 = T.pack (concatMap Cell.cluster (V.toList (frame V.! 0)))
            row0 `shouldSatisfy` T.isInfixOf "[0/3]"

    describe "pane colours" $
        it "window-style and pane-colours paint what a client sees of a pane" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            p0 <- stubPane 0
            let p = p0 { emulator = emu }
            _ <- Emu.feed emu "a\ESC[31mb"
            atomically $ do
                modifyTVar' win.panes (Map.insert p.id p)
                writeTVar st.globalSessionOptions
                    (singletonDelta "status" (OVScalar "off"))
                writeTVar p.options
                    (insertDelta "pane-colours" (OVArray (Map.singleton (KeyIndex 1) "colour100"))
                        (singletonDelta "window-style" (OVScalar "bg=colour4")))
            (client, _peer) <- wiredClient st Attached
            renderOnce st client
            frame <- readIORef client.lastFrame
            map (.style) (take 2 (V.toList (frame V.! 0))) `shouldBe`
                [ Cell.defaultStyle { Cell.bg = Cell.Indexed 4 }
                , Cell.defaultStyle { Cell.fg = Cell.Indexed 100, Cell.bg = Cell.Indexed 4 } ]

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
                    , autostarted = False, sessionHist = [] }
            serverRe <- newReadEnd server
            clientRe <- newReadEnd client
            withAsync (welcome dispatch st serverRe h) $ \_ -> do
                Just (Known (Welcome _)) <- recvMessage clientRe
                Just (Known (ServerVersion v)) <- recvMessage clientRe
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
            peerRe <- newReadEnd peer
            recvMessage peerRe `shouldReturn`
                Just (Known (RestartClientTo "work" []))

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
            peerRe <- newReadEnd peer
            got <- timeout 100_000
                (recvMessage peerRe :: IO (Maybe (Inbound ServerToClient)))
            touchSocket client.sock
            got `shouldBe` Nothing

    -- `restart` (bug 4f) composes restart-server with a client restart: the
    -- reload's farewell tells attached clients to re-exec instead of exit.
    describe "restart" $ do
        it "re-execs attached clients and exits everyone else (4f)" $ do
            reloadFarewell ServerAndClients Attached (Just "beta") ["alpha"]
                `shouldBe` RestartClientTo "beta" ["alpha"]
            reloadFarewell ServerAndClients Attached Nothing []
                `shouldBe` RestartClient
            reloadFarewell ServerAndClients Control (Just "beta") [] `shouldBe` Exited
            reloadFarewell ServerOnly Attached (Just "beta") [] `shouldBe` Exited
            reloadFarewell ServerOnly Control Nothing [] `shouldBe` Exited

        it "aborts the client restart when the reload is rejected (4f)" $ do
            (st, _) <- seedSession "/"
            (client, peer) <- wiredClient st Attached
            errs <- cmdRestart st (Just client) ["/no/such/hat"]
            [e | RErr e <- errs] `shouldSatisfy`
                any (T.isInfixOf "no such binary")
            -- Nothing is queued (see the restart-client no-op test for the
            -- GC/touchSocket choreography).
            performGC
            peerRe <- newReadEnd peer
            got <- timeout 100_000
                (recvMessage peerRe :: IO (Maybe (Inbound ServerToClient)))
            touchSocket client.sock
            got `shouldBe` Nothing

    describe "window-size sizing" $ do
        let big = Size { rows = 50, cols = 200 }
            small = Size { rows = 24, cols = 80 }
            -- an attached client's window area is its viewport minus the status row
            pane s = s { rows = s.rows - 1 }
        it "follows the most-recently-active client by default (latest)" $ do
            (st, sess) <- seedSession "/"
            _ <- addClient st (SessionId 0) big 2      -- bigger client is newer
            _ <- addClient st (SessionId 0) small 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` pane big
        it "fits the smallest client under window-size smallest" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta "window-size" (OVScalar "smallest"))
            _ <- addClient st (SessionId 0) big 2
            _ <- addClient st (SessionId 0) small 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` pane small
        it "keeps a manual size whatever the clients" $ do
            (st, sess) <- seedSession "/"
            sz0 <- readTVarIO sess.lastSize
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta "window-size" (OVScalar "manual"))
            _ <- addClient st (SessionId 0) big 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` sz0
        it "resize-window sizes the window and pins it to manual" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            p <- stubPane 0
            atomically $ modifyTVar' win.panes (Map.insert p.id p)
            _ <- addClient st (SessionId 0) big 1
            cmdResizeWindow st Nothing ["-t", "work", "-x", "33", "-y", "7"]
                `shouldReturn` []
            readTVarIO sess.lastSize `shouldReturn` Size { rows = 7, cols = 33 }
            -- a later client change no longer moves it
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` Size { rows = 7, cols = 33 }
        it "resize-window rejects an out-of-range width" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            p <- stubPane 0
            atomically $ modifyTVar' win.panes (Map.insert p.id p)
            cmdResizeWindow st Nothing ["-t", "work", "-x", "0"]
                `shouldReturn` [RErr "width too small"]
        it "carves no status row when status is off" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalSessionOptions
                (singletonDelta "status" (OVScalar "off"))
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

    describe "new-session" $ do
        it "sizes a detached session from default-size, overridden by -x/-y" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-newsess-size.sock" Nothing
            atomically $ writeTVar st.globalSessionOptions
                (singletonDelta "default-size" (OVScalar "50x20"))
            cmdNewSession st Nothing ["-d", "-x", "0"]
                `shouldReturn` [RErr "width too small"]
            [] <- cmdNewSession st Nothing ["-d", "-s", "a", "-y", "7", "sleep 30"]
            [sess] <- Map.elems <$> readTVarIO st.sessions
            [win] <- Map.elems <$> readTVarIO sess.windows
            [pane] <- Map.elems <$> readTVarIO win.panes
            let reap = do
                    _ <- cmdKillSession st Nothing ["-t", "a"]
                    () <$ timeout 2_000_000 (Pty.waitExit pane.pty)
            flip finally reap $ do
                readTVarIO sess.lastSize `shouldReturn` Size { rows = 7, cols = 50 }
                lookupDelta "default-size" <$> readTVarIO sess.options
                    `shouldReturn` Just (OVScalar "50x7")
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
            ws <- sequence [addWindow sess 0, addWindow other 1]
            ps <- mapM stubPane [0, 1]
            atomically $ do
                V.forM_ (V.fromList (zip ws ps)) $ \(w, p) ->
                    modifyTVar' w.panes (Map.insert p.id p)
                writeTVar other.currentIx 1
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
                (singletonDelta "prefix" (OVScalar "C-b"))
            sessA <- addSession st 0
            sessB <- addSession st 1
            atomically $ writeTVar sessA.options
                (singletonDelta "prefix" (OVScalar "C-a"))
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
                writeTVar st.globalWindowOptions $ singletonDelta
                    "window-status-format"
                    (OVScalar "#I#{?window_active_clients,<eyes>,}")
            _ <- addClient st sessB.id (Size { rows = 24, cols = 80 }) 1
            rows <- statusCells st sessA 80
            T.pack (concatMap (concatMap Cell.cluster . V.toList) rows)
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

    describe "renumber-windows" $ do
        -- Windows 0 1 2 9, each with one stub pane; window 1's pane closes.
        let closeWindowOne setting = do
                (st, sess) <- seedSession "/"
                ws <- mapM (addWindow sess) [0, 1, 2, 9]
                ps <- mapM stubPane [0, 1, 2, 9]
                atomically $ do
                    mapM_ (\(w, p) -> modifyTVar' w.panes (Map.insert p.id p))
                        (zip ws ps)
                    writeTVar sess.currentIx 9
                    writeTVar sess.options
                        (singletonDelta "renumber-windows" (OVScalar setting))
                _ <- atomically (detachPane st (ws !! 1) (ps !! 1))
                (,) <$> (map (fmap (.id)) . Map.toList <$> readTVarIO sess.windows)
                    <*> readTVarIO sess.currentIx
        it "closes the gap a destroyed window leaves when on" $
            closeWindowOne "on" `shouldReturn`
                ([(0, WindowId 0), (1, WindowId 2), (2, WindowId 9)], 2)
        it "leaves the gap when off" $
            closeWindowOne "off" `shouldReturn`
                ([(0, WindowId 0), (2, WindowId 2), (9, WindowId 9)], 9)

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
                (singletonDelta "monitor-activity" (OVScalar "on"))
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
            Right () <- zoomTarget st Nothing Nothing
            readTVarIO win.zoomed `shouldReturn` Nothing
            readTVarIO st.dirty `shouldReturn` gen

        it "zooms the active pane when the window has siblings" $ do
            (st, win) <- windowWithPanes [0, 1]
            Right () <- zoomTarget st Nothing Nothing
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
                        (nodeOf LeftRight [singlePane pa.id, singlePane pb.id])
                    writeTVar win.panes
                        (Map.fromList [(pa.id, pa), (pb.id, pb)])
                    writeTVar win.activeId pa.id
                pure (st, sess, win, pa, pb)

        it "removes the pane and reactivates a survivor in one transaction" $ do
            (st, _, win, pa, pb) <- twoPaneWindow
            r <- atomically (detachPane st win pa)
            r `shouldBe` Detached WindowSurvives SessionSurvives
            layoutPanes <$> readTVarIO win.layout `shouldReturn` [pb.id]
            (Map.member pa.id <$> readTVarIO win.panes) `shouldReturn` False
            readTVarIO win.activeId `shouldReturn` pb.id

        it "no-ops for the loser of a detach race" $ do
            -- Without the dead-guard a second detach would see the pane's
            -- leaf already gone and wrongly collapse the whole window.
            (st, sess, win, pa, _) <- twoPaneWindow
            _  <- atomically (detachPane st win pa)
            r2 <- atomically (detachPane st win pa)
            r2 `shouldBe` AlreadyDetached
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` True

        it "collapses the window when its last pane detaches" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            _   <- addWindow sess 1     -- another window survives
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (singlePane pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
            r <- atomically (detachPane st win pa)
            r `shouldBe` Detached WindowRemoved SessionSurvives
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` False

        it "reports the session emptied by its last pane" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (singlePane pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
            r <- atomically (detachPane st win pa)
            r `shouldBe` Detached WindowRemoved (SessionsEmptied [SessionId 0])
            (Map.member (SessionId 0) <$> readTVarIO st.sessions)
                `shouldReturn` False

        it "unlinks a closed window from every session linking it" $ do
            (st, sess) <- seedSession "/"
            other <- addSession st 1
            win <- addWindow sess 0
            _ <- addWindow sess 1
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (singlePane pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
                modifyTVar' other.windows (Map.insert 4 win)
            r <- atomically (detachPane st win pa)
            r `shouldBe` Detached WindowRemoved (SessionsEmptied [SessionId 1])
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` False
            (Map.member (SessionId 1) <$> readTVarIO st.sessions)
                `shouldReturn` False

        it "pops the window history so a new last-window remains after a close" $ do
            (st, sess) <- seedSession "/"
            _  <- addWindow sess 0
            _  <- addWindow sess 1
            w2 <- addWindow sess 2
            pa <- stubPane 2
            atomically $ do
                writeTVar w2.layout (singlePane pa.id)
                writeTVar w2.panes (Map.singleton pa.id pa)
                writeTVar sess.currentIx 2
                writeTVar sess.windowHist [1, 0]  -- visited 0, then 1, then 2
            _ <- atomically (detachPane st w2 pa)
            readTVarIO sess.currentIx `shouldReturn` 1
            readTVarIO sess.windowHist `shouldReturn` [0]

        it "pops the pane history so a new last-pane remains after a close" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            [pa, pb, pc] <- mapM stubPane [1, 2, 3]
            atomically $ do
                writeTVar win.layout
                    (nodeOf LeftRight [singlePane pa.id,
                        nodeOf LeftRight [singlePane pb.id, singlePane pc.id]])
                writeTVar win.panes
                    (Map.fromList [(p.id, p) | p <- [pa, pb, pc]])
                writeTVar win.activeId pc.id
                writeTVar win.paneHist [pb.id, pa.id]
            _ <- atomically (detachPane st win pc)
            readTVarIO win.activeId `shouldReturn` pb.id
            readTVarIO win.paneHist `shouldReturn` [pa.id]

    describe "synchronize-panes" $ do
        -- Three panes in one window: which ones a key typed into the first
        -- is copied to (tmux's window_pane_copy_key).
        let syncWindow = do
                (st, sess) <- seedSession "/"
                win <- addWindow sess 0
                ps@[pa, pb, pc] <- mapM stubPane [1, 2, 3]
                atomically $ do
                    writeTVar win.layout
                        (nodeOf LeftRight (map (singlePane . (.id)) ps))
                    writeTVar win.panes (Map.fromList [(p.id, p) | p <- ps])
                    writeTVar win.activeId pa.id
                    mapM_ (\p -> writeTVar p.home (Just (sess.id, win))) ps
                pure (st, win, pa, pb, pc)
            syncOn p = atomically $ modifyTVar' p.options
                (insertDelta "synchronize-panes" (OVScalar "on"))
            targetIds st p = map (.id) <$> atomically (syncTargets st p)
        it "off (the default): nothing is copied" $ do
            (st, _, pa, _, _) <- syncWindow
            targetIds st pa `shouldReturn` []
        it "on for the window: every other pane gets the key" $ do
            (st, win, pa, pb, pc) <- syncWindow
            atomically $ modifyTVar' win.options
                (insertDelta "synchronize-panes" (OVScalar "on"))
            targetIds st pa `shouldReturn` [pb.id, pc.id]
        it "a pane's own value counts, on both ends" $ do
            (st, _, pa, pb, pc) <- syncWindow
            mapM_ syncOn [pa, pc]
            targetIds st pa `shouldReturn` [pc.id]
            targetIds st pb `shouldReturn` []
        it "a pane in a mode or hidden by a zoom is skipped" $ do
            (st, win, pa, pb, pc) <- syncWindow
            mapM_ syncOn [pa, pb, pc]
            atomically $ writeTVar pb.mode (Just (copyModeAt 0))
            targetIds st pa `shouldReturn` [pc.id]
            atomically $ writeTVar win.zoomed (Just pa.id)
            targetIds st pa `shouldReturn` []

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
                (nodeOf LeftRight [singlePane pa.id, singlePane pb.id])
            results <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w0, pb)]
            [ sid | (_, Detached _ (SessionsEmptied sids)) <- results, sid <- sids ]
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
                writeTVar w0.layout (singlePane pa.id)
                writeTVar w1.layout (singlePane pb.id)
            results <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w1, pb)]
            [ sid | (_, Detached _ (SessionsEmptied sids)) <- results, sid <- sids ]
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
                    (nodeOf LeftRight [singlePane pa.id, singlePane pb.id])
                writeTVar w0.panes (Map.fromList [(pa.id, pa), (pb.id, pb)])
                writeTVar w0.activeId pa.id
            -- Break pa out into its own window, as cmdBreakPane does: drop it
            -- from w0 first (the new window is not in the tree yet), then place
            -- it in w1. pa now lives only in w1.
            [] <- atomically (removePaneFromTree st pa.id)
            w1 <- addWindow sess 1
            atomically $ do
                writeTVar w1.layout (singlePane pa.id)
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
                    (nodeOf LeftRight [singlePane pa.id, singlePane pb.id])
                writeTVar w0.panes (Map.fromList [(pa.id, pa), (pb.id, pb)])
                writeTVar w0.activeId pa.id
            flip finally release $ do
                [] <- cmdBreakPane st Nothing []
                ws <- readTVarIO sess.windows
                Map.keys ws `shouldBe` [0, 1]
                Just w1 <- pure (Map.lookup 1 ws)
                layoutPanes <$> readTVarIO w1.layout `shouldReturn` [pa.id]
                (Map.keys <$> readTVarIO w1.panes) `shouldReturn` [pa.id]
                layoutPanes <$> readTVarIO w0.layout `shouldReturn` [pb.id]
                readTVarIO w0.activeId `shouldReturn` pb.id
                readTVarIO sess.currentIx `shouldReturn` 1
                homeOf pa `shouldReturn` Just (SessionId 0, w1.id)

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
            layoutPanes <$> readTVarIO w0.layout `shouldReturn` [pa.id, pc.id]
            readTVarIO w0.activeId `shouldReturn` pc.id
            homeOf pc `shouldReturn` Just (SessionId 0, w0.id)

    -- A window changing session changes its panes' alert/notification
    -- routing; the cached pane home must follow (Note [Pane home]).
    describe "pane home across window moves" $ do
        let paneHomedIn sess win n = do
                p <- stubPane n
                atomically $ do
                    modifyTVar' win.panes (Map.insert p.id p)
                    writeTVar p.home (Just (sess.id, win))
                pure p

        it "move-window to another session re-homes the window's panes" $ do
            (st, sess) <- seedSession "/"           -- "work"
            w0 <- addWindow sess 0
            p <- paneHomedIn sess w0 0
            sess2 <- addSession st 1                -- "s"
            -- target resolution needs a well-formed current view in "s"
            w2 <- addWindow sess2 2
            _ <- paneHomedIn sess2 w2 2
            atomically $ writeTVar sess2.currentIx 2
            r <- cmdMoveWindow st Nothing ["-s", "work:0", "-t", "s:5"]
            r `shouldBe` []
            (Map.member 5 <$> readTVarIO sess2.windows) `shouldReturn` True
            homeOf p `shouldReturn` Just (SessionId 1, w0.id)

        it "unlink-window re-homes panes into a surviving linked session" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            p <- paneHomedIn sess w0 0
            sess2 <- addSession st 1
            atomically $ modifyTVar' sess2.windows (Map.insert 0 w0)
            [] <- cmdUnlinkWindow st Nothing ["-t", "work:0"]
            homeOf p `shouldReturn` Just (SessionId 1, w0.id)

        it "kill-session re-homes a shared window's surviving panes" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            p <- paneHomedIn sess w0 0
            sess2 <- addSession st 1
            atomically $ modifyTVar' sess2.windows (Map.insert 0 w0)
            [] <- cmdKillSession st Nothing ["-t", "work"]
            -- the shared window's pane survives, homed in the survivor
            readTVarIO p.dead `shouldReturn` False
            homeOf p `shouldReturn` Just (SessionId 1, w0.id)

    describe "detach-on-destroy" $ do
        let choice nm act att = SessionChoice
                { session = nm, name = nm, activity = act, attached = att }
            others = [choice "b" 5 True, choice "d" 2 False, choice "a" 5 False]
        it "on detaches the clients" $
            destroyDestination DestroyDetach "c" others `shouldBe` Nothing
        it "off picks the most recently active session, ties by name" $
            destroyDestination DestroySwitch "c" others `shouldBe` Just "a"
        it "no-detached picks the most recently active unattached session" $ do
            destroyDestination DestroyNoDetached "c" (take 2 others)
                `shouldBe` Just "d"
            destroyDestination DestroyNoDetached "c" (take 1 others)
                `shouldBe` Nothing
        it "previous and next step through the names, wrapping" $ do
            destroyDestination DestroyPrevious "c" others `shouldBe` Just "b"
            destroyDestination DestroyNext "c" others `shouldBe` Just "d"
            destroyDestination DestroyPrevious "0" others `shouldBe` Just "d"
            destroyDestination DestroyNext "e" others `shouldBe` Just "a"
        it "previous and next with nothing left detach" $
            destroyDestination DestroyNext "c" ([] :: [SessionChoice Text])
                `shouldBe` Nothing
        it "kill-session moves an attached client off the destroyed session" $ do
            (st, sess) <- seedSession "/"   -- "work"
            older <- addSession st 1
            newer <- addSession st 2
            -- a window shared with the survivors, so the kill reaps no pane
            w0 <- addWindow sess 0
            p <- stubPane 0
            atomically $ do
                modifyTVar' w0.panes (Map.insert p.id p)
                modifyTVar' older.windows (Map.insert 0 w0)
                modifyTVar' newer.windows (Map.insert 0 w0)
                writeTVar older.activity 1
                writeTVar newer.activity 2
                writeTVar sess.options
                    (singletonDelta "detach-on-destroy" (OVScalar "off"))
            client <- addClient st sess.id (Size 24 80) 3
            cmdKillSession st Nothing ["-t", "work"] `shouldReturn` []
            readTVarIO client.session `shouldReturn` newer.id

    describe "destroy-unattached" $ do
        it "destroys by mode and the session's group size" $ do
            map (destroysUnattached KeepUnattached) [Nothing, Just 1, Just 2]
                `shouldBe` [False, False, False]
            map (destroysUnattached DestroyAlways) [Nothing, Just 1, Just 2]
                `shouldBe` [True, True, True]
            map (destroysUnattached DestroyKeepLast) [Nothing, Just 1, Just 2]
                `shouldBe` [False, False, True]
            map (destroysUnattached DestroyKeepGroup) [Nothing, Just 1, Just 2]
                `shouldBe` [True, False, True]
        -- "work" and "s" share a window, so destroying either reaps no pane.
        let sharing setting = do
                (st, sess) <- seedSession "/"   -- "work"
                other <- addSession st 1
                w0 <- addWindow sess 0
                p <- stubPane 0
                atomically $ do
                    modifyTVar' w0.panes (Map.insert p.id p)
                    modifyTVar' other.windows (Map.insert 0 w0)
                    writeTVar st.globalSessionOptions (singletonDelta
                        "destroy-unattached" (OVScalar setting))
                _ <- addClient st other.id (Size 24 80) 1
                pure (st, sess)
            survivors st = map (.id) . Map.elems <$> readTVarIO st.sessions
        it "destroys a session no client is attached to" $ do
            (st, _) <- sharing "on"
            checkUnattached st
            survivors st `shouldReturn` [SessionId 1]
        it "spares it when off" $ do
            (st, _) <- sharing "off"
            checkUnattached st
            survivors st `shouldReturn` [SessionId 0, SessionId 1]
        it "spares a rebuilt session until a client has attached to it" $ do
            (st, sess) <- sharing "on"
            atomically (writeTVar sess.destroyHold HeldUntilAttached)
            checkUnattached st
            survivors st `shouldReturn` [SessionId 0, SessionId 1]

    describe "session groups" $ do
        let grouped = do
                (st, sess) <- seedSession "/"       -- "work"
                w0 <- addWindow sess 0
                w1 <- addWindow sess 1
                ps <- mapM stubPane [0, 1]
                atomically $ V.forM_ (V.fromList (zip [w0, w1] ps)) $ \(w, p) ->
                    modifyTVar' w.panes (Map.insert p.id p)
                atomically $ do
                    writeTVar sess.currentIx 1
                    writeTVar st.nextSession 1
                [] <- cmdNewSession st Nothing ["-d", "-s", "g", "-t", "work"]
                [_, g] <- Map.elems <$> readTVarIO st.sessions
                pure (st, sess, g, ps)

        it "new-session -t shares the window list, with its own current window" $ do
            (_, sess, g, _) <- grouped
            (g.windows == sess.windows) `shouldBe` True
            readTVarIO g.currentIx `shouldReturn` 0
            readTVarIO sess.currentIx `shouldReturn` 1
            mapM (readTVarIO . (.group)) [sess, g]
                `shouldReturn` [Just "work", Just "work"]

        it "killing a member leaves the windows with the rest of the group" $ do
            (st, sess, _, ps) <- grouped
            [] <- cmdKillSession st Nothing ["-t", "g"]
            Map.keys <$> readTVarIO sess.windows `shouldReturn` [0, 1]
            mapM (readTVarIO . (.dead)) ps `shouldReturn` [False, False]

    describe "shared window sizing" $ do
        it "lays a window linked into differently-sized sessions out once" $ do
            (st, sess) <- seedSession "/"
            other <- addSession st 1
            win <- addWindow sess 0
            pa <- stubPane 0
            atomically $ do
                modifyTVar' win.panes (Map.insert pa.id pa)
                modifyTVar' other.windows (Map.insert 5 win)
                writeTVar other.lastSize Size { rows = 23, cols = 80 }
            targets <- atomically (paneSizeTargets st)
            map snd targets `shouldBe` [Size { rows = 24, cols = 80 }]

        it "sizes a window to a control client's size for it, and only that window" $ do
            (st, sess) <- seedSession "/"
            w0 <- addWindow sess 0
            w1 <- addWindow sess 1
            plain <- addClient st sess.id (Size 24 80) 1
            cs <- newControlState
            atomically $ do
                writeTVar cs.size (Just (Size 24 80))
                writeTVar cs.windowSizes (Just (Map.singleton w0.id (Size 10 40)))
                modifyTVar' st.clients (Map.insert plain.id plain { control = Just cs })
            applySessionSize st sess.id
            atomically ((,) <$> windowArea st w0 <*> windowArea st w1)
                `shouldReturn` (Size 10 40, Size 24 80)

        it "takes the area of the session whose client last viewed the window" $
            pickArea
                [ (AreaClaim False (Just 9), Size 10 10)
                , (AreaClaim True (Just 3), Size 20 20)
                , (AreaClaim True Nothing, Size 30 30) ]
                `shouldBe` Size 20 20

    describe "serverIdle" $ do
        let idle = IdleInputs
                { idleActiveConns = 0, idleServed = True, idlePhase = Ready
                , idleSessions = 0, idleClients = 0, idlePanes = 0
                , idleExitEmpty = True, idleExitUnattached = False
                , idleShutdown = Serving }
        it "is idle once served, drained, and every pane reaped" $
            serverIdle idle `shouldBe` ExitServer
        it "stays busy while a pane is still being reaped" $
            -- bug 98: a drained server must outlive its children's reap, so a
            -- SIGHUP-ignoring child is SIGKILLed rather than orphaned on exit.
            serverIdle (idle { idlePanes = 1 }) `shouldBe` StayUp
        it "stays busy until it has served a client" $
            serverIdle (idle { idleServed = False }) `shouldBe` StayUp
        it "stays busy while a connection is still being handled" $
            -- the in-flight connection may yet create the first session.
            serverIdle (idle { idleActiveConns = 1 }) `shouldBe` StayUp
        it "exits after a connection came and went without a session (28)" $
            -- served, no handler left, nothing created: don't strand it.
            serverIdle idle `shouldBe` ExitServer
        it "stays busy until startup lands at Ready" $ do
            serverIdle (idle { idlePhase = LoadingConfig }) `shouldBe` StayUp
            serverIdle (idle { idlePhase = Restoring }) `shouldBe` StayUp
        it "stays busy while any session or client remains" $ do
            serverIdle (idle { idleSessions = 1 }) `shouldBe` StayUp
            serverIdle (idle { idleClients = 1 }) `shouldBe` StayUp
        it "stays up empty under exit-empty off, until killed" $ do
            let keep = idle { idleExitEmpty = False }
            serverIdle keep `shouldBe` StayUp
            serverIdle (keep { idleShutdown = ShuttingDown }) `shouldBe` ExitServer
        it "tears its sessions down once unattached under exit-unattached" $ do
            let lone = idle { idleExitUnattached = True, idleSessions = 1 }
            serverIdle lone `shouldBe` TearDown
            serverIdle (lone { idleActiveConns = 1 }) `shouldBe` StayUp
            serverIdle (lone { idlePhase = Restoring }) `shouldBe` StayUp

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
                (singletonDelta "monitor-activity" (OVScalar "on"))
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
        let offEnc = KeyEncoding { extended = ExtendedOff, format = FormatXterm }
            reraw form = (.raw) <$> reencodeKey offEnc Nothing (Key "Home" form)
        it "normalizes Home to khome (\\E[1~) from any incoming form" $ do
            reraw "\ESC[H"  `shouldReturn` "\ESC[1~"
            reraw "\ESCOH"  `shouldReturn` "\ESC[1~"
            reraw "\ESC[1~" `shouldReturn` "\ESC[1~"
        it "normalizes End to kend (\\E[4~)" $
            ((.raw) <$> reencodeKey offEnc Nothing (Key "End" "\ESCOF"))
                `shouldReturn` "\ESC[4~"
        it "normalizes F1-F4 to the SS3 kf1..kf4 forms from the legacy CSI" $ do
            -- bug fad
            ((.raw) <$> reencodeKey offEnc Nothing (Key "F1" "\ESC[11~"))
                `shouldReturn` "\ESCOP"
            ((.raw) <$> reencodeKey offEnc Nothing (Key "F4" "\ESC[14~"))
                `shouldReturn` "\ESCOS"
        it "leaves an unrecognized sequence's raw bytes alone" $
            -- bug fad
            ((.raw) <$> reencodeKey offEnc Nothing (Key "Unknown" "\ESC[97~"))
                `shouldReturn` "\ESC[97~"

        -- bug 64: a modified character key is spelled by the pane's own key
        -- protocol state, so what the app asked for is what it receives.
        it "encodes a modified key as the app's extended-key mode asks, under extended-keys on" $ do
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            let cEnter = Key "C-Enter" "\r"
                on = KeyEncoding { extended = ExtendedOn, format = FormatXterm }
            ((.raw) <$> reencodeKey on (Just emu) cEnter) `shouldReturn` "\r"
            _ <- Emu.feed emu "\ESC[>4;2m"
            ((.raw) <$> reencodeKey on (Just emu) cEnter)
                `shouldReturn` "\ESC[27;5;13~"
            ((.raw) <$> reencodeKey on { format = FormatCsiU } (Just emu) cEnter)
                `shouldReturn` "\ESC[13;5u"
        it "extended-keys off ignores the app's request" $ do
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            _ <- Emu.feed emu "\ESC[>4;2m"
            ((.raw) <$> reencodeKey offEnc (Just emu) (Key "C-Enter" "\r"))
                `shouldReturn` "\r"
        it "extended-keys always puts every pane in mode 1" $ do
            emu <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            let always = KeyEncoding { extended = ExtendedAlways, format = FormatXterm }
            ((.raw) <$> reencodeKey always (Just emu) (Key "C-Tab" "\t"))
                `shouldReturn` "\ESC[27;5;9~"
            ((.raw) <$> reencodeKey always (Just emu) (Key "C-a" "\SOH"))
                `shouldReturn` "\SOH"

    describe "display-panes" $ do
        let arm :: ServerState -> Client -> [Text] -> IO [Reply]
            arm st client args =
                runArgv st (Just client) ("display-panes" : args)
            -- a window of two stub panes, the first active
            twoPanes sess = do
                win <- addWindow sess 0
                p0 <- stubPane 0
                p1 <- stubPane 1
                atomically $ writeTVar win.panes
                    (Map.fromList [(PaneId 0, p0), (PaneId 1, p1)])
                pure win
        it "arms the issuing client's overlay, template defaulted" $ do
            (st, sess) <- seedSession "/"
            client <- addClient st sess.id (Size 24 80) 1
            arm st client [] `shouldReturn` []
            Just dp <- readTVarIO client.displayPanes
            dp.template `shouldBe` "select-pane -t '%%'"
            dp.hold `shouldBe` PanesClose
            dp.deadline `shouldSatisfy` isJust
        it "-d 0 holds until a key; -N survives non-selecting keys" $ do
            (st, sess) <- seedSession "/"
            client <- addClient st sess.id (Size 24 80) 1
            arm st client ["-N", "-d", "0"] `shouldReturn` []
            Just dp <- readTVarIO client.displayPanes
            dp.deadline `shouldBe` Nothing
            dp.hold `shouldBe` PanesHold
        it "rejects a malformed -d delay loudly" $ do
            (st, sess) <- seedSession "/"
            client <- addClient st sess.id (Size 24 80) 1
            [RErr e] <- arm st client ["-d", "soon"]
            e `shouldSatisfy` T.isInfixOf "bad delay"
        it "a digit selects the pane showing that number and closes" $ do
            (st, sess) <- seedSession "/"
            win <- twoPanes sess
            client <- addClient st sess.id (Size 24 80) 1
            arm st client [] `shouldReturn` []
            handleInput dispatch st client "1"
            readTVarIO win.activeId `shouldReturn` PaneId 1
            readTVarIO client.displayPanes `shouldReturn` Nothing
        it "q dismisses without selecting" $ do
            (st, sess) <- seedSession "/"
            win <- twoPanes sess
            client <- addClient st sess.id (Size 24 80) 1
            arm st client [] `shouldReturn` []
            handleInput dispatch st client "q"
            readTVarIO win.activeId `shouldReturn` PaneId 0
            readTVarIO client.displayPanes `shouldReturn` Nothing
