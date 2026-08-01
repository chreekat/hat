module Hat.Server.SessionSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM
import Data.IORef (newIORef)
import System.Timeout (timeout)
import qualified Data.Map.Strict as Map
import Network.Socket
    ( Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socket
    , socketPair, touchSocket )
import System.Mem (performGC)
import Test.Hspec

import qualified Data.Set as Set

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options
    (Options (..), OptionName (..), OptionValue (..)
    , defaultOptions, emptyDelta, singletonDelta)
import Hat.Server
    ( DetachResult (..), SessionFate (..), IdleInputs (..), serverIdle
    , RestartClientOutcome (..), restartClientAction
    , applySessionSize, attentionSeen, awaitReconciled, chooseActivePaneOnClose
    , cmdAttachSession, chooseCurrentOnClose, cmdRestartClient, deliversKey
    , detachPane, detachPaneCurrent, detachPanes, markActivity, markBell
    , nextZoom, noteOuterFocus, pickActivityTarget, pickAttachSession
    , removePaneFromTree, welcome )
import Hat.Server.Environ (emptyEnviron)
import Hat.Server.Keys (EscPending (NoEscPending), Key (..), PrefixState (NoPrefix))
import Hat.Transport.Wire
    ( Autostart (..), ClientToServer (..), Hello (..), Inbound (..), Intent (..)
    , ServerToClient (..), protocolVersion, recvMessage, sendMessage )
import Hat.Server.Layout (Layout (..), Orientation (LeftRight))
import Hat.Server.Render (blankFrame)

-- A bare session with the given id inserted into an existing server.
addSession :: ServerState -> Int -> IO Session
addSession st n = do
    sess <- Session (SessionId n)
        <$> newTVarIO "s"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO Nothing
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
        <*> newTVarIO Nothing
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
        <*> newTVarIO Nothing
        <*> newTVarIO False
        <*> newTVarIO False
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
        <*> newTVarIO Nothing
        <*> newTVarIO True
        <*> newIORef NoPrefix
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame sz)
        <*> newIORef (Pos 0 0, True)
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO True     -- outerFocused
        <*> pure []
        <*> pure ""
    atomically $ modifyTVar' st.clients (Map.insert client.id client)
    pure client

-- A client whose socket is one half of a socketpair, so a test can read what
-- the server sends it. 'ready' is armed so 'send' does not drop the message.
wiredClient :: ServerState -> ClientRole -> IO (Client, Socket)
wiredClient st clientRole = do
    (server, peer) <- socketPair AF_UNIX Stream defaultProtocol
    lock <- newMVar ()
    client <- Client (ClientId 1) clientRole Joined server protocolVersion lock
        <$> newTVarIO (Size 24 80)
        <*> newTVarIO 0
        <*> newTVarIO (SessionId 0)
        <*> newTVarIO Nothing
        <*> newTVarIO True    -- ready
        <*> newIORef NoPrefix
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame (Size 24 80))
        <*> newIORef (Pos 0 0, True)
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO True
        <*> pure []
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
    pure Pane
        { id = PaneId n
        , pty = error "stubPane: pty is never touched by detachPane"
        , emulator = error "stubPane: emulator is never touched by detachPane"
        , size = sizeV
        , dead = deadV
        , startCwd = "/"
        , mode = modeV
        , pipe = pipeV
        , readerTid = tidV
        }

spec :: Spec
spec = do
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
            withAsync (welcome st server h) $ \_ -> do
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

        it "sends RestartClient to the attached client that issued it" $ do
            (st, _) <- seedSession "/"
            (client, peer) <- wiredClient st Attached
            [] <- cmdRestartClient st (Just client) []
            recvMessage peer `shouldReturn` Just (Known RestartClient)

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

    describe "aggressive-resize sizing" $ do
        let big = Size { rows = 50, cols = 200 }
            small = Size { rows = 24, cols = 80 }
        it "sizes the session to the smallest client by default" $ do
            (st, sess) <- seedSession "/"
            _ <- addClient st (SessionId 0) big 1
            _ <- addClient st (SessionId 0) small 2   -- smaller client is newer
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` small
        it "follows the most-recently-active client under aggressive-resize" $ do
            (st, sess) <- seedSession "/"
            atomically $ writeTVar st.globalWindowOptions
                (singletonDelta OptAggressiveResize (OVBool True))
            _ <- addClient st (SessionId 0) big 2      -- bigger client is newer
            _ <- addClient st (SessionId 0) small 1
            applySessionSize st (SessionId 0)
            readTVarIO sess.lastSize `shouldReturn` big

    describe "attach-session -c" $
        -- The feature behind the author's @M-c@ binding: re-anchor where
        -- new windows start. Pure server state — set by the command, read
        -- by new-window — so a state check suffices, no client needed.
        it "re-anchors the session's default working directory" $ do
            (st, sess) <- seedSession "/start"
            _ <- cmdAttachSession st Nothing ["-c", "/re/anchored"]
            readTVarIO sess.startCwd `shouldReturn` "/re/anchored"

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

    describe "chooseCurrentOnClose" $ do
        -- Closing the current window should jump to the last-active window,
        -- mirroring tmux, not to the lowest-numbered survivor.
        let remaining = Set.fromList [0, 2, 3]

        it "keeps the current window when it survives the close" $
            -- current still present (some other window closed): no change.
            chooseCurrentOnClose remaining 2 (Just 0) `shouldBe` Nothing

        it "jumps to the last-active window when the current is gone" $
            -- current window 1 was closed; last-active was 3, still alive.
            chooseCurrentOnClose remaining 1 (Just 3) `shouldBe` Just 3

        it "falls back to the lowest survivor when there is no last-active" $
            chooseCurrentOnClose remaining 1 Nothing `shouldBe` Just 0

        it "falls back to the lowest survivor when the last-active is also gone" $
            -- last-active pointed at window 5, which no longer exists.
            chooseCurrentOnClose remaining 1 (Just 5) `shouldBe` Just 0

    describe "chooseActivePaneOnClose" $ do
        -- Closing the active pane should reactivate the window's last-active
        -- pane, mirroring tmux, not an arbitrary surviving neighbour.
        let survivors = map PaneId [2, 5, 7]

        it "reactivates the last-active pane when it survives the close" $
            -- active pane 5 was closed; last-active was 7, still alive.
            chooseActivePaneOnClose survivors (Just (PaneId 7))
                `shouldBe` Just (PaneId 7)

        it "falls back to the first surviving pane when there is no last-active" $
            chooseActivePaneOnClose survivors Nothing
                `shouldBe` Just (PaneId 2)

        it "falls back to the first survivor when the last-active is also gone" $
            -- last-active pointed at pane 9, which no longer exists.
            chooseActivePaneOnClose survivors (Just (PaneId 9))
                `shouldBe` Just (PaneId 2)

        it "has no choice when no panes remain" $
            chooseActivePaneOnClose [] (Just (PaneId 7)) `shouldBe` Nothing

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
            r `shouldBe` Detached SessionSurvives
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
            r `shouldBe` Detached SessionSurvives
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` False

        it "reports the session emptied by its last pane" $ do
            (st, sess) <- seedSession "/"
            win <- addWindow sess 0
            pa <- stubPane 1
            atomically $ do
                writeTVar win.layout (Leaf pa.id)
                writeTVar win.panes (Map.singleton pa.id pa)
            r <- atomically (detachPane st (SessionId 0) win pa)
            r `shouldBe` Detached SessionEmptied
            (Map.member (SessionId 0) <$> readTVarIO st.sessions)
                `shouldReturn` False

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
            emptied <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w0, pb)]
            emptied `shouldBe` []
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
            emptied <- atomically $ detachPanes st
                [(SessionId 0, w0, pa), (SessionId 0, w1, pb)]
            emptied `shouldBe` [SessionId 0]
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
            r `shouldBe` Detached SessionSurvives
            -- w1 (pa's current window) collapsed; w0 survives with pb.
            (Map.member 1 <$> readTVarIO sess.windows) `shouldReturn` False
            (Map.member 0 <$> readTVarIO sess.windows) `shouldReturn` True
            (Map.member pb.id <$> readTVarIO w0.panes) `shouldReturn` True

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
