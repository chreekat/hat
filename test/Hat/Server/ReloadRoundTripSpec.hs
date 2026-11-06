-- | A reload's two halves run in one process, through the bytes the exec
-- path hands over: what the payload carries is pinned here; what only an
-- exec shows (fd, pid and lock survival, the listen backlog, the probe of a
-- real binary) stays with the integration suite.
module Hat.Server.ReloadRoundTripSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
    (atomically, check, modifyTVar', readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad (filterM, void)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Posix.Process (ProcessStatus (..))
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Server.Dispatch
    (ReloadScope (..), captureHandover, cmdRestartServer, farewellClients
    , reloadFarewells, retireBeforeExec)
import Hat.Server.Handover (ScrollbackCarry (..), rebuildReload)
import Hat.Server.Layout (fitLayout, singlePane)
import Hat.Server.LayoutString (emitLayout)
import Hat.Server.Persist (PaneSnap (..))
import Hat.Server.Reload
    (Handover (..), HotPane (..), HotSession (..), HotWindow (..), ReloadDeath (..)
    , ReloadModes (..), ReloadStart (..), ReloadTree (..), decodeHandover
    , emptyReloadScreen)
import Hat.Server.Command.Types (Reply (..))
import Hat.Term.Pty qualified as Pty
import Hat.TestSupport (addClient)
import Hat.Transport.Wire (ServerToClient (RestartClientTo))

-- | The outgoing image's pre-exec sequence — capture and encode, farewells,
-- the teardown of what no handover carries — and the next image's decode and
-- adoption, into a fresh server. Everything 'cmdRestartServer' runs but the
-- exec's own bookkeeping: the handover file, the fd flags, the argv.
reloadInProcess :: ServerState -> IO ServerState
reloadInProcess st = do
    parkAcceptOnClose st
    (_, blob) <- captureHandover KeepScrollback st
    farewellClients ServerOnly st Nothing
    retireBeforeExec st
    Handover { tree = decoded } <-
        either (fail . ("handover: " <>) . T.unpack) pure (decodeHandover blob)
    tree <- either (fail . ("handover tree: " <>) . T.unpack) pure decoded
    st' <- freshState
    st' <$ rebuildReload st' tree

-- The accept loop's half of the gate handshake 'farewellClients' waits on,
-- for a server running no accept loop.
parkAcceptOnClose :: ServerState -> IO ()
parkAcceptOnClose st = void . forkIO . atomically $ do
    g <- readTVar st.acceptGate
    check (g == AcceptClosing)
    writeTVar st.acceptGate AcceptParked

freshState :: IO ServerState
freshState = do
    lg <- newLogger "/dev/null"
    newServerState Map.empty lg "/tmp/hat-reloadroundtripspec.sock" Nothing

-- | A server whose tree is @sessions@, each one window around one pane. Dead
-- and empty panes only: a live pane's master would be owned twice across an
-- in-process round trip, by the old tree's handle and the new tree's.
serverWith :: [(Text, (PaneSnap, HotPane))] -> IO ServerState
serverWith sessions = do
    st <- freshState
    st <$ rebuildReload st ReloadTree
        { sessions =
            [ HotSession
                { name = nm, startCwd = "/tmp", currentIx = 0, windowHist = []
                , createdAt = Nothing, restores = 0, group = Nothing
                , windows =
                    [ HotWindow
                        { ix = 0, name = "sh", layout = lay, active = 0
                        , paneHist = [], autoRename = True, key = Nothing
                        , linkOf = Nothing, modal = Nothing, size = Nothing
                        , panes = [p] } ] }
            | (nm, p) <- sessions ]
        , currentSession = Nothing, lastSession = Nothing }
  where
    lay = emitLayout (fitLayout Size { rows = 24, cols = 80 } (singlePane (PaneId 0)))

emptyPane :: (PaneSnap, HotPane)
emptyPane = (snapOf EmptyPane, hotOf StartShell Nothing)

-- | A @remain-on-exit@ pane whose @sh -c cmd@ exited with @status@.
deadPane :: Text -> Int -> (PaneSnap, HotPane)
deadPane cmd status =
    ( snapOf ProgramPane
    , hotOf (StartCommand cmd)
        (Just ReloadDeath { status = Just status, signal = Nothing, diedAt = 1_700_000_000 }) )

snapOf :: PaneKind -> PaneSnap
snapOf k = PaneSnap
    { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = k
    , inputOff = False, title = Nothing, titleStack = [] }

hotOf :: ReloadStart -> Maybe ReloadDeath -> HotPane
hotOf start death = HotPane
    { masterFd = -1, childPid = 4242
    , modes = ReloadModes False False 0 False 0 0 [], screen = emptyReloadScreen
    , start = start, death = death }

sessionNamed :: ServerState -> Text -> IO Session
sessionNamed st nm = do
    hits <- filterM (fmap (== nm) . readTVarIO . (.name)) . Map.elems
        =<< readTVarIO st.sessions
    maybe (fail ("no session " <> T.unpack nm)) pure (listToMaybe hits)

sessionNames :: ServerState -> IO [Text]
sessionNames st = mapM (readTVarIO . (.name)) . Map.elems =<< readTVarIO st.sessions

nameOf :: ServerState -> Maybe SessionId -> IO (Maybe Text)
nameOf st msid = do
    sessions <- readTVarIO st.sessions
    traverse (readTVarIO . (.name)) (msid >>= (`Map.lookup` sessions))

panesOf :: ServerState -> IO [Pane]
panesOf st = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    wins <- concat <$> mapM (fmap Map.elems . readTVarIO . (.windows)) sessions
    concat <$> mapM (fmap Map.elems . readTVarIO . (.panes)) wins

-- | A popup's command — a sleeper on its own pty — registered with the
-- server the way 'Hat.Server.PopupIO.openPopup' registers one, and killed
-- after the body if it is still running.
withPopupJob :: ServerState -> (Pty.PtyHandle -> IO a) -> IO a
withPopupJob st = bracket start stop
  where
    start = do
        pty <- Pty.spawn Pty.Spawn
            { cmd = "sleep", args = ["1000"], env = [], cwd = Nothing
            , size = Size { rows = 5, cols = 20 } }
        pty <$ atomically (modifyTVar' st.popupJobs (Map.insert (Pty.pid pty) pty))
    stop pty = Pty.signalKill pty >> void (Pty.waitExit pty)

spec :: Spec
spec = do
    describe "reloadInProcess" $ do
        -- Bug cb: a reload must not forget the alternate session, so a
        -- reattached client's switch-client -l still toggles back.
        it "carries the current and the alternate session, so switch-client -l still toggles (cb)" $ do
            st <- serverWith [("0", emptyPane), ("work", emptyPane)]
            zero <- sessionNamed st "0"
            work <- sessionNamed st "work"
            atomically $ do
                writeTVar st.lastActiveSession (Just work.id)
                writeTVar st.lastSession (Just zero.id)
            st' <- reloadInProcess st
            (readTVarIO st'.lastActiveSession >>= nameOf st') `shouldReturn` Just "work"
            (readTVarIO st'.lastSession >>= nameOf st') `shouldReturn` Just "0"

        it "round-trips a non-ASCII session name" $ do
            st <- serverWith [("caf\233", emptyPane)]
            st' <- reloadInProcess st
            sessionNames st' `shouldReturn` ["caf\233"]

        it "keeps a dead pane dead, with its status and the command respawn-pane reruns" $ do
            st <- serverWith [("0", deadPane "echo run >> runs; exit 7" 7)]
            st' <- reloadInProcess st
            [pane] <- panesOf st'
            fmap (.status) <$> readTVarIO pane.exited `shouldReturn` Just (Just 7)
            Pty.waitExit pane.pty `shouldReturn` Just (Exited (ExitFailure 7))
            pane.start `shouldBe` ShellCommand "echo run >> runs; exit 7"

        -- A popup's command is no pane: nothing carries it, so it must not
        -- outlive the old image (the 903 integration test's claim).
        it "ends a popup's command before the exec" $ do
            st <- serverWith [("0", emptyPane)]
            withPopupJob st $ \job -> do
                _ <- reloadInProcess st
                ended <- timeout 1_000_000 (Pty.waitExit job)
                ended `shouldSatisfy` isJust

    -- The missing half of RestoreSpec's "no such binary" guard: a refused
    -- reload has torn nothing down, so the server keeps serving as it was.
    describe "a refused reload" $
        it "leaves the readers flowing, the accept gate open and the tree intact" $ do
            st <- serverWith [("0", emptyPane)]
            names <- sessionNames st
            [RErr err] <- cmdRestartServer st Nothing ["/no/such/hat"]
            err `shouldSatisfy` T.isInfixOf "no such binary"
            readTVarIO st.readGate `shouldReturn` ReadersFlowing
            readTVarIO st.acceptGate `shouldReturn` AcceptOpen
            sessionNames st `shouldReturn` names

    -- Bug 8d: a restart carries a client's whole last-session stack back
    -- through its farewell, not just the one alternate.
    describe "reloadFarewells" $
        it "names a client's whole last-session stack in its restart farewell (8d)" $ do
            st <- serverWith [("0", emptyPane), ("a", emptyPane), ("b", emptyPane)]
            [zero, a, b] <- mapM (sessionNamed st) ["0", "a", "b"]
            c <- addClient st b.id Size { rows = 24, cols = 80 } 1
            atomically $ writeTVar c.sessionHist [a.id, zero.id]
            fmap snd <$> reloadFarewells ServerAndClients st Nothing
                `shouldReturn` [RestartClientTo "b" ["a", "0"]]
