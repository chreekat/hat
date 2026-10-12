-- | 'send', the choke point for every server-initiated message: nothing
-- reaches a client before its Welcome (regression guard for the "unexpected
-- greeting" bug), and no client's socket ever holds up a sender.
module Hat.Server.SendSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, writeTVar)
import Control.Exception (IOException, try)
import Data.ByteString qualified as B
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Network.Socket
    (Family (AF_UNIX), SocketType (Stream), close, socketPair, withFdSocket)
import System.Posix.IO (FdOption (NonBlockingRead), setFdOption)
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Server.Outbound (newOutbound)
import Hat.Server.ClientIO (broadcast, send, sendFarewells, withClientWriter)
import Hat.Server.Keys (EscPending (NoEscPending), initialKeyState)
import Hat.Server.Render (blankFrame)
import Hat.Transport.Wire
    ( Autostart (..), Inbound (..), ReadEnd (..), ServerToClient (..)
    , newReadEnd, protocolVersion, recvMessage )

-- A Client wired to one end of a socketpair (returned second), initially
-- not ready.
mkClient :: IO (Client, ReadEnd)
mkClient = mkClientWith (ClientId 0)

mkClientWith :: ClientId -> IO (Client, ReadEnd)
mkClientWith cid = do
    (a, b) <- socketPair AF_UNIX Stream 0
    let sz = Size { rows = 24, cols = 80 }
    outboundQ <- newOutbound
    sizeV   <- newTVarIO sz
    activeV <- newTVarIO 0
    sessV   <- newTVarIO (SessionId 0)
    lastV   <- newTVarIO []
    readyV  <- newTVarIO False
    keyV    <- newIORef initialKeyState
    escV    <- newIORef NoEscPending
    frameV  <- newIORef (blankFrame sz)
    originsV <- newIORef V.empty
    chromeV <- newIORef Nothing
    statusV <- newIORef Nothing
    expansionsV <- newIORef Map.empty
    mouseModeVar <- newIORef 0
    curV    <- newIORef (Pos 0 0, True)
    colourV <- newIORef Nothing
    shapeVar <- newIORef CursorDefault
    fullV   <- newTVarIO True
    toastV  <- newTVarIO Nothing
    flashV  <- newTVarIO Nothing
    promptV <- newTVarIO Nothing
    pickV   <- newTVarIO Nothing
    popupV  <- newTVarIO Nothing
    dpanesV <- newTVarIO Nothing
    focusV  <- newTVarIO True
    envImpV <- newTVarIO ImportEnv
    let client = Client
            { id = cid, role = Attached, autostart = Joined, sock = a, wireLevel = protocolVersion
            , outbound = outboundQ, size = sizeV
            , lastActive = activeV
            , session = sessV, sessionHist = lastV, ready = readyV
            , keyState = keyV, escState = escV, lastFrame = frameV
            , lastOrigins = originsV, lastCursor = curV
            , lastChrome = chromeV
            , lastStatus = statusV
            , lastExpansions = expansionsV
            , lastMouseMode = mouseModeVar
            , lastCursorColour = colourV, lastCursorShape = shapeVar
            , needsFull = fullV, toast = toastV, flash = flashV, prompt = promptV
            , picker = pickV, popup = popupV, displayPanes = dpanesV
            , outerFocused = focusV, envImport = envImpV
            , env = [], cwd = "", stdinSource = NoStdin, control = Nothing }
    re <- newReadEnd b
    pure (client, re)

-- A ready client registered on session 0, as attaching leaves it.
attachedClient :: ServerState -> ClientId -> IO (Client, ReadEnd)
attachedClient st cid = do
    (client, peer) <- mkClientWith cid
    atomically $ do
        writeTVar client.ready True
        modifyTVar' st.clients (Map.insert cid client)
    pure (client, peer)

-- Fill the client's socket until the kernel takes no more, as a peer that
-- has stopped reading leaves it.
wedge :: Client -> IO ()
wedge client = withFdSocket client.sock $ \fd -> do
    setFdOption (Fd fd) NonBlockingRead True
    let fill = try (fdWrite (Fd fd) (B.replicate 4096 0)) >>= \case
            Left (_ :: IOException) -> pure ()
            Right _ -> fill
    fill

recv :: ReadEnd -> IO (Maybe (Inbound ServerToClient))
recv peer = do
    r <- timeout 250_000 (recvMessage peer)
    -- collapse "timed out" and "socket closed" into Nothing
    pure (maybe Nothing Prelude.id r)

spec :: Spec
spec = describe "send" $ do
    it "drops a broadcast to a client that is not yet ready" $ do
        (client, peer) <- mkClient
        send client (SetTitle "before")
        recv peer `shouldReturn` Nothing
        close peer.sock
        close client.sock

    it "delivers once the client is marked ready" $ do
        (client, peer) <- mkClient
        atomically $ writeTVar client.ready True
        withClientWriter client (send client (SetTitle "after"))
        recv peer `shouldReturn` Just (Known (SetTitle "after"))
        close peer.sock
        close client.sock

    it "delivers a client's messages in order, the last one before teardown" $ do
        (client, peer) <- mkClient
        atomically $ writeTVar client.ready True
        withClientWriter client $
            mapM_ (send client) [SetTitle "a", RingBell, Exited]
        recv peer `shouldReturn` Just (Known (SetTitle "a"))
        recv peer `shouldReturn` Just (Known RingBell)
        recv peer `shouldReturn` Just (Known Exited)
        close peer.sock
        close client.sock

    it "a client that stops reading holds up no broadcast to the others" $ do
        lg <- newLogger "/dev/null"
        st <- newServerState Map.empty lg "/tmp/hat-sendspec.sock" Nothing
        (stuck, stuckPeer) <- attachedClient st (ClientId 0)
        (live, livePeer) <- attachedClient st (ClientId 1)
        wedge stuck
        withClientWriter stuck . withClientWriter live $ do
            timeout 2_000_000 (broadcast st (SessionId 0) RingBell)
                `shouldReturn` Just ()
            recv livePeer `shouldReturn` Just (Known RingBell)
            close stuckPeer.sock
        close livePeer.sock
        mapM_ (close . (.sock)) [stuck, live]

    it "a client that stops reading holds up no other client's farewell" $ do
        (stuck, stuckPeer) <- mkClientWith (ClientId 0)
        (live, livePeer) <- mkClientWith (ClientId 1)
        atomically $ mapM_ (\c -> writeTVar c.ready True) [stuck, live]
        wedge stuck
        withClientWriter stuck . withClientWriter live $ do
            unsent <- timeout 2_000_000
                (sendFarewells 100_000 [(stuck, Exited), (live, RestartClient)])
            map (.id) <$> unsent `shouldBe` Just [ClientId 0]
            recv livePeer `shouldReturn` Just (Known RestartClient)
            close stuckPeer.sock
        close livePeer.sock
        mapM_ (close . (.sock)) [stuck, live]
