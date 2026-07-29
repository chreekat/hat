-- | The server must never send a client anything before its Welcome.
-- 'send' — the choke point for broadcasts and render frames alike —
-- enforces this by dropping messages until the client is marked ready.
-- Regression guard for the "unexpected greeting" bug: attaching to an
-- already-busy (restored) session used to leak a SetTitle before Welcome.
module Hat.Server.SendSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Data.IORef (newIORef)
import Network.Socket
    (Family (AF_UNIX), Socket, SocketType (Stream), close, socketPair)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
import Hat.Server.ClientIO (send)
import Hat.Server.Keys (PrefixState (NoPrefix))
import Hat.Server.Render (blankFrame)
import Hat.Transport.Wire
    (Inbound (..), ServerToClient (..), protocolVersion, recvMessage)

-- A Client wired to one end of a socketpair (returned second), initially
-- not ready.
mkClient :: IO (Client, Socket)
mkClient = do
    (a, b) <- socketPair AF_UNIX Stream 0
    let sz = Size { rows = 24, cols = 80 }
    lock    <- newMVar ()
    sizeV   <- newTVarIO sz
    activeV <- newTVarIO 0
    sessV   <- newTVarIO (SessionId 0)
    lastV   <- newTVarIO Nothing
    readyV  <- newTVarIO False
    keyV    <- newIORef NoPrefix
    frameV  <- newIORef (blankFrame sz)
    curV    <- newIORef (Pos 0 0, True)
    fullV   <- newTVarIO True
    toastV  <- newTVarIO Nothing
    promptV <- newTVarIO Nothing
    pickV   <- newTVarIO Nothing
    focusV  <- newTVarIO True
    let client = Client
            { id = ClientId 0, sock = a, wireLevel = protocolVersion
            , sendLock = lock, size = sizeV
            , lastActive = activeV
            , session = sessV, lastSession = lastV, ready = readyV
            , keyState = keyV, lastFrame = frameV, lastCursor = curV
            , needsFull = fullV, toast = toastV, prompt = promptV
            , picker = pickV, outerFocused = focusV, env = [], cwd = "" }
    pure (client, b)

recv :: Socket -> IO (Maybe (Inbound ServerToClient))
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
        close peer
        close client.sock

    it "delivers once the client is marked ready" $ do
        (client, peer) <- mkClient
        atomically $ writeTVar client.ready True
        send client (SetTitle "after")
        recv peer `shouldReturn` Just (Known (SetTitle "after"))
        close peer
        close client.sock
