-- | The server must never send a client anything before its Welcome.
-- 'send' — the choke point for broadcasts and render frames alike —
-- enforces this by dropping messages until the client is marked ready.
-- Regression guard for the "unexpected greeting" bug: attaching to an
-- already-busy (restored) session used to leak a SetTitle before Welcome.
module Hat.Server.SendSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Data.IORef (newIORef)
import Data.Vector qualified as V
import Network.Socket
    (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
import Hat.Server.ClientIO (send)
import Hat.Server.Keys (EscPending (NoEscPending), PrefixState (NoPrefix))
import Hat.Server.Render (blankFrame)
import Hat.Transport.Wire
    ( Autostart (..), Inbound (..), ReadEnd (..), ServerToClient (..)
    , newReadEnd, protocolVersion, recvMessage )

-- A Client wired to one end of a socketpair (returned second), initially
-- not ready.
mkClient :: IO (Client, ReadEnd)
mkClient = do
    (a, b) <- socketPair AF_UNIX Stream 0
    let sz = Size { rows = 24, cols = 80 }
    lock    <- newMVar ()
    sizeV   <- newTVarIO sz
    activeV <- newTVarIO 0
    sessV   <- newTVarIO (SessionId 0)
    lastV   <- newTVarIO []
    readyV  <- newTVarIO False
    keyV    <- newIORef NoPrefix
    escV    <- newIORef NoEscPending
    frameV  <- newIORef (blankFrame sz)
    originsV <- newIORef V.empty
    chromeV <- newIORef Nothing
    statusV <- newIORef Nothing
    curV    <- newIORef (Pos 0 0, True)
    colourV <- newIORef ""
    fullV   <- newTVarIO True
    toastV  <- newTVarIO Nothing
    flashV  <- newTVarIO Nothing
    promptV <- newTVarIO Nothing
    pickV   <- newTVarIO Nothing
    focusV  <- newTVarIO True
    envImpV <- newTVarIO ImportEnv
    let client = Client
            { id = ClientId 0, role = Attached, autostart = Joined, sock = a, wireLevel = protocolVersion
            , sendLock = lock, size = sizeV
            , lastActive = activeV
            , session = sessV, sessionHist = lastV, ready = readyV
            , keyState = keyV, escState = escV, lastFrame = frameV
            , lastOrigins = originsV, lastCursor = curV
            , lastChrome = chromeV
            , lastStatus = statusV
            , lastCursorColour = colourV
            , needsFull = fullV, toast = toastV, flash = flashV, prompt = promptV
            , picker = pickV, outerFocused = focusV, envImport = envImpV
            , env = [], cwd = "" }
    re <- newReadEnd b
    pure (client, re)

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
        send client (SetTitle "after")
        recv peer `shouldReturn` Just (Known (SetTitle "after"))
        close peer.sock
        close client.sock
