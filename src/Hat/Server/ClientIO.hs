-- | Server-initiated output to attached clients: the single choke point
-- every broadcast and render frame passes through.
module Hat.Server.ClientIO
    ( send
    , broadcast
    ) where

import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)

import Hat.Model
import Hat.Transport.Wire (ServerToClient, sendMessageAt)

-- | Every server-initiated message except the Welcome/ServerError handshake
-- (sent raw on the socket) goes through here — both broadcasts and a
-- client's own render frames. Dropping anything before the client is
-- 'ready' guarantees Welcome is the first byte it sees, even when it is
-- attaching to an already-busy (e.g. restored) session.
send :: Client -> ServerToClient -> IO ()
send client msg = do
    isReady <- readTVarIO client.ready
    when isReady $
        withMVar client.sendLock
            (\_ -> sendMessageAt client.wireLevel client.sock msg)
            `catch` \(_ :: SomeException) -> pure ()

-- | Send one message to every client attached to a session.
broadcast :: ServerState -> SessionId -> ServerToClient -> IO ()
broadcast st sid msg = do
    cs <- atomically (sessionClients st sid)
    forM_ cs $ \c -> send c msg
