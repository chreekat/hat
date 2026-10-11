-- | Server-initiated output to clients: the single choke point every
-- broadcast, reply and render frame passes through. A sender only queues
-- onto the client's 'Outbound'; the client's own writer thread
-- ('withClientWriter') alone touches its socket. See Note [Falling behind].
module Hat.Server.ClientIO
    ( send
    , sendAs
    , sendDirect
    , sendFinal
    , broadcast
    , broadcastAs
    , hasRoom
    , sendFarewells
    , farewellGrace
    , withClientWriter
    ) where

import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM (STM, atomically, orElse, readTVar, readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (filterM, forM_, void, when)
import Data.ByteString (ByteString)
import System.Timeout (timeout)

import Hat.Geometry (Size (..))
import Hat.Model
import Hat.Server.ControlQueue (controlSend, outputWatermark)
import Hat.Server.Outbound
import Hat.Transport.Wire (ServerToClient, frameServerMessageAt, sendFrame)

-- | Send one message to a client, in order; a control client gets its
-- control-mode meaning instead (see 'controlSend').
send :: Client -> ServerToClient -> IO ()
send = sendAs InOrder

-- | 'send' with what the message gives way to when the client falls behind.
sendAs :: Delivery -> Client -> ServerToClient -> IO ()
sendAs delivery client msg = case client.control of
    Just cs -> atomically (controlSend cs msg)
    Nothing -> deliver (\hw -> enqueue client.outbound hw delivery) client msg

-- | 'send' without the control-client detour: a control client's writer
-- thread uses it for the protocol bytes themselves.
sendDirect :: Client -> ServerToClient -> IO ()
sendDirect client = deliver (\hw -> enqueue client.outbound hw InOrder) client

-- | Send a client its last message: nothing sent after it is queued.
sendFinal :: Client -> ServerToClient -> IO ()
sendFinal client = deliver (const (seal client.outbound)) client

-- Frame a message and queue it with @put@, given the client's high water
-- mark. Dropping anything before the client is 'ready' guarantees Welcome
-- (written raw on the socket) is the first thing it sees, even when it
-- attaches to an already-busy session.
deliver :: (Int -> ByteString -> STM ()) -> Client -> ServerToClient -> IO ()
deliver put client msg = do
    isReady <- readTVarIO client.ready
    when isReady $ do
        bytes <- evaluate (frameServerMessageAt client.wireLevel msg)
        atomically (highWater client >>= \hw -> put hw bytes)

-- | Send one message to every client attached to a session.
broadcast :: ServerState -> SessionId -> ServerToClient -> IO ()
broadcast = broadcastAs InOrder

broadcastAs :: Delivery -> ServerState -> SessionId -> ServerToClient -> IO ()
broadcastAs delivery st sid msg = do
    cs <- atomically (sessionClients st sid)
    forM_ cs $ \c -> sendAs delivery c msg

-- Note [Falling behind]
-- ~~~~~~~~~~~~~~~~~~~~~
-- A client that reads slowly or not at all (an XOFF'd or stopped terminal,
-- a stalled link) holds up only itself: every send just queues on its
-- 'Outbound', and only its writer thread waits on the socket. The queue is
-- bounded by a high water mark ('hasRoom'), tmux's bound on a tty's output
-- buffer (@TTY_BLOCK_START@, tty.c) or a control client's
-- (@CONTROL_BUFFER_HIGH@, control.c):
--
-- * The client's own producers -- its renderer, a control client's writer
--   -- wait for room before making more. A frame is a diff against the last
--   one queued, so waiting coalesces frames instead of discarding them, and
--   catching up needs no full redraw. (tmux writes a pane's changes through
--   to its clients' ttys as they happen, so it must discard a blocked tty's
--   output and redraw it once the tty drains.)
-- * What a pane's program sends it unasked -- bells (an alert's included),
--   desktop notifications, @allow-passthrough@ payloads -- is 'Expendable':
--   dropped while it is behind, as tmux drops a blocked tty's output.
-- * A setting of its terminal of which only the current value matters -- the
--   title, the cursor's colour and shape, the clipboard a program sets with
--   OSC 52 -- is 'Superseding' in its own 'Slot': a stale value still queued
--   gives way, the newest is never dropped. (tmux drops these with the rest
--   and re-sends the current state once the tty drains.)
-- * Replies and lifecycle messages go in order whatever the backlog: the
--   client asked for them, or they end it. So do the outer terminal's mouse
--   tracking, which decides what input comes back and must change in step
--   with the frames showing why, and a copy the user made, which must reach
--   the outer clipboard even when a newer copy follows it.
-- * A connection's teardown waits for its writer to finish, as tmux keeps an
--   exiting client until it has read everything. A restart cannot wait
--   that long -- every pane is paused until it re-execs -- so it gives each
--   client a bounded grace to take its farewell ('sendFarewells'); a client
--   still not reading loses it, and its socket closes with the exec.

-- | Whether the client's backlog is below its high water mark: eight bytes
-- a cell for a terminal (tmux's @TTY_BLOCK_START@), @CONTROL_BUFFER_HIGH@
-- for a control client. See Note [Falling behind].
hasRoom :: Client -> STM Bool
hasRoom client = (<) <$> backlog client.outbound <*> highWater client

highWater :: Client -> STM Int
highWater client = case client.control of
    Just _ -> pure outputWatermark
    Nothing -> do
        sz <- readTVar client.size
        pure (1 + fromIntegral sz.cols * fromIntegral sz.rows * 8)

-- | Send each client its farewell ('sendFinal'), then wait up to @grace@
-- (µs) for every one to be written. Returns the clients still not reading
-- by then: they lose their farewell, but hold up no one.
sendFarewells :: Int -> [(Client, ServerToClient)] -> IO [Client]
sendFarewells grace farewells = do
    forM_ farewells $ \(c, msg) -> case c.control of
        Just cs -> atomically (controlSend cs msg)
        Nothing -> sendFinal c msg
    void . timeout grace . atomically $ mapM_ farewellSent clients
    atomically $ filterM (fmap not . sent) clients
  where
    clients = map fst farewells
    sent c = (True <$ farewellSent c) `orElse` pure False

-- An unready client was sent nothing, so it has nothing to wait for.
farewellSent :: Client -> STM ()
farewellSent c = do
    isReady <- readTVar c.ready
    when isReady (awaitFarewell c.outbound)

-- | How long (µs) a restart waits for a client to take its farewell: room
-- for a slow link to drain a full socket buffer ahead of it.
farewellGrace :: Int
farewellGrace = 3_000_000

-- | Run a connection's life with its writer thread draining its
-- 'Outbound', and on the way out let the writer finish what was queued.
withClientWriter :: Client -> IO a -> IO a
withClientWriter client body =
    withAsync (runWriter client.outbound (sendFrame client.sock)) $ \w -> do
        link w
        r <- body
        atomically (awaitDrained client.outbound)
        pure r
