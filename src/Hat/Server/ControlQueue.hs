-- | Queueing onto a control-mode client's outbound stream: command-block
-- lines, notifications, pane output, and the request to exit. Everything a
-- control client is sent passes through here; its writer thread (see
-- "Hat.Server.Control") is the only one that touches its socket.
module Hat.Server.ControlQueue
    ( controlEmit
    , controlNotify
    , openBlock
    , closeBlock
    , controlOutput
    , requestExit
    , controlSend
    , outputWatermark
    , forwardToControl
    , awaitControlRoom
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Word (Word64)

import Hat.Model
import Hat.Server.ControlTypes
import Hat.Transport.Wire (ServerToClient (..))

-- | Queue a line that belongs to a command's reply. Unlike a notification
-- it is never dropped: an exiting client still gets the end of the block it
-- was in.
controlEmit :: ControlState -> ByteString -> STM ()
controlEmit cs line = modifyTVar' cs.queue (|> ControlLine line)

-- | Queue a notification line. An exiting client gets no new ones; while a
-- command block is open the line waits for its end, so a notification
-- never lands inside a @%begin@\/@%end@ block.
controlNotify :: ControlState -> ByteString -> STM ()
controlNotify cs line = readTVar cs.exiting >>= \case
    Exiting _ -> pure ()
    Running -> readTVar cs.block >>= \case
        BlockOpen -> modifyTVar' cs.held (|> line)
        BlockClosed -> controlEmit cs line

openBlock :: ControlState -> STM ()
openBlock cs = writeTVar cs.block BlockOpen

-- | End a command block and release the notifications it held.
closeBlock :: ControlState -> STM ()
closeBlock cs = do
    writeTVar cs.block BlockClosed
    held <- swapTVar cs.held Seq.empty
    forM_ held (controlEmit cs)

-- | Queue a pane's output bytes, read at monotonic millisecond @now@. Runs
-- of the same pane coalesce into one item (keeping the older age) until it
-- reaches 'outputChunk'.
controlOutput :: ControlState -> PaneId -> Word64 -> ByteString -> STM ()
controlOutput cs pid now bytes = do
    q <- readTVar cs.queue
    writeTVar cs.queue $ case q of
        rest :|> ControlOutput p t old
            | p == pid, B.length old < outputChunk ->
                rest :|> ControlOutput p t (old <> bytes)
        _ -> q |> ControlOutput pid now bytes
    modifyTVar' cs.outputBytes (+ B.length bytes)

-- | The most raw output bytes one @%output@ line carries.
outputChunk :: Int
outputChunk = 4096

-- | Queued output bytes past which a control client counts as unable to
-- take more (tmux's @CONTROL_BUFFER_HIGH@, in raw rather than escaped
-- bytes). See 'Hat.Server.Control.controlRoom'.
outputWatermark :: Int
outputWatermark = 8192

-- | Mark the client exiting, keeping the first reason given, and discard
-- its queued pane output: only the lines already queued are still sent.
requestExit :: ControlState -> Maybe Text -> STM ()
requestExit cs reason = readTVar cs.exiting >>= \case
    Exiting _ -> pure ()
    Running -> do
        writeTVar cs.exiting (Exiting reason)
        modifyTVar' cs.queue (Seq.filter isLine)
        writeTVar cs.outputBytes 0
  where
    isLine = \case
        ControlLine _ -> True
        ControlOutput {} -> False

-- | What a message the server would send an attached client means to a
-- control client: the lifecycle ones end it, the rest (frames, titles,
-- bells, toasts) have no control-mode form.
controlSend :: ControlState -> ServerToClient -> STM ()
controlSend cs = \case
    DetachOk -> requestExit cs Nothing
    Exited -> requestExit cs Nothing
    RestartClient -> requestExit cs (Just "server exited")
    RestartClientTo _ _ -> requestExit cs (Just "server exited")
    _ -> pure ()

-- | Queue a pane's freshly read output, read at monotonic millisecond @now@,
-- for every control client whose session holds the pane's window and wants
-- its output.
forwardToControl :: ServerState -> Pane -> Word64 -> ByteString -> STM ()
forwardToControl st pane now bytes = do
    watchers <- paneWatchers st pane
    forM_ [ cs | (_, Just cs) <- watchers ] $ \cs -> do
        ex <- readTVar cs.exiting
        fl <- readTVar cs.flags
        flow <- Map.lookup pane.id <$> readTVar cs.panes
        case (ex, fl.output, flow) of
            (Running, SendOutput, Nothing) -> controlOutput cs pane.id now bytes
            _ -> pure ()

-- | Block while a pane's output has nowhere to go: every client whose
-- session holds its window is a control client that cannot take more
-- (tmux stops reading the pane then). Output nobody watches never blocks.
awaitControlRoom :: ServerState -> Pane -> STM ()
awaitControlRoom st pane = do
    watchers <- paneWatchers st pane
    unless (null watchers) $ do
        room <- or <$> mapM (accepts . snd) watchers
        check room
  where
    accepts = \case
        Nothing -> pure True
        Just cs -> do
            ex <- readTVar cs.exiting
            fl <- readTVar cs.flags
            flow <- Map.lookup pane.id <$> readTVar cs.panes
            queued <- readTVar cs.outputBytes
            pure $ case (ex, fl.output, flow) of
                (Exiting _, _, _) -> True
                (_, NoOutput, _) -> True
                (_, _, Just PanePaused) -> True
                (_, _, Just PaneOff) -> False
                _ -> queued < outputWatermark

-- The clients whose session holds the pane's window, with their control
-- state.
paneWatchers :: ServerState -> Pane -> STM [(Client, Maybe ControlState)]
paneWatchers st pane = readTVar pane.home >>= \case
    Nothing -> pure []
    Just (_, win) -> do
        cs <- Map.elems <$> readTVar st.clients
        sessions <- readTVar st.sessions
        filterM' cs $ \c -> do
            sid <- readTVar c.session
            case Map.lookup sid sessions of
                Nothing -> pure Nothing
                Just sess -> do
                    ws <- readTVar sess.windows
                    pure $ if any (\w -> w.id == win.id) ws
                        then Just (c, c.control) else Nothing
  where
    filterM' xs f = catMaybes <$> mapM f xs
