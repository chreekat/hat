-- | A client's outbound wire stream: framed messages queued for the one
-- writer thread that owns its socket ('runWriter'), so no sender ever waits
-- on how fast a client reads.
module Hat.Server.Outbound
    ( Outbound
    , Delivery (..)
    , Slot (..)
    , newOutbound
    , enqueue
    , seal
    , backlog
    , awaitDrained
    , awaitFarewell
    , runWriter
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Sequence (Seq (..), (|>))
import Data.Foldable (toList)
import Data.Sequence qualified as Seq

-- | What a queued message gives way to.
data Delivery
    = InOrder
        -- ^ nothing: it is delivered, in order, however far behind the
        --   client is
    | Superseding Slot
        -- ^ a newer message for the same slot, which replaces it while
        --   it is still queued
    | Expendable
        -- ^ the backlog: it is dropped while the client is behind

-- | A setting of the client's terminal, of which only the newest value
-- matters.
data Slot = TitleSlot | CursorColourSlot | CursorShapeSlot | ClipboardSlot
    deriving (Eq, Show)

data Item = Item
    { slot :: Maybe Slot
    , bytes :: ByteString  -- ^ one framed message
    }

-- | Whether the stream still takes messages.
data Stream
    = Streaming
    | Sealed   -- ^ the client's last message is queued
    | Severed  -- ^ a write failed: nobody is reading any more

data Outbound = Outbound
    { items :: TVar (Seq Item)
    , pending :: TVar Int  -- ^ bytes queued or being written
    , stream :: TVar Stream
    }

newOutbound :: IO Outbound
newOutbound = Outbound
    <$> newTVarIO Seq.empty
    <*> newTVarIO 0
    <*> newTVarIO Streaming

-- | Queue a framed message, as its 'Delivery' allows when @highWater@ or
-- more bytes are already pending.
enqueue :: Outbound -> Int -> Delivery -> ByteString -> STM ()
enqueue ob highWater delivery msg = readTVar ob.stream >>= \case
    Streaming -> case delivery of
        InOrder -> push ob Nothing msg
        Expendable -> do
            behind <- (>= highWater) <$> readTVar ob.pending
            unless behind (push ob Nothing msg)
        Superseding s -> do
            (stale, kept) <- Seq.partition ((== Just s) . (.slot))
                <$> readTVar ob.items
            writeTVar ob.items kept
            modifyTVar' ob.pending (subtract (sum (B.length . (.bytes) <$> stale)))
            push ob (Just s) msg
    Sealed -> pure ()
    Severed -> pure ()

push :: Outbound -> Maybe Slot -> ByteString -> STM ()
push ob s msg = do
    modifyTVar' ob.items (|> Item { slot = s, bytes = msg })
    modifyTVar' ob.pending (+ B.length msg)

-- | Queue the client's last message; the stream takes nothing after it.
seal :: Outbound -> ByteString -> STM ()
seal ob msg = readTVar ob.stream >>= \case
    Streaming -> push ob Nothing msg >> writeTVar ob.stream Sealed
    Sealed -> pure ()
    Severed -> pure ()

-- | Bytes queued or being written.
backlog :: Outbound -> STM Int
backlog ob = readTVar ob.pending

-- | Block until everything queued is written, or nobody is reading.
awaitDrained :: Outbound -> STM ()
awaitDrained ob = readTVar ob.stream >>= \case
    Severed -> pure ()
    _ -> readTVar ob.pending >>= check . (== 0)

-- | Block until the client's last message ('seal') is written, or nobody
-- is reading.
awaitFarewell :: Outbound -> STM ()
awaitFarewell ob = readTVar ob.stream >>= \case
    Streaming -> retry
    Sealed -> awaitDrained ob
    Severed -> pure ()

-- | Write the queued messages with @write@, in order, until a write fails;
-- then drop the rest and take no more.
runWriter :: Outbound -> (ByteString -> IO ()) -> IO ()
runWriter ob write = loop
  where
    loop = do
        next <- atomically $ readTVar ob.stream >>= \case
            Severed -> pure Nothing
            _ -> readTVar ob.items >>= \case
                Empty -> retry
                q -> do
                    let (batch, rest) = takeBatch q
                    Just batch <$ writeTVar ob.items rest
        forM_ next $ \batch -> try (write batch) >>= \case
            Left (_ :: IOException) -> atomically $ do
                writeTVar ob.stream Severed
                writeTVar ob.items Seq.empty
                writeTVar ob.pending 0
            Right () -> do
                atomically $ modifyTVar' ob.pending (subtract (B.length batch))
                loop

-- | The queue's leading messages as one write: at least one, and no more
-- than 'batchBytes' past the first.
takeBatch :: Seq Item -> (ByteString, Seq Item)
takeBatch q = (B.concat (map (.bytes) (toList batch)), rest)
  where
    sizes = Seq.scanl (+) 0 (B.length . (.bytes) <$> q)
    n = max 1 (Seq.length (Seq.takeWhileL (<= batchBytes) (Seq.drop 1 sizes)))
    (batch, rest) = Seq.splitAt n q

-- | The most bytes one write carries, unless a single message is bigger.
batchBytes :: Int
batchBytes = 65536
