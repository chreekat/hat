-- | The per-client pane highlight: the active pane's own edge cells take a
-- tint while the prefix key is armed, and briefly linger on a pane that newly
-- takes focus, whatever moved it there. See 'Hat.Server.View.flashTarget' for
-- what it tints.
module Hat.Server.Flash
    ( armFlash
    , disarmFlash
    , noteActivePane
    , startLingerClock
    , lingerDeadline
    , flashExpired
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, unless, void, when)
import Data.IORef (atomicModifyIORef')
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model

-- | Highlight the active pane; held until the prefix disarms.
armFlash :: ServerState -> Client -> IO ()
armFlash st client = atomically $ setFlash st client (Just FlashArmed)

-- | Leave the armed highlight. When the prefix command moved the active pane
-- the highlight lingers on the new pane to trace the motion; its clock starts
-- once the move's frame is drawn ('startLingerClock'), not here, so the redraw
-- latency never eats into the span. Otherwise it clears at once.
disarmFlash :: ServerState -> Client -> Bool -> IO ()
disarmFlash st client moved
    | moved     = atomically $ setFlash st client (Just FlashLingerPending)
    | otherwise = atomically $ setFlash st client Nothing

-- | Record the active pane a frame is about to show, lingering on it when it
-- differs from the last frame's. An armed prefix already highlights it, and a
-- client's first frame has nothing it moved from.
noteActivePane :: ServerState -> Client -> PaneId -> IO ()
noteActivePane st client active = do
    prev <- atomicModifyIORef' client.shownActive (\p -> (Just active, p))
    when (maybe False (/= active) prev) $ atomically $ do
        cur <- readTVar client.flash
        unless (cur == Just FlashArmed) $ setFlash st client (Just FlashLingerPending)

-- | Start a pending linger's clock the first time its frame is rendered, so
-- the highlight is visible for its full span after the moved-to pane reaches
-- the client. A no-op for any other flash; called once per render.
startLingerClock :: ServerState -> Client -> IO ()
startLingerClock st client = do
    now <- getMonotonicTimeNSec
    let deadline = lingerDeadline now
    started <- atomically $ do
        cur <- readTVar client.flash
        case cur of
            Just FlashLingerPending -> True <$ writeTVar client.flash (Just (FlashLinger deadline))
            _ -> pure False
    when started $ void . forkIO $ do
        -- rounded up past the deadline, as in 'Hat.Server.Toast.showToast'
        threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
        expireFlash st client

-- | The monotonic instant (ns) a linger started at @shownAt@ clears itself.
lingerDeadline :: Word64 -> Word64
lingerDeadline shownAt = shownAt + 200 * 1000000

-- | Whether a flash has timed out at monotonic instant @now@. Only a
-- clock-started linger expires; the armed highlight and a pending linger hold.
flashExpired :: Word64 -> Flash -> Bool
flashExpired _   FlashArmed        = False
flashExpired _   FlashLingerPending = False
flashExpired now (FlashLinger dl)  = dl <= now

-- | Clear the linger once its deadline has passed; a re-armed highlight or a
-- later linger survives an older linger's timer.
expireFlash :: ServerState -> Client -> IO ()
expireFlash st client = do
    now <- getMonotonicTimeNSec
    atomically $ do
        cur <- readTVar client.flash
        forM_ cur $ \f -> when (flashExpired now f) $ setFlash st client Nothing

-- | Store a flash state, repainting only when it actually changes.
setFlash :: ServerState -> Client -> Maybe Flash -> STM ()
setFlash st client next = do
    cur <- readTVar client.flash
    when (cur /= next) $ do
        writeTVar client.flash next
        bumpDirty st
