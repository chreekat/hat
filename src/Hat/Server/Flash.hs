-- | The per-client prefix highlight: the active pane's own edge cells take a
-- tint while the prefix key is armed, and briefly linger on the new pane after
-- a prefix command moves the active pane. See 'Hat.Server.View.flashTarget'
-- for what it tints.
module Hat.Server.Flash
    ( armFlash
    , disarmFlash
    , lingerDeadline
    , flashExpired
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model

-- | Highlight the active pane; held until the prefix disarms.
armFlash :: ServerState -> Client -> IO ()
armFlash st client = atomically $ setFlash st client (Just FlashArmed)

-- | Leave the armed highlight. When the prefix command moved the active pane
-- the highlight lingers on the new pane to trace the motion, then clears on a
-- timer; otherwise it clears at once.
disarmFlash :: ServerState -> Client -> Bool -> IO ()
disarmFlash st client moved
    | not moved = atomically $ setFlash st client Nothing
    | otherwise = do
        now <- getMonotonicTimeNSec
        let deadline = lingerDeadline now
        atomically $ setFlash st client (Just (FlashLinger deadline))
        void . forkIO $ do
            -- rounded up past the deadline, as in 'Hat.Server.Toast.showToast'
            threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
            expireFlash st client

-- | The monotonic instant (ns) a linger shown at @shownAt@ clears itself.
lingerDeadline :: Word64 -> Word64
lingerDeadline shownAt = shownAt + 100 * 1000000

-- | Whether a flash has timed out at monotonic instant @now@. The armed
-- highlight has no deadline, so only a linger ever expires.
flashExpired :: Word64 -> Flash -> Bool
flashExpired _   FlashArmed        = False
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
