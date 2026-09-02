-- | The per-client prefix flash: the active pane's border briefly takes
-- a tint when the prefix key arms, with the timer that clears it. See
-- 'Hat.Server.View.flashTarget' for what it tints.
module Hat.Server.Flash
    ( showFlash
    , dismissFlash
    , flashDeadline
    , flashExpired
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model

-- | Show the flash and start the timer that clears it.
showFlash :: ServerState -> Client -> IO ()
showFlash st client = do
    now <- getMonotonicTimeNSec
    let deadline = flashDeadline now
    atomically $ do
        writeTVar client.flash (Just Flash { deadline = deadline })
        bumpDirty st
    void . forkIO $ do
        -- rounded up past the deadline, as in 'Hat.Server.Toast.showToast'
        threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
        expireFlash st client

-- | The monotonic instant (ns) a flash shown at @shownAt@ clears itself.
flashDeadline :: Word64 -> Word64
flashDeadline shownAt = shownAt + 150 * 1000000

-- | Whether a flash's deadline has passed at monotonic instant @now@.
flashExpired :: Word64 -> Flash -> Bool
flashExpired now f = f.deadline <= now

-- | Clear the flash once its deadline has passed; a rearmed (later)
-- flash survives an older flash's timer.
expireFlash :: ServerState -> Client -> IO ()
expireFlash st client = do
    now <- getMonotonicTimeNSec
    atomically $ do
        cur <- readTVar client.flash
        forM_ cur $ \f -> when (flashExpired now f) $ do
            writeTVar client.flash Nothing
            bumpDirty st

-- | Clear the flash on a key press.
dismissFlash :: ServerState -> Client -> IO ()
dismissFlash st client = atomically $ do
    cur <- readTVar client.flash
    forM_ cur $ \_ -> do
        writeTVar client.flash Nothing
        bumpDirty st
