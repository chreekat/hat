-- | The per-client toast: a transient one-line message over the pane area,
-- with the timer that clears it.
module Hat.Server.Toast
    ( showToast
    , toastReplies
    , expireToast
    , dismissToast
    , toastDeadline
    , toastExpired
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Locate (clientOptions)

showToast :: ServerState -> Client -> Text -> IO ()
showToast st client t = do
    opts <- clientOptions st client
    now <- getMonotonicTimeNSec
    let toast = Toast { text = t, deadline = toastDeadline opts now }
    atomically $ do
        writeTVar client.toast (Just toast)
        bumpDirty st
    forM_ toast.deadline $ \deadline -> void . forkIO $ do
        -- round up so the wakeup lands past the deadline
        threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
        expireToast st client

-- | @display-time@: the monotonic instant (ns) a toast shown at @shownAt@
-- times out, or 'Nothing' for @0@ = until a key is pressed.
toastDeadline :: Options -> Word64 -> Maybe Word64
toastDeadline opts shownAt
    | opts.displayTime <= 0 = Nothing
    | otherwise = Just (shownAt + fromIntegral opts.displayTime * 1000000)

-- | Whether a toast's deadline has passed at monotonic instant @now@.
toastExpired :: Word64 -> Toast -> Bool
toastExpired now toast = maybe False (<= now) toast.deadline

-- | Clear the toast once its deadline has passed; a fresher toast (a later
-- deadline, or none) survives an older toast's timer.
expireToast :: ServerState -> Client -> IO ()
expireToast st client = do
    now <- getMonotonicTimeNSec
    atomically $ do
        cur <- readTVar client.toast
        forM_ cur $ \toast -> when (toastExpired now toast) $ do
            writeTVar client.toast Nothing
            bumpDirty st

-- | Clear the toast on a key press — the only dismissal under
-- @display-time 0@.
dismissToast :: ServerState -> Client -> IO ()
dismissToast st client = atomically $ do
    cur <- readTVar client.toast
    forM_ cur $ \_ -> do
        writeTVar client.toast Nothing
        bumpDirty st

-- | Show a command's replies to the client that asked for it, errors marked.
-- Only the last line survives on screen; a command that means to report more
-- than one line has a pager, not a toast.
toastReplies :: ServerState -> Client -> [Reply] -> IO ()
toastReplies st client replies = forM_ replies $ \case
    ROutput out -> showToast st client out
    RErr e -> showToast st client ("error: " <> e)
