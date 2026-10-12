-- | The per-client toast: a transient one-line message over the pane area,
-- with the timer that clears it.
module Hat.Server.Toast
    ( showToast
    , showStyledToast
    , showStyledToastFor
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
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Locate (clientOptions)

-- | Show a server message, its text shown as-is.
showToast :: ServerState -> Client -> Text -> IO ()
showToast = showMarkupToast MarkupLiteral

-- | Show a @display-message@ message, its @#[…]@ styles drawn.
showStyledToast :: ServerState -> Client -> Text -> IO ()
showStyledToast = showMarkupToast MarkupStyled

-- | 'showStyledToast' for @ms@ milliseconds (@0@: until a key is pressed)
-- instead of @display-time@.
showStyledToastFor :: ServerState -> Client -> Int -> Text -> IO ()
showStyledToastFor st client ms = showMarkupToastFor MarkupStyled (Just ms) st client

showMarkupToast :: Markup -> ServerState -> Client -> Text -> IO ()
showMarkupToast markup = showMarkupToastFor markup Nothing

showMarkupToastFor :: Markup -> Maybe Int -> ServerState -> Client -> Text -> IO ()
showMarkupToastFor markup mms st client t = do
    opts <- clientOptions st client
    now <- getMonotonicTimeNSec
    let toast = Toast
            { text = t, markup
            , deadline = toastDeadlineMs (maybe opts.displayTime id mms) now }
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
toastDeadline opts = toastDeadlineMs opts.displayTime

-- | 'toastDeadline' for a toast shown @ms@ milliseconds.
toastDeadlineMs :: Int -> Word64 -> Maybe Word64
toastDeadlineMs ms shownAt
    | ms <= 0 = Nothing
    | otherwise = Just (shownAt + fromIntegral ms * 1000000)

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
    RData bytes -> showToast st client (TE.decodeUtf8Lenient bytes)
