-- | Adding to the server's message log ("Hat.Model.MessageLog"): each
-- command as it runs and each message shown to a client, as tmux's
-- @server_add_message@ callers word them.
module Hat.Server.MessageLog
    ( logMessage
    , logShownMessage
    , logCommand
    , commandMessage
    ) where

import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Hat.Model
import Hat.Model.MessageLog (addMessage)
import Hat.Model.Options (optionNumber)
import Hat.Server.Keys (KeyState (..))

-- | Log one message, trimming the log to @message-limit@.
logMessage :: ServerState -> Text -> IO ()
logMessage st msg = do
    now <- getPOSIXTime
    atomically $ do
        limit <- optionNumber "message-limit" <$> readTVar st.options
        modifyTVar' st.messageLog (addMessage limit now msg)

-- | A message shown to a client (tmux's @status_message_set@ and
-- @cmdq_error@).
logShownMessage :: ServerState -> Client -> Text -> IO ()
logShownMessage st c msg = logMessage st (clientName c <> " message: " <> msg)

-- | A command about to run, printed as @printed@ (tmux's
-- @cmdq_add_message@). The server's own configuration runs unlogged.
logCommand :: ServerState -> Maybe Client -> Text -> IO ()
logCommand st mclient printed = do
    phase <- readTVarIO st.startupPhase
    unless (phase == LoadingConfig) $ do
        key <- maybe (pure Nothing) (fmap (.firingKey) . readIORef . (.keyState)) mclient
        logMessage st (commandMessage (clientName <$> mclient) key printed)

-- | How tmux words a command in the log: by the client that ran it, and
-- the key that did, if any.
commandMessage :: Maybe Text -> Maybe Text -> Text -> Text
commandMessage mname mkey printed = case (mname, mkey) of
    (Just n, Just k) -> n <> " key " <> k <> ": " <> printed
    (Just n, Nothing) -> n <> " command: " <> printed
    (Nothing, _) -> "command: " <> printed
