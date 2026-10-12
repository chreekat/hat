-- | @show-messages@ (tmux's @cmd-show-messages.c@): the server's message log.
module Hat.Server.Command.ShowMessages
    ( cmdShowMessages
    ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)

import Hat.Model
import Hat.Model.MessageLog (MessageEntry (..), MessageLog (..))
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, unknownFlag)
import Hat.Server.FormatEnv (clientContextFormatEnv, expandFormat)

-- | tmux's @SHOW_MESSAGES_TEMPLATE@.
showMessagesTemplate :: Text
showMessagesTemplate = "#{t/p:message_time}: #{message_text}"

-- | @show-messages [-JT] [-t target-client]@: the log, newest first. hat
-- keeps neither a job list (@-J@) nor terminal descriptions (@-T@). @-t@
-- only narrows @-T@.
cmdShowMessages :: CommandImpl
cmdShowMessages st mclient args
    | Just e <- unknownFlag "show-messages" "JT" flags = pure [RErr e]
    | not (null pos) =
        pure [RErr "command show-messages: too many arguments (need at most 0)"]
    | "-T" `elem` flags = pure [RErr "show-messages -T: not supported"]
    | "-J" `elem` flags = pure [RErr "show-messages -J: not supported"]
    | otherwise = do
        base <- clientContextFormatEnv st mclient
        l <- readTVarIO st.messageLog
        forM (toList (Seq.reverse l.entries)) $ \e ->
            ROutput <$> expandFormat st (Map.union (vars e) base) showMessagesTemplate
  where
    (_, flags, pos) = parseArgs "t" args
    vars e = Map.fromList
        [ ("message_text", e.text)
        , ("message_number", tshow e.number)
        , ("message_time", tshow (floor e.time :: Integer)) ]
