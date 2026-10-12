-- | Configuration errors (tmux's @cfg_causes@): what went wrong loading a
-- file of commands, kept until there is someone to show it to.
module Hat.Server.ConfigCauses
    ( addConfigCause
    , showConfigCauses
    , printConfigCauses
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

import Hat.Model
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.ControlQueue (controlNotify)
import Hat.Server.ViewMode (ViewText (..), viewModeAdd)

addConfigCause :: ServerState -> Text -> STM ()
addConfigCause st cause = modifyTVar' st.configCauses (<> [cause])

takeCauses :: ServerState -> STM [Text]
takeCauses st = swapTVar st.configCauses []

-- | tmux's @cfg_show_causes@, once configuration has loaded and when a
-- session is created or attached: the causes go to the first client when it
-- is in control mode, else to the view mode of @msess@'s current pane (by
-- default the first client's session, else the first session by name) —
-- once that session has a client to see them.
showConfigCauses :: ServerState -> Maybe Session -> IO ()
showConfigCauses st msess = do
    target <- atomically $ do
        pending <- readTVar st.configCauses
        if null pending then pure Nothing else do
            cs <- readTVar st.clients
            sessions <- readTVar st.sessions
            case Map.lookupMin cs of
                Just (_, c) | Just ctl <- c.control -> do
                    causes <- takeCauses st
                    forM_ causes $ \cause ->
                        controlNotify ctl (TE.encodeUtf8 ("%config-error " <> cause))
                    pure Nothing
                mfirst -> do
                    firstSess <- case mfirst of
                        Just (_, c) -> (`Map.lookup` sessions) <$> readTVar c.session
                        Nothing -> pure Nothing
                    byName <- mapM (\s -> (,s) <$> readTVar s.name) (Map.elems sessions)
                    let mchosen = case (msess, firstSess) of
                            (Just s, _) -> Just s
                            (Nothing, Just s) -> Just s
                            _ -> snd <$> listToMaybe (List.sortOn fst byName)
                    case mchosen of
                        Nothing -> pure Nothing
                        Just s -> do
                            seen <- not . null <$> sessionClients st s.id
                            if not seen then pure Nothing else do
                                mpane <- currentWindow s >>= maybe (pure Nothing) activePane
                                case mpane of
                                    Nothing -> pure Nothing
                                    Just p -> Just . (p,) <$> takeCauses st
    forM_ target $ \(p, causes) ->
        unless (null causes) (viewModeAdd st p ViewLiteral causes)

-- | tmux's @cfg_print_causes@, at the end of a @source-file@: every cause,
-- printed to the client that ran it (to a control-mode client as
-- @%config-error@ lines).
printConfigCauses :: ServerState -> Maybe Client -> IO [Reply]
printConfigCauses st mclient = do
    causes <- atomically (takeCauses st)
    pure $ case mclient of
        Nothing -> []
        Just c
            | isJust c.control -> [ ROutput ("%config-error " <> cause) | cause <- causes ]
            | otherwise -> map ROutput causes
