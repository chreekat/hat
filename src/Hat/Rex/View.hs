-- | A Rex session's view: its windows and the blocks in them, as
-- @session.attach@ and @session.view@ report it.
module Hat.Rex.View
    ( SessionId (..)
    , WindowId (..)
    , BlockId (..)
    , View (..)
    , Window (..)
    , Block (..)
    , parseView
    , terminalCreator
    ) where

import Data.Aeson (FromJSON, ToJSON, Value, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

newtype SessionId = SessionId Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON, ToJSON)

newtype WindowId = WindowId Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON, ToJSON)

newtype BlockId = BlockId Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON, ToJSON)

data View = View
    { revision :: Int  -- ^ larger is newer, within one session
    , sessionId :: SessionId
    , label :: Text
    , activeWindow :: Maybe WindowId  -- ^ 'Nothing' for a session with no windows
    , windows :: [Window]
    }
    deriving (Eq, Show)

data Window = Window
    { windowId :: WindowId
    , label :: Text
    , focusedBlock :: Maybe BlockId
    , blocks :: [Block]  -- ^ of every layer, back to front
    }
    deriving (Eq, Show)

data Block = Block
    { blockId :: BlockId
    , creatorName :: Maybe Text
    }
    deriving (Eq, Show)

-- | The creator whose blocks are terminals.
terminalCreator :: Text
terminalCreator = "com.superlogical.terminal"

-- | Read a view. Fields hat does not act on are not looked at; an absent
-- or null list is empty.
parseView :: Value -> Parser View
parseView = withObject "view" $ \o -> do
    revision <- o .: "revision"
    sessionId <- o .: "session_id"
    label <- fromMaybe "" <$> o .:? "label"
    activeWindow <- o .:? "active_window_id"
    windows <- mapM parseWindow . fromMaybe [] =<< o .:? "windows"
    pure View { revision, sessionId, label, activeWindow, windows }

parseWindow :: Value -> Parser Window
parseWindow = withObject "window" $ \o -> do
    windowId <- o .: "window_id"
    label <- fromMaybe "" <$> o .:? "label"
    focusedBlock <- o .:? "focused_block_id"
    layers <- fromMaybe [] <$> o .: "layers"
    blocks <- concat <$> mapM parseLayerBlocks layers
    pure Window { windowId, label, focusedBlock, blocks }

parseLayerBlocks :: Value -> Parser [Block]
parseLayerBlocks = withObject "layer" $ \o ->
    mapM parseBlock . fromMaybe [] =<< o .: "blocks"

parseBlock :: Value -> Parser Block
parseBlock = withObject "block" $ \o -> do
    blockId <- o .: "block_id"
    creatorName <- o .:? "creator_name"
    pure Block { blockId, creatorName }
