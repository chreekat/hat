-- | Server options and the key-binding tables. Every option hat accepts
-- also has behavior somewhere; unknown options are rejected at 'setOption'
-- rather than silently stored, so a config never looks supported when it
-- isn't.
module Hat.Model.Options
    ( Options (..)
    , StatusPosition (..)
    , ModeKeys (..)
    , Keymap
    , defaultOptions
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data StatusPosition = StatusTop | StatusBottom
    deriving (Eq, Show)

data ModeKeys = KeysVi | KeysEmacs
    deriving (Eq, Show)

data Options = Options
    { prefix          :: Text  -- ^ key name, e.g. \"C-b\", \"C-Space\"
    , baseIndex       :: Int
    , paneBaseIndex   :: Int
    , statusPosition  :: StatusPosition
    , modeKeys        :: ModeKeys
    , historyLimit    :: Int
    , defaultTerminal :: Text
    , wordSeparators  :: Text  -- ^ characters that split @next-word@ etc.
    , statusLeft      :: Text
    , statusLeftLength :: Int
    , statusRight     :: Text
    , statusRightLength :: Int
    , windowStatusFormat :: Text
    , windowStatusCurrentFormat :: Text
    , user            :: Map Text Text  -- ^ @\@foo@ options
    }
    deriving (Eq, Show)

-- | table name -> key name -> command sequence (argv lists)
type Keymap = Map Text (Map Text [[Text]])

defaultOptions :: Options
defaultOptions = Options
    { prefix = "C-b"
    , baseIndex = 0
    , paneBaseIndex = 0
    , statusPosition = StatusBottom
    , modeKeys = KeysEmacs
    , historyLimit = 50000
    , defaultTerminal = "tmux-256color"
    , wordSeparators = "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~"
    , statusLeft = "[#S] "
    , statusLeftLength = 10
    , statusRight = "%H:%M %d-%b-%y #H"
    , statusRightLength = 40
    , windowStatusFormat = "#I:#W#F"
    , windowStatusCurrentFormat = "#I:#W#F"
    , user = Map.empty
    }
