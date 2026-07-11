-- | Server options and the key-binding tables. Options the engine
-- doesn't know yet are preserved in 'raw' so user configs keep working
-- as features land.
module Hat.Model.Options
    ( Options (..)
    , StatusPosition (..)
    , ModeKeys (..)
    , Keymap
    , defaultOptions
    , insertRawOption
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
    , user            :: Map Text Text  -- ^ @\@foo@ options
    , raw             :: Map Text Text  -- ^ everything else, preserved
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
    , user = Map.empty
    , raw = Map.empty
    }

-- | Record an unknown option in 'raw'. Lives here (rather than inline at
-- the call site) so the update is unambiguous: 'raw' collides with other
-- record fields of the same name once several models are in scope.
insertRawOption :: Text -> Text -> Options -> Options
insertRawOption name value opts =
    opts { raw = Map.insert name value opts.raw }
