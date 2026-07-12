-- | Server options and the key-binding tables. Every option hat accepts
-- also has behavior somewhere; unknown options are rejected at 'setOption'
-- rather than silently stored, so a config never looks supported when it
-- isn't.
module Hat.Model.Options
    ( Options (..)
    , StatusPosition (..)
    , ModeKeys (..)
    , BorderLines (..)
    , BorderIndicators (..)
    , Keymap
    , defaultOptions
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import qualified Hat.Term.Cell as Cell

data StatusPosition = StatusTop | StatusBottom
    deriving (Eq, Show)

data ModeKeys = KeysVi | KeysEmacs
    deriving (Eq, Show)

-- | @pane-border-lines@: which box-drawing glyph set draws pane borders.
data BorderLines = BorderSingle | BorderHeavy | BorderDouble | BorderSimple
    deriving (Eq, Show)

-- | @pane-border-indicators@: how the active pane's border is marked.
data BorderIndicators
    = IndicatorsOff | IndicatorsColour | IndicatorsArrows | IndicatorsBoth
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
    , statusStyle            :: Cell.Style
    , windowStatusStyle      :: Cell.Style
    , windowStatusCurrentStyle :: Cell.Style
    , windowStatusBellStyle  :: Cell.Style
    , paneBorderStyle        :: Cell.Style
    , paneActiveBorderStyle  :: Cell.Style
    , paneBorderLines        :: BorderLines
    , paneBorderIndicators   :: BorderIndicators
    , setTitles              :: Bool
    , escapeTime             :: Int   -- ^ ms; 0 is hat's native behavior
    , displayTime            :: Int   -- ^ toast duration, ms
    , focusEvents            :: Bool
    , aggressiveResize       :: Bool
    , monitorActivity        :: Bool
    , automaticRename        :: Bool   -- ^ windows track their foreground command
    , automaticRenameFormat  :: Text   -- ^ the name an auto-renamed window takes
    , updateEnvironment      :: [Text]  -- ^ vars refreshed on each attach
    , mainPaneWidth          :: Int     -- ^ main-* layouts' main pane, cells
    , mainPaneHeight         :: Int
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
    , statusStyle = barStyle
    , windowStatusStyle = barStyle
    , windowStatusCurrentStyle = barStyle
    , windowStatusBellStyle = barStyle
    , paneBorderStyle = Cell.defaultStyle
    , paneActiveBorderStyle = Cell.defaultStyle { Cell.fg = Cell.Indexed 2 }
    , paneBorderLines = BorderSingle
    , paneBorderIndicators = IndicatorsColour
    , setTitles = False
    , escapeTime = 0
    , displayTime = 3000
    , focusEvents = False
    , aggressiveResize = False
    , monitorActivity = False
    , automaticRename = True
    , automaticRenameFormat = "#{pane_current_command}"
    , updateEnvironment =
        [ "DISPLAY", "KRB5CCNAME", "SSH_ASKPASS", "SSH_AUTH_SOCK"
        , "SSH_AGENT_PID", "SSH_CONNECTION", "WINDOWID", "XAUTHORITY" ]
    , mainPaneWidth = 80
    , mainPaneHeight = 24
    , user = Map.empty
    }
  where
    -- The default status bar: black on green.
    barStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 2 }
