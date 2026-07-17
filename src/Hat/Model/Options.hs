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
    , OptionName (..)
    , OptionValue (..)
    , OptionsDelta
    , ScopeClass (..)
    , emptyDelta
    , singletonDelta
    , insertDelta
    , deltaMember
    , mergeDeltas
    , applyEntry
    , applyDelta
    , resolveOptions
    , optionScopeClass
    , validateScope
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

-- | The closed set of options hat implements, plus @\@foo@ user options.
data OptionName
    = OptPrefix
    | OptBaseIndex
    | OptPaneBaseIndex
    | OptStatusPosition
    | OptModeKeys
    | OptHistoryLimit
    | OptDefaultTerminal
    | OptWordSeparators
    | OptStatusLeft
    | OptStatusLeftLength
    | OptStatusRight
    | OptStatusRightLength
    | OptWindowStatusFormat
    | OptWindowStatusCurrentFormat
    | OptStatusStyle
    | OptWindowStatusStyle
    | OptWindowStatusCurrentStyle
    | OptWindowStatusBellStyle
    | OptPaneBorderStyle
    | OptPaneActiveBorderStyle
    | OptPaneBorderLines
    | OptPaneBorderIndicators
    | OptSetTitles
    | OptEscapeTime
    | OptDisplayTime
    | OptFocusEvents
    | OptAggressiveResize
    | OptMonitorActivity
    | OptAutomaticRename
    | OptAutomaticRenameFormat
    | OptUpdateEnvironment
    | OptMainPaneWidth
    | OptMainPaneHeight
    | OptUser Text   -- ^ the option name, including its @\@@ prefix
    deriving (Eq, Ord, Show)

-- | A parsed, validated option value. Each 'OptionName' pairs with exactly one
-- constructor here; the pairing is maintained by the parser and consumed by
-- 'applyEntry'.
data OptionValue
    = OVText Text
    | OVInt Int
    | OVBool Bool
    | OVTextList [Text]
    | OVStatusPosition StatusPosition
    | OVModeKeys ModeKeys
    | OVStyle Cell.Style
    | OVBorderLines BorderLines
    | OVBorderIndicators BorderIndicators
    deriving (Eq, Show)

-- | The options a @set@ at one scope carries: only the fields it actually set,
-- so lower scopes show through on resolution. See 'resolveOptions'.
newtype OptionsDelta = OptionsDelta (Map OptionName OptionValue)
    deriving (Eq, Show)

-- | Which scope level an option lives at (tmux's server/session/window split).
-- See 'optionScopeClass' and 'validateScope'.
data ScopeClass = ServerOption | SessionOption | WindowOption
    deriving (Eq, Show)

emptyDelta :: OptionsDelta
emptyDelta = OptionsDelta Map.empty

singletonDelta :: OptionName -> OptionValue -> OptionsDelta
singletonDelta name val = OptionsDelta (Map.singleton name val)

insertDelta :: OptionName -> OptionValue -> OptionsDelta -> OptionsDelta
insertDelta name val (OptionsDelta m) = OptionsDelta (Map.insert name val m)

-- | Whether an option was set in this delta (used to tell a user's set apart
-- from a color-scheme default). See 'Hat.Server.ColorScheme.applyPalette'.
deltaMember :: OptionName -> OptionsDelta -> Bool
deltaMember name (OptionsDelta m) = Map.member name m

-- | Collapse overlapping deltas into one, most-specific first: when two deltas
-- set the same option the earlier (more specific) one wins.
mergeDeltas :: [OptionsDelta] -> OptionsDelta
mergeDeltas = OptionsDelta . Map.unions . map (\(OptionsDelta m) -> m)

-- | Fold one validated option entry into a resolved 'Options'.
applyEntry :: OptionName -> OptionValue -> Options -> Options
applyEntry name val o = case (name, val) of
    (OptPrefix, OVText t) -> o { prefix = t }
    (OptBaseIndex, OVInt n) -> o { baseIndex = n }
    (OptPaneBaseIndex, OVInt n) -> o { paneBaseIndex = n }
    (OptStatusPosition, OVStatusPosition p) -> o { statusPosition = p }
    (OptModeKeys, OVModeKeys k) -> o { modeKeys = k }
    (OptHistoryLimit, OVInt n) -> o { historyLimit = n }
    (OptDefaultTerminal, OVText t) -> o { defaultTerminal = t }
    (OptWordSeparators, OVText t) -> o { wordSeparators = t }
    (OptStatusLeft, OVText t) -> o { statusLeft = t }
    (OptStatusLeftLength, OVInt n) -> o { statusLeftLength = n }
    (OptStatusRight, OVText t) -> o { statusRight = t }
    (OptStatusRightLength, OVInt n) -> o { statusRightLength = n }
    (OptWindowStatusFormat, OVText t) -> o { windowStatusFormat = t }
    (OptWindowStatusCurrentFormat, OVText t) ->
        o { windowStatusCurrentFormat = t }
    (OptStatusStyle, OVStyle s) -> o { statusStyle = s }
    (OptWindowStatusStyle, OVStyle s) -> o { windowStatusStyle = s }
    (OptWindowStatusCurrentStyle, OVStyle s) ->
        o { windowStatusCurrentStyle = s }
    (OptWindowStatusBellStyle, OVStyle s) -> o { windowStatusBellStyle = s }
    (OptPaneBorderStyle, OVStyle s) -> o { paneBorderStyle = s }
    (OptPaneActiveBorderStyle, OVStyle s) -> o { paneActiveBorderStyle = s }
    (OptPaneBorderLines, OVBorderLines l) -> o { paneBorderLines = l }
    (OptPaneBorderIndicators, OVBorderIndicators i) ->
        o { paneBorderIndicators = i }
    (OptSetTitles, OVBool b) -> o { setTitles = b }
    (OptEscapeTime, OVInt n) -> o { escapeTime = n }
    (OptDisplayTime, OVInt n) -> o { displayTime = n }
    (OptFocusEvents, OVBool b) -> o { focusEvents = b }
    (OptAggressiveResize, OVBool b) -> o { aggressiveResize = b }
    (OptMonitorActivity, OVBool b) -> o { monitorActivity = b }
    (OptAutomaticRename, OVBool b) -> o { automaticRename = b }
    (OptAutomaticRenameFormat, OVText t) -> o { automaticRenameFormat = t }
    (OptUpdateEnvironment, OVTextList ts) -> o { updateEnvironment = ts }
    (OptMainPaneWidth, OVInt n) -> o { mainPaneWidth = n }
    (OptMainPaneHeight, OVInt n) -> o { mainPaneHeight = n }
    (OptUser k, OVText t) -> o { user = Map.insert k t o.user }
    -- Unreachable: the parser never pairs a name with a foreign value.
    _ -> o

applyDelta :: OptionsDelta -> Options -> Options
applyDelta (OptionsDelta m) o = Map.foldrWithKey applyEntry o m

-- | Resolve a scope chain (most-specific first) down onto the defaults, so a
-- window's set shadows its session's, which shadows the global.
resolveOptions :: [OptionsDelta] -> Options
resolveOptions deltas = applyDelta (mergeDeltas deltas) defaultOptions

-- | The scope level an option belongs to, mirroring tmux's options table.
optionScopeClass :: OptionName -> ScopeClass
optionScopeClass = \case
    OptDefaultTerminal -> ServerOption
    OptEscapeTime -> ServerOption
    OptModeKeys -> WindowOption
    OptWordSeparators -> WindowOption
    OptAggressiveResize -> WindowOption
    OptMonitorActivity -> WindowOption
    OptAutomaticRename -> WindowOption
    OptAutomaticRenameFormat -> WindowOption
    OptMainPaneWidth -> WindowOption
    OptMainPaneHeight -> WindowOption
    OptWindowStatusFormat -> WindowOption
    OptWindowStatusCurrentFormat -> WindowOption
    OptWindowStatusStyle -> WindowOption
    OptWindowStatusCurrentStyle -> WindowOption
    OptWindowStatusBellStyle -> WindowOption
    OptPaneBorderStyle -> WindowOption
    OptPaneActiveBorderStyle -> WindowOption
    OptPaneBorderLines -> WindowOption
    OptPaneBorderIndicators -> WindowOption
    OptUser _ -> SessionOption
    _ -> SessionOption

-- | Reject setting an option at a scope its class forbids (e.g. @setw prefix@),
-- so a mis-scoped set fails loud rather than landing in the wrong table.
validateScope :: ScopeClass -> OptionName -> Either Text ()
validateScope requested name
    | optionScopeClass name == requested = Right ()
    | otherwise = Left (scopeError requested name)

scopeError :: ScopeClass -> OptionName -> Text
scopeError requested name =
    optionNameText name <> " is a " <> className (optionScopeClass name)
        <> " option, not a " <> className requested <> " option"
  where
    className = \case
        ServerOption -> "server"
        SessionOption -> "session"
        WindowOption -> "window"

optionNameText :: OptionName -> Text
optionNameText = \case
    OptPrefix -> "prefix"
    OptBaseIndex -> "base-index"
    OptPaneBaseIndex -> "pane-base-index"
    OptStatusPosition -> "status-position"
    OptModeKeys -> "mode-keys"
    OptHistoryLimit -> "history-limit"
    OptDefaultTerminal -> "default-terminal"
    OptWordSeparators -> "word-separators"
    OptStatusLeft -> "status-left"
    OptStatusLeftLength -> "status-left-length"
    OptStatusRight -> "status-right"
    OptStatusRightLength -> "status-right-length"
    OptWindowStatusFormat -> "window-status-format"
    OptWindowStatusCurrentFormat -> "window-status-current-format"
    OptStatusStyle -> "status-style"
    OptWindowStatusStyle -> "window-status-style"
    OptWindowStatusCurrentStyle -> "window-status-current-style"
    OptWindowStatusBellStyle -> "window-status-bell-style"
    OptPaneBorderStyle -> "pane-border-style"
    OptPaneActiveBorderStyle -> "pane-active-border-style"
    OptPaneBorderLines -> "pane-border-lines"
    OptPaneBorderIndicators -> "pane-border-indicators"
    OptSetTitles -> "set-titles"
    OptEscapeTime -> "escape-time"
    OptDisplayTime -> "display-time"
    OptFocusEvents -> "focus-events"
    OptAggressiveResize -> "aggressive-resize"
    OptMonitorActivity -> "monitor-activity"
    OptAutomaticRename -> "automatic-rename"
    OptAutomaticRenameFormat -> "automatic-rename-format"
    OptUpdateEnvironment -> "update-environment"
    OptMainPaneWidth -> "main-pane-width"
    OptMainPaneHeight -> "main-pane-height"
    OptUser k -> k
