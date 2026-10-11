-- | Resolved options and the scope overlays they resolve from. Which options
-- exist, their types and defaults, and whether hat implements them is the
-- registry's business ("Hat.Model.OptionTable"); this module folds the
-- per-scope overlays ('OptionsDelta') onto those defaults into an 'Options'
-- value that consumers read — through a typed record field for the options
-- hat has long had, or through the typed accessors ('optionFlag' and
-- friends) for any registered option.
module Hat.Model.Options
    ( Options (..)
    , StatusPosition (..)
    , ModeKeys (..)
    , AlertAction (..)
    , alertAllows
    , RemainOnExit (..)
    , BorderLines (..)
    , BorderIndicators (..)
    , WindowSizeMode (..)
    , Keymap
    , defaultOptions
      -- * Overlays
    , OptionsDelta
    , emptyDelta
    , singletonDelta
    , insertDelta
    , deleteDelta
    , deltaMember
    , lookupDelta
    , deltaEntries
    , mergeDeltas
    , applyEntry
    , applyDelta
    , resolveOptions
      -- * Typed reads of any registered option
    , optionValue
    , optionText
    , optionFlag
    , optionNumber
    , optionArray
      -- * As format variables
    , optionFormats
    , deltaFormats
      -- * Odds and ends
    , lookupCommandAlias
    , quoteIfNeeded
    , module Hat.Model.OptionTable
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Model.OptionTable
import Hat.Server.Style (parseStyle)
import Hat.Term.Cell qualified as Cell

data StatusPosition = StatusTop | StatusBottom
    deriving (Eq, Show)

data ModeKeys = KeysVi | KeysEmacs
    deriving (Eq, Show)

-- | @bell-action@: which windows' bells raise the alert hook.
data AlertAction = AlertAny | AlertNone | AlertCurrent | AlertOther
    deriving (Eq, Show)

-- | Whether an alert in a (non-)current window fires its hook under an
-- action setting.
alertAllows :: AlertAction -> Bool -> Bool
alertAllows action isCurrent = case action of
    AlertAny -> True
    AlertNone -> False
    AlertCurrent -> isCurrent
    AlertOther -> not isCurrent

-- | @remain-on-exit@: whether a pane whose program exited stays in the tree
-- as a dead pane — always, only when it failed (nonzero exit or a signal), or
-- until a key is pressed in it.
data RemainOnExit = RemainOff | RemainOn | RemainFailed | RemainKey
    deriving (Eq, Show)

-- | @pane-border-lines@: which box-drawing glyph set draws pane borders.
data BorderLines = BorderSingle | BorderHeavy | BorderDouble | BorderSimple
    deriving (Eq, Show)

-- | @pane-border-indicators@: how the active pane's border is marked.
data BorderIndicators
    = IndicatorsOff | IndicatorsColour | IndicatorsArrows | IndicatorsBoth
    deriving (Eq, Show)

-- | @window-size@: how a window's size follows its clients. See
-- 'Hat.Server.Layout.effectiveWindowSize'.
data WindowSizeMode = SizeLargest | SizeSmallest | SizeManual | SizeLatest
    deriving (Eq, Show)

-- | One scope chain's resolved options. Every field below is decoded from
-- its registry row ('fieldBindings'), so its default is that row's.
data Options = Options
    { prefix          :: Text  -- ^ key name, e.g. \"C-b\", \"C-Space\"
    , baseIndex       :: Int
    , paneBaseIndex   :: Int
    , statusLines     :: Int  -- ^ @status@: status-bar height in rows (0 = off)
    , statusPosition  :: StatusPosition
    , modeKeys        :: ModeKeys
    , historyLimit    :: Int
    , defaultTerminal :: Text
    , wordSeparators  :: Text  -- ^ characters that split @next-word@ etc.
    , statusLeft      :: Text
    , statusLeftLength :: Int
    , statusRight     :: Text
    , statusRightLength :: Int
    , statusInterval  :: Int  -- ^ seconds between status redraws; 0 disables
    , windowStatusFormat :: Text
    , windowStatusCurrentFormat :: Text
    , statusStyle            :: Cell.Style
    , windowStatusStyle      :: Cell.Style
    , windowStatusCurrentStyle :: Cell.Style
    , windowStatusBellStyle  :: Cell.Style
    , paneBorderStyle        :: Cell.Style
    , paneActiveBorderStyle  :: Cell.Style
    , modeStyle              :: Cell.Style  -- ^ copy-mode selection highlight
    , paneBorderLines        :: BorderLines
    , paneBorderIndicators   :: BorderIndicators
    , cursorColour           :: Text  -- ^ tmux colour text; @\"\"@ = terminal default
    , setTitles              :: Bool
    , escapeTime             :: Int   -- ^ ms; 0 is hat's native behavior
    , displayTime            :: Int   -- ^ toast duration, ms
    , displayPanesTime       :: Int   -- ^ pane-number overlay duration, ms
    , displayPanesColour     :: Text  -- ^ tmux colour text
    , displayPanesActiveColour :: Text  -- ^ tmux colour text, active pane
    , focusEvents            :: Bool
    , aggressiveResize       :: Bool
    , monitorActivity        :: Bool
    , monitorBell            :: Bool
    , monitorSilence         :: Int   -- ^ seconds of quiet before alert-silence; 0 disables
    , bellAction             :: AlertAction
    , automaticRename        :: Bool   -- ^ windows track their foreground command
    , automaticRenameFormat  :: Text   -- ^ the name an auto-renamed window takes
    , updateEnvironment      :: [Text]  -- ^ vars refreshed on each attach
    , mainPaneWidth          :: Int     -- ^ main-* layouts' main pane, cells
    , mainPaneHeight         :: Int
    , commandAlias    :: Map ArrayKey Text   -- ^ array of @name=command@ entries; see 'lookupCommandAlias'
    , user            :: Map Text Text  -- ^ @\@foo@ options
    , values          :: Map Text OptionValue
        -- ^ every registered option's resolved value, for the typed
        -- accessors ('optionFlag' …)
    }
    deriving (Eq, Show)

-- | table name -> key name -> command sequence (argv lists)
type Keymap = Map Text (Map Text [[Text]])

-- | The registry's defaults, decoded. The record starts with no field set:
-- every field must be bound by some row in 'fieldBindings', which the
-- @OptionsSpec@ check that forces 'defaultOptions' enforces.
defaultOptions :: Options
defaultOptions = foldl (\o d -> applyEntry d.name d.defaultValue o) unbound
    [ d | d <- optionTable, d.role == PlainOption ]
  where
    unbound = Options
        { prefix = missing "prefix", baseIndex = missing "base-index"
        , paneBaseIndex = missing "pane-base-index"
        , statusLines = missing "status"
        , statusPosition = missing "status-position"
        , modeKeys = missing "mode-keys", historyLimit = missing "history-limit"
        , defaultTerminal = missing "default-terminal"
        , wordSeparators = missing "word-separators"
        , statusLeft = missing "status-left"
        , statusLeftLength = missing "status-left-length"
        , statusRight = missing "status-right"
        , statusRightLength = missing "status-right-length"
        , statusInterval = missing "status-interval"
        , windowStatusFormat = missing "window-status-format"
        , windowStatusCurrentFormat = missing "window-status-current-format"
        , statusStyle = missing "status-style"
        , windowStatusStyle = missing "window-status-style"
        , windowStatusCurrentStyle = missing "window-status-current-style"
        , windowStatusBellStyle = missing "window-status-bell-style"
        , paneBorderStyle = missing "pane-border-style"
        , paneActiveBorderStyle = missing "pane-active-border-style"
        , modeStyle = missing "mode-style"
        , paneBorderLines = missing "pane-border-lines"
        , paneBorderIndicators = missing "pane-border-indicators"
        , cursorColour = missing "cursor-colour"
        , setTitles = missing "set-titles", escapeTime = missing "escape-time"
        , displayTime = missing "display-time"
        , displayPanesTime = missing "display-panes-time"
        , displayPanesColour = missing "display-panes-colour"
        , displayPanesActiveColour = missing "display-panes-active-colour"
        , focusEvents = missing "focus-events"
        , aggressiveResize = missing "aggressive-resize"
        , monitorActivity = missing "monitor-activity"
        , monitorBell = missing "monitor-bell"
        , monitorSilence = missing "monitor-silence"
        , bellAction = missing "bell-action"
        , automaticRename = missing "automatic-rename"
        , automaticRenameFormat = missing "automatic-rename-format"
        , updateEnvironment = missing "update-environment"
        , mainPaneWidth = missing "main-pane-width"
        , mainPaneHeight = missing "main-pane-height"
        , commandAlias = missing "command-alias"
        , user = Map.empty
        , values = Map.empty
        }
    missing n = error ("Hat.Model.Options: no binding decodes " <> n)

-- | The record fields decoded from registry rows. A value reaching a binding
-- has already been validated against its row's type, so decoding is total;
-- the fallbacks only guard a row/binding mismatch.
fieldBindings :: Map Text (OptionValue -> Options -> Options)
fieldBindings = Map.fromList
    [ ("prefix", text (\v o -> o { prefix = v }))
    , ("base-index", int (\v o -> o { baseIndex = v }))
    , ("pane-base-index", int (\v o -> o { paneBaseIndex = v }))
    , ("status", text (\v o -> o { statusLines = statusCount v }))
    , ("status-position", text (\v o -> o
        { statusPosition = if v == "top" then StatusTop else StatusBottom }))
    , ("mode-keys", text (\v o -> o
        { modeKeys = if v == "vi" then KeysVi else KeysEmacs }))
    , ("history-limit", int (\v o -> o { historyLimit = v }))
    , ("default-terminal", text (\v o -> o { defaultTerminal = v }))
    , ("word-separators", text (\v o -> o { wordSeparators = v }))
    , ("status-left", text (\v o -> o { statusLeft = v }))
    , ("status-left-length", int (\v o -> o { statusLeftLength = v }))
    , ("status-right", text (\v o -> o { statusRight = v }))
    , ("status-right-length", int (\v o -> o { statusRightLength = v }))
    , ("status-interval", int (\v o -> o { statusInterval = v }))
    , ("window-status-format", text (\v o -> o { windowStatusFormat = v }))
    , ("window-status-current-format", text (\v o -> o
        { windowStatusCurrentFormat = v }))
    , ("status-style", style (\v o -> o { statusStyle = v }))
    , ("window-status-style", style (\v o -> o { windowStatusStyle = v }))
    , ("window-status-current-style", style (\v o -> o
        { windowStatusCurrentStyle = v }))
    , ("window-status-bell-style", style (\v o -> o
        { windowStatusBellStyle = v }))
    , ("pane-border-style", style (\v o -> o { paneBorderStyle = v }))
    , ("pane-active-border-style", style (\v o -> o
        { paneActiveBorderStyle = v }))
    , ("mode-style", style (\v o -> o { modeStyle = v }))
    , ("pane-border-lines", text (\v o -> o { paneBorderLines = case v of
        "heavy" -> BorderHeavy
        "double" -> BorderDouble
        "simple" -> BorderSimple
        _ -> BorderSingle }))
    , ("pane-border-indicators", text (\v o -> o
        { paneBorderIndicators = case v of
            "off" -> IndicatorsOff
            "arrows" -> IndicatorsArrows
            "both" -> IndicatorsBoth
            _ -> IndicatorsColour }))
    , ("cursor-colour", text (\v o -> o { cursorColour = v }))
    , ("set-titles", flag (\v o -> o { setTitles = v }))
    , ("escape-time", int (\v o -> o { escapeTime = v }))
    , ("display-time", int (\v o -> o { displayTime = v }))
    , ("display-panes-time", int (\v o -> o { displayPanesTime = v }))
    , ("display-panes-colour", text (\v o -> o { displayPanesColour = v }))
    , ("display-panes-active-colour", text (\v o -> o
        { displayPanesActiveColour = v }))
    , ("focus-events", flag (\v o -> o { focusEvents = v }))
    , ("aggressive-resize", flag (\v o -> o { aggressiveResize = v }))
    , ("monitor-activity", flag (\v o -> o { monitorActivity = v }))
    , ("monitor-bell", flag (\v o -> o { monitorBell = v }))
    , ("monitor-silence", int (\v o -> o { monitorSilence = v }))
    , ("bell-action", text (\v o -> o { bellAction = case v of
        "none" -> AlertNone
        "current" -> AlertCurrent
        "other" -> AlertOther
        _ -> AlertAny }))
    , ("automatic-rename", flag (\v o -> o { automaticRename = v }))
    , ("automatic-rename-format", text (\v o -> o
        { automaticRenameFormat = v }))
    , ("update-environment", arr (\v o -> o { updateEnvironment = Map.elems v }))
    , ("main-pane-width", int (\v o -> o { mainPaneWidth = v }))
    , ("main-pane-height", int (\v o -> o { mainPaneHeight = v }))
    , ("command-alias", arr (\v o -> o { commandAlias = v }))
    ]
  where
    text f v = f (scalarOf v)
    int f v = f (decimalOf (scalarOf v))
    flag f v = f (scalarOf v == "on")
    style f v = f (parseStyle (scalarOf v))
    arr f = \case
        OVArray m -> f m
        OVScalar _ -> f Map.empty
    statusCount = \case
        "off" -> 0
        "on" -> 1
        n -> decimalOf n

scalarOf :: OptionValue -> Text
scalarOf = \case
    OVScalar t -> t
    OVArray m -> T.unwords (Map.elems m)

decimalOf :: Text -> Int
decimalOf t = case TR.signed TR.decimal t of
    Right (n, "") -> n
    _ -> 0

-- | The options a @set@ at one scope carries: only the entries it actually
-- set (keyed by option name, @\@user@ names included), so lower scopes show
-- through on resolution. See 'resolveOptions'.
newtype OptionsDelta = OptionsDelta (Map Text OptionValue)
    deriving (Eq, Show)

emptyDelta :: OptionsDelta
emptyDelta = OptionsDelta Map.empty

singletonDelta :: Text -> OptionValue -> OptionsDelta
singletonDelta name val = OptionsDelta (Map.singleton name val)

insertDelta :: Text -> OptionValue -> OptionsDelta -> OptionsDelta
insertDelta name val (OptionsDelta m) = OptionsDelta (Map.insert name val m)

-- | Remove a scope's own entry (@set -u@), so resolution falls through to
-- the parent scopes (or the registry default at the top).
deleteDelta :: Text -> OptionsDelta -> OptionsDelta
deleteDelta name (OptionsDelta m) = OptionsDelta (Map.delete name m)

-- | Whether an option was set in this delta (used to tell a user's set apart
-- from a color-scheme default). See 'Hat.Server.ColorScheme.applyPalette'.
deltaMember :: Text -> OptionsDelta -> Bool
deltaMember name (OptionsDelta m) = Map.member name m

-- | A scope's own entry for one option, if any — never the inherited value.
lookupDelta :: Text -> OptionsDelta -> Maybe OptionValue
lookupDelta name (OptionsDelta m) = Map.lookup name m

-- | Everything set at one scope, in name order.
deltaEntries :: OptionsDelta -> [(Text, OptionValue)]
deltaEntries (OptionsDelta m) = Map.toAscList m

-- | Collapse overlapping deltas into one, most-specific first: when two deltas
-- set the same option the earlier (more specific) one wins.
mergeDeltas :: [OptionsDelta] -> OptionsDelta
mergeDeltas = OptionsDelta . Map.unions . map (\(OptionsDelta m) -> m)

-- | Fold one validated option entry into a resolved 'Options'.
applyEntry :: Text -> OptionValue -> Options -> Options
applyEntry name val o
    | "@" `T.isPrefixOf` name = o { user = Map.insert name (scalarOf val) o.user }
    | otherwise = bind val o { values = Map.insert name val o.values }
  where
    bind = Map.findWithDefault (\_ x -> x) name fieldBindings

applyDelta :: OptionsDelta -> Options -> Options
applyDelta (OptionsDelta m) o = Map.foldrWithKey applyEntry o m

-- | Resolve a scope chain (most-specific first) down onto the defaults, so a
-- window's set shadows its session's, which shadows the global.
resolveOptions :: [OptionsDelta] -> Options
resolveOptions deltas = applyDelta (mergeDeltas deltas) defaultOptions

-- | A registered option's resolved value. Calling it with an unregistered
-- name is a programming error, not a user-facing condition.
optionValue :: Text -> Options -> OptionValue
optionValue n o = case Map.lookup n o.values of
    Just v -> v
    Nothing -> error ("Hat.Model.Options: unregistered option " <> T.unpack n)

-- | A scalar option's canonical text: a string, style, colour, key, or
-- choice name.
optionText :: Text -> Options -> Text
optionText n = scalarOf . optionValue n

optionFlag :: Text -> Options -> Bool
optionFlag n = (== "on") . optionText n

optionNumber :: Text -> Options -> Int
optionNumber n = decimalOf . optionText n

-- | An array option's entries in key order.
optionArray :: Text -> Options -> [Text]
optionArray n o = case optionValue n o of
    OVArray m -> Map.elems m
    OVScalar t -> [t]

-- | Every live option and user option as a format variable (tmux's
-- @format_find@ reads options before any other variable): flags as @1@\/@0@,
-- arrays as their entries joined by spaces.
optionFormats :: Options -> Map Text Text
optionFormats o = Map.union o.user $ Map.fromList
    [ (d.name, formatText d.name v)
    | d <- optionTable, isLive d, d.role == PlainOption
    , Just v <- [Map.lookup d.name o.values] ]

-- | One scope's own entries as format variables, rendered as 'optionFormats'
-- renders them.
deltaFormats :: OptionsDelta -> Map Text Text
deltaFormats d = Map.fromList
    [ (n, formatText n v)
    | (n, v) <- deltaEntries d
    , "@" `T.isPrefixOf` n || maybe False isLive (lookupOptionDef n) ]

-- | tmux's numeric @options_to_string@.
formatText :: Text -> OptionValue -> Text
formatText n v = case ((.kind) <$> lookupOptionDef n, v) of
    (Just KFlag, OVScalar t) -> if t == "on" then "1" else "0"
    _ -> valueText v

-- | The command a name aliases, if any: the first @name=command@ entry in
-- key order, as tmux's @cmd_get_alias@ resolves it. See
-- 'Hat.Server.Dispatch.runArgv' for when aliases apply.
lookupCommandAlias :: Options -> Text -> Maybe Text
lookupCommandAlias o wanted = listToMaybe
    [ T.drop 1 rhs
    | entry <- Map.elems o.commandAlias
    , let (nm, rhs) = T.breakOn "=" entry
    , not (T.null rhs), nm == wanted ]

-- | Double-quote a value when it would not survive re-parsing bare.
quoteIfNeeded :: Text -> Text
quoteIfNeeded t
    | T.null t || T.any needs t =
        "\"" <> T.concatMap escape t <> "\""
    | otherwise = t
  where
    needs c = c `elem` (" \t;#\"'\\$" :: String)
    escape c = case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        _ -> T.singleton c
