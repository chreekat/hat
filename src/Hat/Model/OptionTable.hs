-- | The option registry: one row per tmux option, mirroring tmux's
-- @options-table.c@ (same names, order, scopes, types, bounds and choice
-- lists), plus hat's implementation gate. Everything generic about options
-- — name matching, scope routing, value validation, @set-option@ and
-- @show-options@ — is derived from these rows; see "Hat.Model.Options" for
-- how values resolve and "Hat.Server.Command.Option" for the commands.
--
-- Giving an option behaviour is: flip its row from 'notYet' to 'live' here
-- (narrowing it with 'choicesOnly' \/ 'paneLocal' \/ 'refusing' when hat
-- honours only part of tmux's value space), and read the resolved value in
-- the owning subsystem through the typed accessors of "Hat.Model.Options"
-- ('Hat.Model.Options.optionFlag' and friends). A row left 'notYet' fails
-- loud on @set@ and @show@, so a config never looks supported when it isn't.
module Hat.Model.OptionTable
    ( -- * Rows
      OptionDef (..)
    , OptionScope (..)
    , OptionKind (..)
    , Gate (..)
    , LiveSupport (..)
    , PaneValues (..)
    , OptionRole (..)
    , optionTable
    , lookupOptionDef
    , isLive
    , notYetError
    , refusing
    , choicesOnly
    , aliasing
    , choiceSpelling
    , cursorStyles
      -- * Values
    , OptionValue (..)
    , ArrayKey (..)
    , parseArrayKey
    , firstFreeIndex
    , arrayKeyText
    , valueText
      -- * Names
    , MatchError (..)
    , matchOptionName
    , splitOptionKey
    , hookNames
    ) where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

-- | Where an option lives (tmux's @OPTIONS_TABLE_*@ scope bits). A
-- 'WindowPaneScope' option may also be set per pane (@set -p@).
data OptionScope = ServerScope | SessionScope | WindowScope | WindowPaneScope
    deriving (Eq, Show)

-- | tmux's option types. Strings carry their @IS_STYLE@ \/ @IS_COLOUR@
-- validation as distinct kinds; bounds are tmux's @minimum@\/@maximum@.
data OptionKind
    = KString
    | KStyle         -- ^ a string validated as a style when set
    | KColourString  -- ^ a string validated as a single colour when set
    | KNumber Integer Integer
    | KKey
    | KColour        -- ^ stored as a colour; shown in canonical spelling
    | KFlag
    | KChoice [Text]
    | KCommand
    deriving (Eq, Show)

-- | How much of an option hat honours.
data Gate
    = NotYet  -- ^ registered so names match as tmux's do, but set/show fail loud
    | Live LiveSupport

-- | The part of a live option's value space hat honours; anything outside
-- it is refused at @set@ rather than stored.
data LiveSupport = LiveSupport
    { onlyChoices :: Maybe [Text]
        -- ^ for a choice option, the values hat implements ('Nothing' = all)
    , paneValues :: PaneValues
    , refuse :: Text -> Maybe Text
        -- ^ a canonical value hat cannot honour yet, with the reason
    , aliases :: [(Text, Text)]
        -- ^ for a choice option, hat's extra spellings of its choices, each
        -- with the choice it names
    }

-- | Whether a window-and-pane option's consumer reads a pane's own value
-- (@set -p@).
data PaneValues = PaneValuesHonoured | PaneValuesIgnored
    deriving (Eq, Show)

-- | Hooks are array options of commands that live in the hook store
-- ("Hat.Server.Hooks"), not in the option overlays.
data OptionRole = PlainOption | HookOption
    deriving (Eq, Show)

data OptionDef = OptionDef
    { name :: Text
    , scope :: OptionScope
    , kind :: OptionKind
    , array :: Maybe Text
        -- ^ for an array option, the separator a whole-array assignment
        -- splits on (tmux's @separator@; @" ,"@ when the row leaves it unset,
        -- @""@ for hooks, which never split)
    , role :: OptionRole
    , pattern :: Maybe Text
        -- ^ an @fnmatch(3)@ glob a string value must match (tmux's
        -- @pattern@)
    , defaultValue :: OptionValue
    , gate :: Gate
    , description :: Maybe Text  -- ^ tmux's @text@: what the option does
    , unit :: Maybe Text         -- ^ what a number counts, as tmux names it
    }

-- | A validated option value. Scalars hold the canonical text @show-options@
-- prints (a number in decimal, a flag as @on@\/@off@, a choice by name, a
-- key or colour in tmux's canonical spelling); the typed accessors in
-- "Hat.Model.Options" decode it. Arrays map keys to their entries.
data OptionValue
    = OVScalar Text
    | OVArray (Map ArrayKey Text)
    deriving (Eq, Show)

-- | An array option's key. tmux keeps numeric keys and string keys, ordering
-- all numeric ones first (ascending) and then strings by @strcmp@ — which is
-- exactly this derived 'Ord'.
data ArrayKey = KeyIndex Integer | KeyName Text
    deriving (Eq, Ord, Show)

-- | tmux's @options_array_correct_key@: all-digit keys are indices
-- (normalised, so @01@ is @1@); anything else is a string key. Empty or
-- out-of-range keys are rejected.
parseArrayKey :: Text -> Maybe ArrayKey
parseArrayKey t
    | T.null t = Nothing
    | T.all isDigit t = case TR.decimal t of
        Right (n, "") | n <= uintMax -> Just (KeyIndex n)
        _ -> Nothing
    | otherwise = Just (KeyName t)

-- | The lowest index an array leaves unused, where tmux places an appended
-- entry.
firstFreeIndex :: Map ArrayKey a -> ArrayKey
firstFreeIndex m = KeyIndex (until (\i -> not (Map.member (KeyIndex i) m)) (+ 1) 0)

arrayKeyText :: ArrayKey -> Text
arrayKeyText = \case
    KeyIndex n -> T.pack (show n)
    KeyName t -> t

-- | A value as one string: a scalar's text, an array's entries joined by
-- spaces (tmux's @options_to_string@ without a key).
valueText :: OptionValue -> Text
valueText = \case
    OVScalar t -> t
    OVArray m -> T.unwords (Map.elems m)

isLive :: OptionDef -> Bool
isLive d = case d.gate of
    Live _ -> True
    NotYet -> False

-- | The loud refusal for a registered option hat has not implemented.
notYetError :: Text -> Text
notYetError n = n <> ": option not yet supported by hat"

lookupOptionDef :: Text -> Maybe OptionDef
lookupOptionDef n = Map.lookup n byName

byName :: Map Text OptionDef
byName = Map.fromList [ (d.name, d) | d <- optionTable ]

-- | Why an option spelling did not resolve; the 'Text' is tmux's message.
data MatchError = MatchError Text
    deriving (Eq, Show)

-- | tmux's @options_parse@: split @name[key]@ into the name and its key.
-- A malformed key (empty, unterminated, trailing text, out of range) makes
-- the whole spelling invalid, as in tmux.
splitOptionKey :: Text -> Maybe (Text, Maybe ArrayKey)
splitOptionKey t
    | T.null t = Nothing
    | otherwise = case T.breakOn "[" t of
        (base, rest)
            | T.null rest -> Just (t, Nothing)
            | Just inner <- T.stripSuffix "]" (T.drop 1 rest)
            , not (T.isInfixOf "]" inner) -> do
                k <- parseArrayKey inner
                Just (base, Just k)
            | otherwise -> Nothing

-- | tmux's @options_match@: resolve a typed option spelling to a registered
-- name (or pass a @\@user@ option through). Historical @-color@ spellings
-- map to @-colour@; an exact name wins; otherwise a unique prefix resolves.
matchOptionName :: Text -> Either MatchError (Text, Maybe ArrayKey)
matchOptionName spelled = case splitOptionKey spelled of
    Nothing -> Left invalid
    Just (base, mkey)
        | "@" `T.isPrefixOf` base -> Right (base, mkey)
        | otherwise ->
            let wanted = maybe base id (lookup base otherNames)
            in case lookupOptionDef wanted of
                Just d -> Right (d.name, mkey)
                Nothing -> case [ d.name | d <- optionTable
                                         , wanted `T.isPrefixOf` d.name ] of
                    [n] -> Right (n, mkey)
                    [] -> Left invalid
                    _ -> Left (MatchError ("ambiguous option: " <> spelled))
  where
    invalid = MatchError ("invalid option: " <> spelled)

-- | tmux's @options_other_names@.
otherNames :: [(Text, Text)]
otherNames =
    [ ("display-panes-color", "display-panes-colour")
    , ("display-panes-active-color", "display-panes-active-colour")
    , ("clock-mode-color", "clock-mode-colour")
    , ("cursor-color", "cursor-colour")
    , ("prompt-cursor-color", "prompt-cursor-colour")
    , ("prompt-command-cursor-color", "prompt-command-cursor-colour")
    , ("pane-colors", "pane-colours")
    ]

-- | The built-in hook names @set-hook@ and @wait-for -E@ accept by name
-- (the per-command @after-*@ hooks are matched against the command table
-- instead; see 'Hat.Server.Hooks.validSetHookName').
hookNames :: [Text]
hookNames =
    [ d.name | d <- optionTable, d.role == HookOption
    , not ("after-" `T.isPrefixOf` d.name) ]

-- Row builders ----------------------------------------------------------

intMax, uintMax, shrtMax, ushrtMax :: Integer
intMax = 2147483647
uintMax = 4294967295
shrtMax = 32767
ushrtMax = 65535

scalar :: Text -> OptionScope -> OptionKind -> Text -> OptionDef
scalar n sc k def = OptionDef
    { name = n, scope = sc, kind = k, array = Nothing, role = PlainOption
    , pattern = Nothing, defaultValue = OVScalar def, gate = NotYet
    , description = Nothing, unit = Nothing }

-- | An array option whose default is the given entries at keys 0, 1, ….
arrayOf :: Text -> OptionScope -> OptionKind -> Text -> [Text] -> OptionDef
arrayOf n sc k sep def = OptionDef
    { name = n, scope = sc, kind = k, array = Just sep, role = PlainOption
    , pattern = Nothing
    , defaultValue = OVArray (Map.fromList (zip (map KeyIndex [0 ..]) def))
    , gate = NotYet, description = Nothing, unit = Nothing }

-- | A hook; an @after-@ hook is described as tmux's
-- @OPTIONS_TABLE_AFTER_HOOK@ describes it.
hook :: Text -> OptionScope -> OptionDef
hook n sc = (arrayOf n sc KCommand "" [])
    { role = HookOption, gate = Live fullSupport
    , description = afterText <$> T.stripPrefix "after-" n }
  where
    afterText cmd = "Run after the " <> cmd <> " command completes."

-- | Give a row tmux's description.
described :: OptionDef -> Text -> OptionDef
described d t = d { description = Just t }

-- | Give a number row the unit tmux shows beside its value.
measuredIn :: OptionDef -> Text -> OptionDef
measuredIn d u = d { unit = Just u }

-- | Leave a row registered but unimplemented.
notYet :: OptionDef -> OptionDef
notYet d = d { gate = NotYet }

-- | Mark a row implemented over tmux's whole value space.
live :: OptionDef -> OptionDef
live d = d { gate = Live fullSupport }

fullSupport :: LiveSupport
fullSupport = LiveSupport
    { onlyChoices = Nothing, paneValues = PaneValuesIgnored, refuse = const Nothing
    , aliases = [] }

-- | Narrow a live choice option to the values hat implements.
choicesOnly :: [Text] -> OptionDef -> OptionDef
choicesOnly cs = adjustSupport (\s -> s { onlyChoices = Just cs })

-- | Accept extra spellings of a live choice option's choices, beyond
-- tmux's.
aliasing :: [(Text, Text)] -> OptionDef -> OptionDef
aliasing as = adjustSupport (\s -> s { aliases = as })

-- | The choice a spelling names: an alias's choice, else the spelling.
choiceSpelling :: OptionDef -> Text -> Text
choiceSpelling d v = case d.gate of
    Live s | Just c <- lookup v s.aliases -> c
    _ -> v

-- | A live window-and-pane option whose consumer honours a pane's own value.
paneLocal :: OptionDef -> OptionDef
paneLocal = adjustSupport (\s -> s { paneValues = PaneValuesHonoured })

-- | Refuse the canonical values hat cannot honour yet.
refusing :: (Text -> Maybe Text) -> OptionDef -> OptionDef
refusing f = adjustSupport (\s -> s { refuse = f })

adjustSupport :: (LiveSupport -> LiveSupport) -> OptionDef -> OptionDef
adjustSupport f d = case d.gate of
    Live s -> d { gate = Live (f s) }
    NotYet -> d { gate = Live (f fullSupport) }

cursorStyles, popupLines, bellActions, visualBell :: [Text]
cursorStyles =
    [ "default", "blinking-block", "block", "blinking-underline"
    , "underline", "blinking-bar", "bar" ]
popupLines =
    ["single", "double", "heavy", "simple", "rounded", "padded", "none"]
bellActions = ["none", "any", "current", "other"]
visualBell = ["off", "on", "both"]

-- | tmux's @status-format@ defaults (@OPTIONS_TABLE_STATUS_FORMAT1..3@):
-- the window list, the pane list and the session list.
statusFormat1, statusFormat2, statusFormat3 :: Text
statusFormat1 = T.concat
    [ "#[align=left range=left #{E:status-left-style}]"
    , "#[push-default]"
    , "#{T;=/#{status-left-length}:status-left}"
    , "#[pop-default]"
    , "#[norange default]"
    , "#[list=on align=#{status-justify}]"
    , "#[list=left-marker]<#[list=right-marker]>#[list=on]"
    , "#{W:"
    ,     "#[range=window|#{window_index} "
    ,         "#{E:window-status-style}"
    ,         "#{?#{&&:#{window_last_flag},"
    ,             "#{!=:#{E:window-status-last-style},default}}, "
    ,             "#{E:window-status-last-style},"
    ,         "}"
    ,         "#{?#{&&:#{window_bell_flag},"
    ,             "#{!=:#{E:window-status-bell-style},default}}, "
    ,             "#{E:window-status-bell-style},"
    ,             "#{?#{&&:#{||:#{window_activity_flag},"
    ,                          "#{window_silence_flag}},"
    ,                 "#{!=:"
    ,                 "#{E:window-status-activity-style},"
    ,                 "default}}, "
    ,                 "#{E:window-status-activity-style},"
    ,             "}"
    ,         "}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#{T:window-status-format}"
    ,     "#[pop-default]"
    ,     "#[norange default]"
    ,     "#{?loop_last_flag,,#{E:window-status-separator}}"
    , ","
    ,     "#[range=window|#{window_index} list=focus "
    ,         "#{?#{!=:#{E:window-status-current-style},default},"
    ,             "#{E:window-status-current-style},"
    ,             "#{E:window-status-style}"
    ,         "}"
    ,         "#{?#{&&:#{window_last_flag},"
    ,             "#{!=:#{E:window-status-last-style},default}}, "
    ,             "#{E:window-status-last-style},"
    ,         "}"
    ,         "#{?#{&&:#{window_bell_flag},"
    ,             "#{!=:#{E:window-status-bell-style},default}}, "
    ,             "#{E:window-status-bell-style},"
    ,             "#{?#{&&:#{||:#{window_activity_flag},"
    ,                          "#{window_silence_flag}},"
    ,                 "#{!=:"
    ,                 "#{E:window-status-activity-style},"
    ,                 "default}}, "
    ,                 "#{E:window-status-activity-style},"
    ,             "}"
    ,         "}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#{T:window-status-current-format}"
    ,     "#[pop-default]"
    ,     "#[norange list=on default]"
    ,     "#{?loop_last_flag,,#{E:window-status-separator}}"
    , "}"
    , "#[nolist align=right range=right #{E:status-right-style}]"
    , "#[push-default]"
    , "#{T;=/#{status-right-length}:status-right}"
    , "#[pop-default]"
    , "#[norange default]"
    ]
statusFormat2 = T.concat
    [ "#[align=left]#{R: ,#{n:#{session_name}}}P: "
    , "#[norange default]"
    , "#[list=on align=#{status-justify}]"
    , "#[list=left-marker]<#[list=right-marker]>#[list=on]"
    , "#{P:"
    ,     "#[range=pane|#{pane_id} "
    ,         "#{E:pane-status-style}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#{T:window-pane-status-format}"
    ,     "#[pop-default]"
    ,     "#[norange list=on default]  "
    , ","
    ,     "#[range=pane|#{pane_id} list=focus "
    ,         "#{?#{!=:#{E:pane-status-current-style},default},"
    ,             "#{E:pane-status-current-style},"
    ,             "#{E:pane-status-style}"
    ,         "}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#{T:window-pane-current-status-format}"
    ,     "#[pop-default]"
    ,     "#[norange list=on default] "
    , "}"
    ]
statusFormat3 = T.concat
    [ "#[align=left]#{R: ,#{n:#{session_name}}}S: "
    , "#[norange default]"
    , "#[list=on align=#{status-justify}]"
    , "#[list=left-marker]<#[list=right-marker]>#[list=on]"
    , "#{S:"
    ,     "#[range=session|#{session_id} "
    ,         "#{E:session-status-style}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#S#{session_alert}"
    ,     "#[pop-default]"
    ,     "#[norange list=on default]  "
    , ","
    ,     "#[range=session|#{session_id} list=focus "
    ,         "#{?#{!=:#{E:session-status-current-style},default},"
    ,                 "#{E:session-status-current-style},"
    ,                 "#{E:session-status-style}"
    ,         "}"
    ,     "]"
    ,     "#[push-default]"
    ,     "#S*#{session_alert}"
    ,     "#[pop-default]"
    ,     "#[norange list=on default] "
    , "}"
    ]

-- | Every option, in @options-table.c@ order (which is also the order
-- @show-options@ lists them in). Defaults are hat's: for a live option the
-- value hat actually uses, which may differ from tmux's (noted inline).
optionTable :: [OptionDef]
optionTable =
    -- Server options.
    [ live $ scalar "backspace" ServerScope KKey "C-?"
        `described` "The key to send for backspace."
    , live $ scalar "buffer-limit" ServerScope (KNumber 1 intMax) "50"
        `described` "The maximum number of automatic buffers. When this is \
            \reached, the oldest buffer is deleted."
    -- hat's default omits tmux's aliases for commands hat lacks.
    , live $ arrayOf "command-alias" ServerScope KString ","
        [ "split-pane=split-window", "splitp=split-window"
        , "choose-window=choose-tree -w", "choose-session=choose-tree -s" ]
        `described` "Array of command aliases. Each entry is an alias and a \
            \command separated by '='."
    , notYet $ arrayOf "codepoint-widths" ServerScope KString "," []
        `described` "Array of override widths for Unicode codepoints."
    , live $ scalar "copy-command" ServerScope KString ""
        `described` "Shell command run when text is copied. If empty, no \
            \command is run."
    , paneLocal $ live $
        scalar "cursor-colour" WindowPaneScope KColourString ""
        `described` "Colour of the cursor."
    , paneLocal $ live $ scalar "cursor-style" WindowPaneScope (KChoice cursorStyles) "default"
        `described` "Style of the cursor."
    -- tmux defaults to new-session; a bare hat attaches to a session when
    -- there is one.
    , live $ scalar "default-client-command" ServerScope KCommand "new-session -A"
        `described` "Default command to run when tmux is run without a command."
    , live $ scalar "default-terminal" ServerScope KString "tmux-256color"
        `described` "Default for the 'TERM' environment variable."
    , live $ scalar "editor" ServerScope KString "/usr/bin/vi"
        `described` "Editor run to edit files."
    -- tmux defaults to 10; hat's native escape handling needs no wait.
    , live $ scalar "escape-time" ServerScope (KNumber 0 intMax) "0"
        `described` "Time to wait before assuming a key is Escape."
        `measuredIn` "milliseconds"
    , live $ scalar "exit-empty" ServerScope KFlag "on"
        `described` "Whether the server should exit if there are no sessions."
    , live $ scalar "exit-unattached" ServerScope KFlag "off"
        `described` "Whether the server should exit if there are no attached \
            \clients."
    -- tmux defaults to off, ignoring an app's request for extended keys.
    , live $ scalar "extended-keys" ServerScope
        (KChoice ["off", "on", "always"]) "on"
        `described` "Whether to request extended key sequences from terminals \
            \that support it."
    , live $ scalar "extended-keys-format" ServerScope
        (KChoice ["csi-u", "xterm"]) "xterm"
        `described` "The format of emitted extended key sequences."
    , live $ scalar "focus-events" ServerScope KFlag "off"
        `described` "Whether to send focus events to applications."
    , live $ scalar "get-clipboard" ServerScope
        (KChoice ["off", "buffer", "request", "both"]) "buffer"
        `described` "When an application requests the clipboard, whether to \
            \ignore the request ('off'); respond with the newest buffer \
            \('buffer'); request the clipboard from the most recently used \
            \terminal ('request'); or to request the clipboard, create a \
            \buffer, and send it to the application ('both')."
    , live $ scalar "history-file" ServerScope KString ""
        `described` "Location of the command prompt history file. Empty does \
            \not write a history file."
    , live $ scalar "input-buffer-size" ServerScope
        (KNumber 1048576 uintMax) "1048576"
        `described` "Number of bytes accepted in a single input before \
            \dropping."
    , live $ scalar "menu-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themewhite"
        `described` "Default style of menu."
    , live $ scalar "menu-selected-style" WindowScope KStyle
        "bg=themeyellow,fg=themeblack"
        `described` "Default style of selected menu item."
    , live $ scalar "menu-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
        `described` "Default style of menu borders."
    , live $ scalar "menu-border-lines" WindowScope (KChoice popupLines) "single"
        `described` "Type of characters used to draw menu border lines. Some \
            \of these are only supported on terminals with UTF-8 support."
    , live $ scalar "message-limit" ServerScope (KNumber 0 intMax) "1000"
        `described` "Maximum number of server messages to keep."
    , live $ scalar "prefix-timeout" ServerScope (KNumber 0 intMax) "0"
        `described` "The timeout for the prefix key if no subsequent key is \
            \pressed. Zero means disabled."
        `measuredIn` "milliseconds"
    , live $ scalar "prompt-history-limit" ServerScope (KNumber 0 intMax) "100"
        `described` "Maximum number of commands to keep in history."
    , live $ scalar "set-clipboard" ServerScope
        (KChoice ["off", "external", "on"]) "external"
        `described` "Whether to attempt to set the system clipboard ('on' or \
            \'external') and whether to allow applications to create paste \
            \buffers with an escape sequence ('on' only)."
    , live $ arrayOf "terminal-overrides" ServerScope KString "," ["linux*:AX@"]
        `described` "List of terminal capabilities overrides."
    , live $ arrayOf "terminal-features" ServerScope KString ","
        [ "xterm*:clipboard:ccolour:cstyle:focus:title", "screen*:title"
        , "rxvt*:ignorefkeys" ]
        `described` "List of terminal features, used if they cannot be \
            \automatically detected."
    , live $ scalar "theme" ServerScope
        (KChoice ["detect", "terminal", "light", "dark"]) "detect"
        `described` "Whether tmux should detect the terminal theme, use \
            \terminal ANSI colours, or force the light or dark theme."
    , live $ themeColour "dark-theme-black" "gray5" "black"
        `described` "Dark theme colour for black."
    , live $ themeColour "dark-theme-white" "gray90" "white"
        `described` "Dark theme colour for white."
    , live $ themeColour "dark-theme-light-grey" "gray70" "white"
        `described` "Dark theme colour for light grey."
    , live $ themeColour "dark-theme-dark-grey" "gray15" "black"
        `described` "Dark theme colour for dark grey."
    , live $ themeColour "dark-theme-green" "yellowgreen" "green"
        `described` "Dark theme colour for green."
    , live $ themeColour "dark-theme-yellow" "darkgoldenrod" "yellow"
        `described` "Dark theme colour for yellow."
    , live $ themeColour "dark-theme-red" "indianred" "red"
        `described` "Dark theme colour for red."
    , live $ themeColour "dark-theme-blue" "skyblue3" "blue"
        `described` "Dark theme colour for blue."
    , live $ themeColour "dark-theme-cyan" "cadetblue" "cyan"
        `described` "Dark theme colour for cyan."
    , live $ themeColour "dark-theme-magenta" "mediumpurple" "magenta"
        `described` "Dark theme colour for magenta."
    , live $ themeColour "light-theme-black" "gray10" "black"
        `described` "Light theme colour for black."
    , live $ themeColour "light-theme-white" "gray95" "white"
        `described` "Light theme colour for white."
    , live $ themeColour "light-theme-light-grey" "gray80" "white"
        `described` "Light theme colour for light grey."
    , live $ themeColour "light-theme-dark-grey" "gray45" "black"
        `described` "Light theme colour for dark grey."
    , live $ themeColour "light-theme-green" "seagreen" "green"
        `described` "Light theme colour for green."
    , live $ themeColour "light-theme-yellow" "darkgoldenrod" "yellow"
        `described` "Light theme colour for yellow."
    , live $ themeColour "light-theme-red" "indianred4" "red"
        `described` "Light theme colour for red."
    , live $ themeColour "light-theme-blue" "steelblue" "blue"
        `described` "Light theme colour for blue."
    , live $ themeColour "light-theme-cyan" "darkcyan" "cyan"
        `described` "Light theme colour for cyan."
    , live $ themeColour "light-theme-magenta" "purple4" "magenta"
        `described` "Light theme colour for magenta."
    , live $ arrayOf "user-keys" ServerScope KString "," []
        `described` "User key assignments. Each sequence in the list is \
            \translated into a key: 'User0', 'User1' and so on."
    , notYet $ scalar "variation-selector-always-wide" ServerScope KFlag "on"
        `described` "If the Unicode VS16 codepoint should always be treated as \
            \a wide character."

    -- Session options.
    , live $ scalar "activity-action" SessionScope (KChoice bellActions) "other"
        `described` "Action to take on an activity alert."
    , live $ scalar "assume-paste-time" SessionScope (KNumber 0 intMax) "1"
        `described` "Maximum time between input to assume it is pasting rather \
            \than typing."
        `measuredIn` "milliseconds"
    , live $ scalar "base-index" SessionScope (KNumber 0 intMax) "0"
        `described` "Default index of the first window in each session."
    , live $ scalar "bell-action" SessionScope (KChoice bellActions) "any"
        `described` "Action to take on a bell alert."
    , live $ scalar "default-command" SessionScope KString ""
        `described` "Default command to run in new panes. If empty, a shell is \
            \started."
    , live $ scalar "default-shell" SessionScope KString "/bin/sh"
        `described` "Location of default shell."
    , live $ (scalar "default-size" SessionScope KString "80x24")
        { pattern = Just "[0-9]*x[0-9]*" }
        `described` "Initial size of new sessions."
    , live $ scalar "destroy-unattached" SessionScope
        (KChoice ["off", "on", "keep-last", "keep-group"]) "off"
        `described` "Whether to destroy sessions when they have no attached \
            \clients, or keep the last session whether in the group."
    , live $ scalar "detach-on-destroy" SessionScope
        (KChoice ["off", "on", "no-detached", "previous", "next"]) "on"
        `described` "Whether to detach when a session is destroyed, or switch \
            \the client to another session if any exist."
    -- tmux defaults to 750.
    , live $ scalar "display-time" SessionScope (KNumber 0 intMax) "3000"
        `described` "Time for which status line messages should appear."
        `measuredIn` "milliseconds"
    , live $ scalar "focus-follows-mouse" SessionScope KFlag "off"
        `described` "Whether moving the mouse into a pane selects it."
    -- tmux defaults to 2000.
    , live $ scalar "history-limit" SessionScope (KNumber 0 intMax) "50000"
        `described` "Maximum number of lines to keep in the history for each \
            \pane. If changed, the new value applies only to new panes."
        `measuredIn` "lines"
    , live $ scalar "initial-repeat-time" SessionScope (KNumber 0 2000000) "0"
        `described` "Time to wait for a key binding to repeat the first time \
            \the key is pressed, if it is bound with the '-r' flag. Subsequent \
            \presses use the 'repeat-time' option."
        `measuredIn` "milliseconds"
    , live $ scalar "key-table" SessionScope KString "root"
        `described` "Default key table. Key presses are first looked up in \
            \this table."
    , live $ scalar "lock-after-time" SessionScope (KNumber 0 intMax) "0"
        `described` "Time after which a client is locked if not used."
        `measuredIn` "seconds"
    , live $ scalar "lock-command" SessionScope KString "lock -np"
        `described` "Shell command to run to lock a client."
    , live $ scalar "message-command-style" SessionScope KStyle
        "bg=themeblack,fg=themeyellow,\
        \#{?#{m/r:(^|#,)IS(PANE|MODE)($|#,),#{prompt_flags}},,\
        \fill=themeblack}"
        `described` "Style of the command prompt when in command mode, if \
            \'status-keys' is set to 'vi'."
    , live $ scalar "message-format" SessionScope KString
        "#[#{?#{command_prompt},#{E:message-command-style},\
        \#{E:message-style}}]#{message}"
        `described` "Format string for the prompt and message area. The \
            \'#{message}' placeholder is replaced with the content."
    , live $ scalar "message-line" SessionScope
        (KChoice ["0", "1", "2", "3", "4"]) "0"
        `described` "Position (line) of messages and the command prompt."
    -- tmux's default draws in themeyellow and themeblack.
    , live $ scalar "message-style" SessionScope KStyle
        "bg=yellow,fg=black,\
        \#{?#{m/r:(^|#,)IS(PANE|MODE)($|#,),#{prompt_flags}},,\
        \fill=yellow}"
        `described` "Style of messages and the command prompt. A 'fill' \
            \attribute controls background clearing and a 'width' attribute \
            \(fixed or percentage) constrains the prompt area width."
    , live $ scalar "mouse" SessionScope KFlag "off"
        `described` "Whether the mouse is recognised and mouse key bindings \
            \are executed. Applications inside panes can use the mouse even \
            \when 'off'."
    , live $ scalar "prefix" SessionScope KKey "C-b"
        `described` "The prefix key."
    , live $ scalar "prefix2" SessionScope KKey "None"
        `described` "A second prefix key."
    , live $ scalar "renumber-windows" SessionScope KFlag "off"
        `described` "Whether windows are automatically renumbered rather than \
            \leaving gaps."
    , live $ scalar "repeat-time" SessionScope (KNumber 0 2000000) "500"
        `described` "Time to wait for a key binding to repeat, if it is bound \
            \with the '-r' flag."
        `measuredIn` "milliseconds"
    , live $ scalar "set-titles" SessionScope KFlag "off"
        `described` "Whether to set the terminal title, if supported."
    -- hat's own title rather than tmux's (see docs/options-audit.md)
    , live $ scalar "set-titles-string" SessionScope KString
        "#{J/80/#: :hat,\
        \#{?#{m/r:^[0-9]*$,#{session_name}},,#{session_name}},\
        \#{?#{||:#{automatic_rename},#{m/r:^[0-9]*$,#{window_name}}},,#{window_name}},\
        \#{~/#{HOME}:pane_current_path},\
        \#{?#{!=:#{pane_title},},#{pane_title},#{pane_current_command}}}"
        `described` "Format of the terminal title to set."
    , live $ scalar "silence-action" SessionScope (KChoice bellActions) "other"
        `described` "Action to take on a silence alert."
    , live $ scalar "status" SessionScope
        (KChoice ["off", "on", "2", "3", "4", "5"]) "on"
        `described` "Number of lines in the status line."
    , live $ scalar "status-bg" SessionScope KColour "default"
        `described` "Background colour of the status line. This option is \
            \deprecated, use 'status-style' instead."
    , live $ scalar "status-fg" SessionScope KColour "default"
        `described` "Foreground colour of the status line. This option is \
            \deprecated, use 'status-style' instead."
    , live $ arrayOf "status-format" SessionScope KString " ,"
        [statusFormat1, statusFormat2, statusFormat3]
        `described` "Formats for the status lines. Each array member is the \
            \format for one status line. The default status line is made up of \
            \several components which may be configured individually with \
            \other options such as 'status-left'."
    , live $ scalar "status-interval" SessionScope (KNumber 0 intMax) "15"
        `described` "Number of seconds between status line updates."
        `measuredIn` "seconds"
    , live $ scalar "status-justify" SessionScope
        (KChoice ["left", "centre", "right", "absolute-centre"]) "left"
        `described` "Position of the window list in the status line."
    , live $ scalar "status-keys" SessionScope (KChoice ["emacs", "vi"]) "emacs"
        `described` "Key set to use at the command prompt."
    , live $ scalar "status-left" SessionScope KString "[#{session_name}] "
        `described` "Contents of the left side of the status line."
    , live $ scalar "status-left-length" SessionScope (KNumber 0 shrtMax) "10"
        `described` "Maximum width of the left side of the status line."
    , live $ scalar "status-left-style" SessionScope KStyle "default"
        `described` "Style of the left side of the status line."
    , live $ scalar "status-position" SessionScope
        (KChoice ["top", "bottom"]) "bottom"
        `described` "Position of the status line."
    -- tmux's default also shows the pane title and window offset.
    , live $ scalar "status-right" SessionScope KString "%H:%M %d-%b-%y #H"
        `described` "Contents of the right side of the status line."
    , live $ scalar "status-right-length" SessionScope (KNumber 0 shrtMax) "40"
        `described` "Maximum width of the right side of the status line."
    , live $ scalar "status-right-style" SessionScope KStyle "default"
        `described` "Style of the right side of the status line."
    -- tmux defaults to bg=themegreen,fg=themeblack.
    , live $ scalar "status-style" SessionScope KStyle "fg=black,bg=green"
        `described` "Style of the status line."
    , live $ scalar "pane-status-current-style" WindowScope KStyle "underscore"
        `described` "Style of the current pane in the status line."
    , live $ scalar "pane-status-style" WindowScope KStyle "default"
        `described` "Style of panes in the status line, except the current \
            \pane."
    , live $ scalar "prompt-cursor-colour" SessionScope KColourString ""
        `described` "Colour of the cursor when in the command prompt."
    , live $ scalar "prompt-command-cursor-colour" SessionScope KColourString ""
        `described` "Colour of the cursor in the command prompt when in \
            \command mode, if 'status-keys' is set to 'vi'."
    , live $ scalar "prompt-cursor-style" SessionScope
        (KChoice cursorStyles) "default"
        `described` "Style of the cursor when in the command prompt."
    , live $ scalar "prompt-command-cursor-style" SessionScope
        (KChoice cursorStyles) "default"
        `described` "Style of the cursor in the command prompt when in command \
            \mode, if 'status-keys' is set to 'vi'."
    , live $ scalar "session-status-current-style" WindowScope KStyle
        "underscore"
        `described` "Style of the current session in the status line."
    , live $ scalar "session-status-style" WindowScope KStyle "default"
        `described` "Style of sessions in the status line, except the current \
            \session."
    -- tmux's default also lists MSYSTEM, WAYLAND_DISPLAY and the XDG_* names.
    , live $ arrayOf "update-environment" SessionScope KString " ,"
        [ "DISPLAY", "KRB5CCNAME", "SSH_ASKPASS", "SSH_AUTH_SOCK"
        , "SSH_AGENT_PID", "SSH_CONNECTION", "WINDOWID", "XAUTHORITY" ]
        `described` "List of environment variables to update in the session \
            \environment when a client is attached."
    , live $ scalar "visual-activity" SessionScope (KChoice visualBell) "off"
        `described` "How activity alerts should be shown: a message ('on'), a \
            \message and a bell ('both') or nothing ('off')."
    , live $ scalar "visual-bell" SessionScope (KChoice visualBell) "off"
        `described` "How bell alerts should be shown: a message ('on'), a \
            \message and a bell ('both') or nothing ('off')."
    , live $ scalar "visual-silence" SessionScope (KChoice visualBell) "off"
        `described` "How silence alerts should be shown: a message ('on'), a \
            \message and a bell ('both') or nothing ('off')."
    , live $ scalar "word-separators" SessionScope KString
        "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~"
        `described` "Characters considered to separate words."

    -- Window options.
    , live $ scalar "aggressive-resize" WindowScope KFlag "off"
        `described` "When 'window-size' is 'smallest', whether the maximum \
            \size of a window is the smallest attached session where it is the \
            \current window ('on') or the smallest session it is linked to \
            \('off')."
    , paneLocal $ live $ scalar "allow-passthrough" WindowPaneScope
        (KChoice ["off", "on", "all"]) "off"
        `described` "Whether applications are allowed to use the escape \
            \sequence to bypass tmux. Can be 'off' (disallowed), 'on' (allowed \
            \if the pane is visible), or 'all' (allowed even if the pane is \
            \invisible)."
    , paneLocal $ live $ scalar "allow-rename" WindowPaneScope KFlag "off"
        `described` "Whether applications are allowed to use the escape \
            \sequence to rename windows."
    , paneLocal $ live $ scalar "allow-set-title" WindowPaneScope KFlag "on"
        `described` "Whether applications are allowed to use the escape \
            \sequence to set the pane title."
    , paneLocal $ live $ scalar "alternate-screen" WindowPaneScope KFlag "on"
        `described` "Whether applications are allowed to use the alternate \
            \screen."
    , live $ scalar "automatic-rename" WindowScope KFlag "on"
        `described` "Whether windows are automatically renamed."
    -- tmux's default also marks panes in a mode or dead.
    , live $ scalar "automatic-rename-format" WindowScope KString
        "#{pane_current_command}"
        `described` "Format used to automatically rename windows."
    , live $ scalar "clock-mode-colour" WindowScope KColourString "themeblue"
        `described` "Colour of the clock in clock mode."
    , live $ scalar "clock-mode-style" WindowScope
        (KChoice ["12", "24", "12-with-seconds", "24-with-seconds"]) "24"
        `described` "Time format of the clock in clock mode."
    -- tmux defaults to themered.
    , live $ scalar "display-panes-active-colour" WindowScope KColourString "red"
        `described` "Colour of the active pane for 'display-panes'."
    , live $ scalar "display-panes-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
        `described` "Style of the pane borders in 'display-panes'."
    -- tmux defaults to themeblue.
    , live $ scalar "display-panes-colour" WindowScope KColourString "blue"
        `described` "Colour of not active panes for 'display-panes'."
    , live $ scalar "display-panes-format" WindowScope KString
        "#[align=right]#{pane_unzoomed_width}x#{pane_unzoomed_height}"
        `described` "Format of text shown by 'display-panes', expanded for \
            \each pane."
    , live $ scalar "display-panes-time" WindowScope (KNumber 1 intMax) "1000"
        `described` "Time for which 'display-panes' should show pane numbers."
        `measuredIn` "milliseconds"
    , live $ scalar "copy-mode-match-style" WindowScope KStyle
        "bg=themecyan,fg=themeblack"
        `described` "Style of search matches in copy mode."
    , live $ scalar "copy-mode-current-match-style" WindowScope KStyle
        "bg=thememagenta,fg=themeblack"
        `described` "Style of the current search match in copy mode."
    , live $ scalar "copy-mode-mark-style" WindowScope KStyle
        "bg=themeyellow,fg=themeblack"
        `described` "Style of the marked line in copy mode."
    , paneLocal $ live $ scalar "copy-mode-position-format" WindowPaneScope KString
        "#[align=right]#{t/p:top_line_time}\
        \#{?#{e|>:#{top_line_time},0}, ,}\
        \[#{copy_position}/#{copy_position_limit}]\
        \#{?search_timed_out, (timed out),\
        \#{?search_count, (#{search_count}\
        \#{?search_count_partial,+,} results),}}"
        `described` "Format of the position indicator in copy mode."
    -- tmux's default follows mode-style (#{E:mode-style}).
    , live $ scalar "copy-mode-position-style" WindowScope KStyle
        "bg=yellow,fg=black"
        `described` "Style of position indicator in copy mode."
    , live $ scalar "copy-mode-selection-style" WindowScope KStyle
        "#{E:mode-style}"
        `described` "Style of selection in copy mode."
    , live $ scalar "copy-mode-current-line-number-style" WindowScope KStyle
        "fg=themeyellow"
        `described` "Style of current line number in copy mode."
    , live $ scalar "copy-mode-current-line-style" WindowScope KStyle "default"
        `described` "Style of the line containing the cursor in copy mode."
    , live $ scalar "copy-mode-line-number-style" WindowScope KStyle
        "fg=themelightgrey,dim"
        `described` "Style of line numbers in copy mode."
    , live $ scalar "copy-mode-line-numbers" WindowScope
        (KChoice ["off", "default", "absolute", "relative", "hybrid"]) "off"
        `described` "Line number mode in copy mode."
    -- tmux's default shades the window's empty cells and dots those outside
    -- it.
    , live $ scalar "fill-character" WindowScope KString " "
        `described` "Format used to fill unused parts of window."
    , live $ scalar "main-pane-height" WindowScope KString "24"
        `described` "Height of the main pane in the 'main-horizontal' layout. \
            \This may be a percentage, for example '10%'."
    , live $ scalar "main-pane-width" WindowScope KString "80"
        `described` "Width of the main pane in the 'main-vertical' layout. \
            \This may be a percentage, for example '10%'."
    , live $ scalar "mode-keys" WindowScope (KChoice ["emacs", "vi"]) "emacs"
        `described` "Key set used in copy mode."
    , live $ scalar "mode-style" WindowScope KStyle "reverse"
        `described` "Style of indicators and highlighting in modes."
    , live $ scalar "monitor-activity" WindowScope KFlag "off"
        `described` "Whether an alert is triggered by activity."
    , live $ scalar "monitor-bell" WindowScope KFlag "on"
        `described` "Whether an alert is triggered by a bell."
    , live $ scalar "monitor-silence" WindowScope (KNumber 0 intMax) "0"
        `described` "Time after which an alert is triggered by silence. Zero \
            \means no alert."
    , live $ scalar "other-pane-height" WindowScope KString "0"
        `described` "Height of the other panes in the 'main-horizontal' \
            \layout. This may be a percentage, for example '10%'."
    , live $ scalar "other-pane-width" WindowScope KString "0"
        `described` "Height of the other panes in the 'main-vertical' layout. \
            \This may be a percentage, for example '10%'."
    -- tmux's default is a format: themegreen, or another theme colour for
    -- a modal, marked, synchronized or in-mode pane.
    , paneLocal $ live $ scalar "pane-active-border-style" WindowPaneScope KStyle
        "fg=green"
        `described` "Style of the active pane border."
    , live $ scalar "pane-base-index" WindowScope (KNumber 0 ushrtMax) "0"
        `described` "Index of the first pane in each window."
    , paneLocal $ live $ scalar "pane-border-format" WindowPaneScope KString
        "#{?pane_active,#[reverse],}#{pane_index}#[default] \"#{pane_title}\"\
        \#{?#{mouse},#[align=right]\
        \#[range=control|7][#{?#{pane_floating_flag},t,f}]#[norange]\
        \#[range=control|8][#{?#{window_zoomed_flag},u,z}]#[norange]\
        \#[range=control|9][x]#[norange],}"
        `described` "Format of text in the pane status lines."
    , aliasing [("color", "colour")] $ live $ scalar "pane-border-indicators" WindowScope
        (KChoice ["off", "colour", "arrows", "both"]) "colour"
        `described` "Whether to indicate the active pane by colouring border \
            \or displaying arrow markers."
    , paneLocal $ live $ scalar "pane-border-lines" WindowPaneScope
            (KChoice ["single", "double", "heavy", "simple", "number", "spaces", "none"])
            "single"
        `described` "Type of characters used to draw pane border lines. Some \
            \of these are only supported on terminals with UTF-8 support."
    , paneLocal $ live $ scalar "pane-border-status" WindowPaneScope
        (KChoice ["off", "top", "bottom", "top-floating", "bottom-floating"]) "off"
        `described` "Position of the pane status lines."
    -- tmux defaults to fg=themelightgrey.
    , paneLocal $ live $ scalar "pane-border-style" WindowPaneScope KStyle "default"
        `described` "Style of the pane status lines."
    , paneLocal $ live $ arrayOf "pane-colours" WindowPaneScope KColour " ," []
        `described` "The default colour palette for colours zero to 255."
    , live $ scalar "pane-scrollbars" WindowScope
        (KChoice ["off", "modal", "on", "auto-hide"]) "off"
        `described` "Pane scrollbar state: off, on, modal, or auto-hide."
    , live $ scalar "pane-scrollbars-timeout" WindowScope (KNumber 0 intMax) "500"
        `described` "Time before modal and auto-hide pane scrollbars disappear."
        `measuredIn` "milliseconds"
    , paneLocal $ live $ scalar "pane-scrollbars-style" WindowPaneScope KStyle
        "bg=themedarkgrey,fg=themelightgrey,width=1,pad=0"
        `described` "Style of the pane scrollbar."
    , live $ scalar "pane-scrollbars-position" WindowScope
        (KChoice ["right", "left"]) "right"
        `described` "Pane scrollbar position."
    , live $ scalar "popup-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themewhite"
        `described` "Default style of popups."
    , live $ scalar "popup-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
        `described` "Default style of popup borders."
    , live $ scalar "popup-border-lines" WindowScope (KChoice popupLines) "single"
        `described` "Type of characters used to draw popup border lines. Some \
            \of these are only supported on terminals with UTF-8 support."
    , paneLocal $ live $
        scalar "remain-on-exit" WindowPaneScope
            (KChoice ["off", "on", "failed", "key"]) "off"
        `described` "Whether panes should remain ('on'), remain until a key is \
            \pressed ('key') or be automatically killed ('off' or 'failed') \
            \when the program inside exits."
    , paneLocal $ live $ scalar "remain-on-exit-format" WindowPaneScope KString
        "Pane is dead (\
        \#{?#{!=:#{pane_dead_status},},status #{pane_dead_status},}\
        \#{?#{!=:#{pane_dead_signal},},signal #{pane_dead_signal},}, \
        \#{t:pane_dead_time})"
        `described` "Message shown after the program in a pane has exited, if \
            \remain-on-exit is enabled."
    , paneLocal $ live $ scalar "scroll-on-clear" WindowPaneScope KFlag "on"
        `described` "Whether the contents of the screen should be scrolled \
            \intohistory when clearing the whole screen."
    -- Read only by switch-mode (tmux's window-switch.c), which hat lacks.
    , notYet $ scalar "switch-mode-match-style" WindowPaneScope KStyle
        "bg=cyan fg=black"
        `described` "Style of matched characters in switch mode."
    , paneLocal $ live $ scalar "synchronize-panes" WindowPaneScope KFlag "off"
        `described` "Whether typing should be sent to all panes simultaneously."
    , live $ scalar "tiled-layout-max-columns" WindowScope
        (KNumber 0 ushrtMax) "0"
        `described` "Maximum number of columns in the 'tiled' layout. A value \
            \of 0 means no limit."
    , live $ scalar "tree-mode-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
        `described` "Style of borders in tree mode."
    , paneLocal $ live $ scalar "tree-mode-preview-format" WindowPaneScope KString
        "#{?pane_format,#{pane_index}:#{pane_title},\
        \#{window_index}:#{window_name}}"
        `described` "Format of the preview indicator in tree mode."
    , live $ scalar "tree-mode-preview-style" WindowScope KStyle
        "fg=#{?#{||:#{&&:#{pane_format},#{pane_active}},\
        \#{&&:#{window_format},#{window_active}}},themered,themeblue}"
        `described` "Style of preview indicator in tree mode."
    , live $ scalar "tree-mode-selection-style" WindowScope KStyle
        "#{E:mode-style}"
        `described` "Style of the selected line in tree mode."
    , refusing (dimmed "window-active-style") $ paneLocal $ live $
        scalar "window-active-style" WindowPaneScope KStyle "default"
        `described` "Default style of the active pane."
    , live $ scalar "window-pane-current-status-format" WindowScope KString
        "#P:[#T]#{?pane_flags,#{pane_flags}, }"
        `described` "Format of the current window pane in the status line."
    , live $ scalar "window-pane-status-format" WindowScope KString
        "#P:[#T]#{?pane_flags,#{pane_flags}, }"
        `described` "Format of window panes in the status line, except the \
            \current pane."
    , live $ scalar "window-size" WindowScope
        (KChoice ["largest", "smallest", "manual", "latest"]) "latest"
        `described` "How window size is calculated. 'latest' uses the size of \
            \the most recently used client, 'largest' the largest client, \
            \'smallest' the smallest client and 'manual' a size set by the \
            \'resize-window' command."
    , refusing (dimmed "window-style") $ paneLocal $ live $
        scalar "window-style" WindowPaneScope KStyle "default"
        `described` "Default style of panes that are not the active pane."
    -- tmux defaults to reverse.
    , live $ scalar "window-status-activity-style" WindowScope KStyle "default"
        `described` "Style of windows in the status line with an activity \
            \alert."
    -- tmux defaults to reverse.
    , live $ scalar "window-status-bell-style" WindowScope KStyle
        "fg=black,bg=green"
        `described` "Style of windows in the status line with a bell alert."
    -- tmux's default pads a window without flags with a space.
    , live $ scalar "window-status-current-format" WindowScope KString "#I:#W#F"
        `described` "Format of the current window in the status line."
    -- tmux defaults to underscore.
    , live $ scalar "window-status-current-style" WindowScope KStyle
        "fg=black,bg=green,underscore"
        `described` "Style of the current window in the status line."
    -- tmux's default pads a window without flags with a space.
    , live $ scalar "window-status-format" WindowScope KString "#I:#W#F"
        `described` "Format of windows in the status line, except the current \
            \window."
    , live $ scalar "window-status-last-style" WindowScope KStyle "default"
        `described` "Style of the last window in the status line."
    , live $ scalar "window-status-separator" WindowScope KString " "
        `described` "Separator between windows in the status line."
    -- tmux leaves it default, inheriting status-style.
    , live $ scalar "window-status-style" WindowScope KStyle "fg=black,bg=green"
        `described` "Style of windows in the status line, except the current \
            \and last windows."
    , live $ scalar "wrap-search" WindowScope KFlag "on"
        `described` "Whether searching in copy mode should wrap at the top or \
            \bottom."
    -- tmux keeps this only for compatibility: it has no effect there either.
    , live $ scalar "xterm-keys" WindowScope KFlag "on"
        `described` "Whether xterm-style function key sequences should be \
            \sent. This option is no longer used."

    -- Hook options.
    , hook "after-bind-key" SessionScope
    , hook "after-capture-pane" SessionScope
    , hook "after-copy-mode" SessionScope
    , hook "after-display-message" SessionScope
    , hook "after-display-panes" SessionScope
    , hook "after-kill-pane" SessionScope
    , hook "after-list-buffers" SessionScope
    , hook "after-list-clients" SessionScope
    , hook "after-list-keys" SessionScope
    , hook "after-list-panes" SessionScope
    , hook "after-list-sessions" SessionScope
    , hook "after-list-windows" SessionScope
    , hook "after-load-buffer" SessionScope
    , hook "after-lock-server" SessionScope
    , hook "after-new-session" SessionScope
    , hook "after-new-window" SessionScope
    , hook "after-paste-buffer" SessionScope
    , hook "after-pipe-pane" SessionScope
    , hook "after-refresh-client" SessionScope
    , hook "after-rename-session" SessionScope
    , hook "after-rename-window" SessionScope
    , hook "after-resize-pane" SessionScope
    , hook "after-resize-window" SessionScope
    , hook "after-save-buffer" SessionScope
    , hook "after-select-layout" SessionScope
    , hook "after-select-pane" SessionScope
    , hook "after-select-window" SessionScope
    , hook "after-send-keys" SessionScope
    , hook "after-set-buffer" SessionScope
    , hook "after-set-environment" SessionScope
    , hook "after-set-hook" SessionScope
    , hook "after-set-option" SessionScope
    , hook "after-show-environment" SessionScope
    , hook "after-show-messages" SessionScope
    , hook "after-show-options" SessionScope
    , hook "after-split-window" SessionScope
    , hook "after-swap-window" SessionScope
    , hook "after-unbind-key" SessionScope
    , hook "alert-activity" SessionScope
        `described` "Run when a window has activity."
    , hook "alert-bell" SessionScope
        `described` "Run when a window has received a bell."
    , hook "alert-silence" SessionScope
        `described` "Run when a window has been silent."
    , hook "client-active" SessionScope
        `described` "Run when a client becomes the latest active client of its \
            \session."
    , hook "client-attached" SessionScope
        `described` "Run when a client is attached."
    , hook "client-created" SessionScope
        `described` "Run when a client is created."
    , hook "client-closed" SessionScope
        `described` "Run when a client is closed."
    , hook "client-detached" SessionScope
        `described` "Run when a client is detached."
    , hook "client-focus-in" SessionScope
        `described` "Run when focus enters a client."
    , hook "client-focus-out" SessionScope
        `described` "Run when focus exits a client."
    , hook "client-resized" SessionScope
        `described` "Run when a client is resized."
    , hook "client-session-changed" SessionScope
        `described` "Run when a client's attached session is changed."
    , hook "client-light-theme" SessionScope
        `described` "Run when a client switches to a light theme."
    , hook "client-dark-theme" SessionScope
        `described` "Run when a client switches to a dark theme."
    , hook "command-error" SessionScope
        `described` "Run when a command fails."
    , hook "marked-pane-changed" SessionScope
        `described` "Run when the marked pane is set or cleared."
    , hook "pane-activity" WindowPaneScope
        `described` "Run when there is new output in a pane."
    , hook "pane-bell" WindowPaneScope
        `described` "Run when a pane receives a bell."
    , hook "pane-command-finished" WindowPaneScope
        `described` "Run when an OSC 133 command finishes in a pane."
    , hook "pane-command-started" WindowPaneScope
        `described` "Run when an OSC 133 command starts in a pane."
    , hook "pane-created" WindowPaneScope
        `described` "Run when a pane is created or respawned."
    , hook "pane-died" WindowPaneScope
        `described` "Run when the program running in a pane exits, but \
            \remain-on-exit is on so the pane has not closed."
    , hook "pane-exited" WindowPaneScope
        `described` "Run when the program running in a pane exits."
    , hook "pane-focus-in" WindowPaneScope
        `described` "Run when the focus enters a pane, if the focus-events \
            \option is on."
    , hook "pane-focus-out" WindowPaneScope
        `described` "Run when the focus exits a pane, if the focus-events \
            \option is on."
    , hook "pane-mode-changed" WindowPaneScope
        `described` "Run when a pane changes mode."
    , hook "pane-mode-entered" WindowPaneScope
        `described` "Run when a pane enters a mode."
    , hook "pane-mode-exited" WindowPaneScope
        `described` "Run when a pane exits a mode."
    , hook "pane-moved" WindowPaneScope
        `described` "Run when a pane is moved to another window."
    , hook "pane-prompt-closed" WindowPaneScope
        `described` "Run when a prompt in a pane is closed."
    , hook "pane-prompt-opened" WindowPaneScope
        `described` "Run when a prompt is opened in a pane."
    , hook "pane-resized" WindowPaneScope
        `described` "Run when a pane is resized."
    , hook "pane-set-clipboard" WindowPaneScope
        `described` "Run when the terminal clipboard is set using the xterm \
            \escape sequence."
    , hook "pane-shell-prompt" WindowPaneScope
        `described` "Run when an OSC 133 shell prompt starts in a pane."
    , hook "pane-title-changed" WindowPaneScope
        `described` "Run when a pane title is changed."
    , hook "session-closed" SessionScope
        `described` "Run when a session is closed."
    , hook "session-created" SessionScope
        `described` "Run when a new session is created."
    , hook "session-added-to-group" SessionScope
        `described` "Run when a session is added to a session group."
    , hook "session-renamed" SessionScope
        `described` "Run when a session is renamed."
    , hook "session-removed-from-group" SessionScope
        `described` "Run when a session is removed from a session group."
    , hook "session-window-changed" SessionScope
        `described` "Run when a session changes its active window."
    , hook "window-created" WindowScope
        `described` "Run when a window is created."
    , hook "window-closed" WindowScope
        `described` "Run when a window is closed."
    , hook "window-layout-changed" WindowScope
        `described` "Run when a window layout is changed."
    , hook "window-linked" SessionScope
        `described` "Run when a window is linked into a session."
    , hook "window-pane-changed" WindowScope
        `described` "Run when a window changes its active pane."
    , hook "window-renamed" WindowScope
        `described` "Run when a window is renamed."
    , hook "window-resized" WindowScope
        `described` "Run when a window is resized. This may be after the \
            \client-resized hook is run."
    , hook "window-unzoomed" WindowScope
        `described` "Run when a window is unzoomed."
    , hook "window-zoomed" WindowScope
        `described` "Run when a window is zoomed."
    , hook "window-unlinked" SessionScope
        `described` "Run when a window is unlinked from a session."
    ]
  where
    themeColour n rich basic = scalar n ServerScope KColourString
        ("#{?#{e|>=:#{client_colours},256}," <> rich <> "," <> basic <> "}")
    -- A pane's colours are not yet dimmed (tmux's @dim=@).
    dimmed n v
        | any ("dim=" `T.isPrefixOf`) (T.split (`elem` (" ," :: String)) (T.toLower v)) =
            Just (n <> ": dim= not yet supported by hat")
        | otherwise = Nothing
