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
    , pattern = Nothing, defaultValue = OVScalar def, gate = NotYet }

-- | An array option whose default is the given entries at keys 0, 1, ….
arrayOf :: Text -> OptionScope -> OptionKind -> Text -> [Text] -> OptionDef
arrayOf n sc k sep def = OptionDef
    { name = n, scope = sc, kind = k, array = Just sep, role = PlainOption
    , pattern = Nothing
    , defaultValue = OVArray (Map.fromList (zip (map KeyIndex [0 ..]) def))
    , gate = NotYet }

hook :: Text -> OptionScope -> OptionDef
hook n sc = (arrayOf n sc KCommand "" []) { role = HookOption, gate = Live fullSupport }

-- | Leave a row registered but unimplemented.
notYet :: OptionDef -> OptionDef
notYet d = d { gate = NotYet }

-- | Mark a row implemented over tmux's whole value space.
live :: OptionDef -> OptionDef
live d = d { gate = Live fullSupport }

fullSupport :: LiveSupport
fullSupport = LiveSupport
    { onlyChoices = Nothing, paneValues = PaneValuesIgnored, refuse = const Nothing }

-- | Narrow a live choice option to the values hat implements.
choicesOnly :: [Text] -> OptionDef -> OptionDef
choicesOnly cs = adjustSupport (\s -> s { onlyChoices = Just cs })

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
    , live $ scalar "buffer-limit" ServerScope (KNumber 1 intMax) "50"
    -- hat's default omits tmux's aliases for commands hat lacks.
    , live $ arrayOf "command-alias" ServerScope KString ","
        ["split-pane=split-window", "splitp=split-window"]
    , notYet $ arrayOf "codepoint-widths" ServerScope KString "," []
    , live $ scalar "copy-command" ServerScope KString ""
    , paneLocal $ live $
        scalar "cursor-colour" WindowPaneScope KColourString ""
    , paneLocal $ live $ scalar "cursor-style" WindowPaneScope (KChoice cursorStyles) "default"
    , notYet $ scalar "default-client-command" ServerScope KCommand "new-session"
    , live $ scalar "default-terminal" ServerScope KString "tmux-256color"
    , notYet $ scalar "editor" ServerScope KString "/usr/bin/vi"
    -- tmux defaults to 10; hat's native escape handling needs no wait.
    , live $ scalar "escape-time" ServerScope (KNumber 0 intMax) "0"
    , live $ scalar "exit-empty" ServerScope KFlag "on"
    , live $ scalar "exit-unattached" ServerScope KFlag "off"
    , live $ scalar "extended-keys" ServerScope
        (KChoice ["off", "on", "always"]) "off"
    , live $ scalar "extended-keys-format" ServerScope
        (KChoice ["csi-u", "xterm"]) "xterm"
    , live $ scalar "focus-events" ServerScope KFlag "off"
    , live $ scalar "get-clipboard" ServerScope
        (KChoice ["off", "buffer", "request", "both"]) "buffer"
    , live $ scalar "history-file" ServerScope KString ""
    , live $ scalar "input-buffer-size" ServerScope
        (KNumber 1048576 uintMax) "1048576"
    , live $ scalar "menu-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themewhite"
    , live $ scalar "menu-selected-style" WindowScope KStyle
        "bg=themeyellow,fg=themeblack"
    , live $ scalar "menu-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
    , live $ scalar "menu-border-lines" WindowScope (KChoice popupLines) "single"
    , live $ scalar "message-limit" ServerScope (KNumber 0 intMax) "1000"
    , live $ scalar "prefix-timeout" ServerScope (KNumber 0 intMax) "0"
    , live $ scalar "prompt-history-limit" ServerScope (KNumber 0 intMax) "100"
    , live $ scalar "set-clipboard" ServerScope
        (KChoice ["off", "external", "on"]) "external"
    , live $ arrayOf "terminal-overrides" ServerScope KString "," ["linux*:AX@"]
    , live $ arrayOf "terminal-features" ServerScope KString ","
        [ "xterm*:clipboard:ccolour:cstyle:focus:title", "screen*:title"
        , "rxvt*:ignorefkeys" ]
    , live $ scalar "theme" ServerScope
        (KChoice ["detect", "terminal", "light", "dark"]) "detect"
    , live $ themeColour "dark-theme-black" "gray5" "black"
    , live $ themeColour "dark-theme-white" "gray90" "white"
    , live $ themeColour "dark-theme-light-grey" "gray70" "white"
    , live $ themeColour "dark-theme-dark-grey" "gray15" "black"
    , live $ themeColour "dark-theme-green" "yellowgreen" "green"
    , live $ themeColour "dark-theme-yellow" "darkgoldenrod" "yellow"
    , live $ themeColour "dark-theme-red" "indianred" "red"
    , live $ themeColour "dark-theme-blue" "skyblue3" "blue"
    , live $ themeColour "dark-theme-cyan" "cadetblue" "cyan"
    , live $ themeColour "dark-theme-magenta" "mediumpurple" "magenta"
    , live $ themeColour "light-theme-black" "gray10" "black"
    , live $ themeColour "light-theme-white" "gray95" "white"
    , live $ themeColour "light-theme-light-grey" "gray80" "white"
    , live $ themeColour "light-theme-dark-grey" "gray45" "black"
    , live $ themeColour "light-theme-green" "seagreen" "green"
    , live $ themeColour "light-theme-yellow" "darkgoldenrod" "yellow"
    , live $ themeColour "light-theme-red" "indianred4" "red"
    , live $ themeColour "light-theme-blue" "steelblue" "blue"
    , live $ themeColour "light-theme-cyan" "darkcyan" "cyan"
    , live $ themeColour "light-theme-magenta" "purple4" "magenta"
    , live $ arrayOf "user-keys" ServerScope KString "," []
    , notYet $ scalar "variation-selector-always-wide" ServerScope KFlag "on"

    -- Session options.
    , live $ scalar "activity-action" SessionScope (KChoice bellActions) "other"
    , live $ scalar "assume-paste-time" SessionScope (KNumber 0 intMax) "1"
    , live $ scalar "base-index" SessionScope (KNumber 0 intMax) "0"
    , live $ scalar "bell-action" SessionScope (KChoice bellActions) "any"
    , live $ scalar "default-command" SessionScope KString ""
    , live $ scalar "default-shell" SessionScope KString "/bin/sh"
    , live $ (scalar "default-size" SessionScope KString "80x24")
        { pattern = Just "[0-9]*x[0-9]*" }
    , live $ scalar "destroy-unattached" SessionScope
        (KChoice ["off", "on", "keep-last", "keep-group"]) "off"
    , live $ scalar "detach-on-destroy" SessionScope
        (KChoice ["off", "on", "no-detached", "previous", "next"]) "on"
    -- tmux defaults to 750.
    , live $ scalar "display-time" SessionScope (KNumber 0 intMax) "3000"
    , live $ scalar "focus-follows-mouse" SessionScope KFlag "off"
    -- tmux defaults to 2000.
    , live $ scalar "history-limit" SessionScope (KNumber 0 intMax) "50000"
    , live $ scalar "initial-repeat-time" SessionScope (KNumber 0 2000000) "0"
    , live $ scalar "key-table" SessionScope KString "root"
    , live $ scalar "lock-after-time" SessionScope (KNumber 0 intMax) "0"
    , live $ scalar "lock-command" SessionScope KString "lock -np"
    , live $ scalar "message-command-style" SessionScope KStyle
        "bg=themeblack,fg=themeyellow,\
        \#{?#{m/r:(^|#,)IS(PANE|MODE)($|#,),#{prompt_flags}},,\
        \fill=themeblack}"
    , live $ scalar "message-format" SessionScope KString
        "#[#{?#{command_prompt},#{E:message-command-style},\
        \#{E:message-style}}]#{message}"
    , live $ scalar "message-line" SessionScope
        (KChoice ["0", "1", "2", "3", "4"]) "0"
    , live $ scalar "message-style" SessionScope KStyle
        "bg=themeyellow,fg=themeblack,\
        \#{?#{m/r:(^|#,)IS(PANE|MODE)($|#,),#{prompt_flags}},,\
        \fill=themeyellow}"
    , live $ scalar "mouse" SessionScope KFlag "off"
    , live $ scalar "prefix" SessionScope KKey "C-b"
    , live $ scalar "prefix2" SessionScope KKey "None"
    , live $ scalar "renumber-windows" SessionScope KFlag "off"
    , live $ scalar "repeat-time" SessionScope (KNumber 0 2000000) "500"
    , live $ scalar "set-titles" SessionScope KFlag "off"
    , notYet $ scalar "set-titles-string" SessionScope KString
        "#S:#I:#W - \"#T\" #{session_alerts}"
    , live $ scalar "silence-action" SessionScope (KChoice bellActions) "other"
    , live $ scalar "status" SessionScope
        (KChoice ["off", "on", "2", "3", "4", "5"]) "on"
    , live $ scalar "status-bg" SessionScope KColour "default"
    , live $ scalar "status-fg" SessionScope KColour "default"
    , live $ arrayOf "status-format" SessionScope KString " ,"
        [statusFormat1, statusFormat2, statusFormat3]
    , live $ scalar "status-interval" SessionScope (KNumber 0 intMax) "15"
    , live $ scalar "status-justify" SessionScope
        (KChoice ["left", "centre", "right", "absolute-centre"]) "left"
    , live $ scalar "status-keys" SessionScope (KChoice ["emacs", "vi"]) "emacs"
    , live $ scalar "status-left" SessionScope KString "[#{session_name}] "
    , live $ scalar "status-left-length" SessionScope (KNumber 0 shrtMax) "10"
    , live $ scalar "status-left-style" SessionScope KStyle "default"
    , live $ scalar "status-position" SessionScope
        (KChoice ["top", "bottom"]) "bottom"
    -- tmux's default also shows the pane title and window offset.
    , live $ scalar "status-right" SessionScope KString "%H:%M %d-%b-%y #H"
    , live $ scalar "status-right-length" SessionScope (KNumber 0 shrtMax) "40"
    , live $ scalar "status-right-style" SessionScope KStyle "default"
    , live $ scalar "status-style" SessionScope KStyle "bg=themegreen,fg=themeblack"
    , live $ scalar "pane-status-current-style" WindowScope KStyle "underscore"
    , live $ scalar "pane-status-style" WindowScope KStyle "default"
    , live $ scalar "prompt-cursor-colour" SessionScope KColourString ""
    , live $ scalar "prompt-command-cursor-colour" SessionScope KColourString ""
    , live $ scalar "prompt-cursor-style" SessionScope
        (KChoice cursorStyles) "default"
    , live $ scalar "prompt-command-cursor-style" SessionScope
        (KChoice cursorStyles) "default"
    , live $ scalar "session-status-current-style" WindowScope KStyle
        "underscore"
    , live $ scalar "session-status-style" WindowScope KStyle "default"
    -- tmux's default also lists MSYSTEM, WAYLAND_DISPLAY and the XDG_* names.
    , live $ arrayOf "update-environment" SessionScope KString " ,"
        [ "DISPLAY", "KRB5CCNAME", "SSH_ASKPASS", "SSH_AUTH_SOCK"
        , "SSH_AGENT_PID", "SSH_CONNECTION", "WINDOWID", "XAUTHORITY" ]
    , live $ scalar "visual-activity" SessionScope (KChoice visualBell) "off"
    , live $ scalar "visual-bell" SessionScope (KChoice visualBell) "off"
    , live $ scalar "visual-silence" SessionScope (KChoice visualBell) "off"
    , live $ scalar "word-separators" SessionScope KString
        "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~"

    -- Window options.
    , live $ scalar "aggressive-resize" WindowScope KFlag "off"
    , paneLocal $ live $ scalar "allow-passthrough" WindowPaneScope
        (KChoice ["off", "on", "all"]) "off"
    , paneLocal $ live $ scalar "allow-rename" WindowPaneScope KFlag "off"
    , paneLocal $ live $ scalar "allow-set-title" WindowPaneScope KFlag "on"
    , paneLocal $ live $ scalar "alternate-screen" WindowPaneScope KFlag "on"
    , live $ scalar "automatic-rename" WindowScope KFlag "on"
    -- tmux's default also marks panes in a mode or dead.
    , live $ scalar "automatic-rename-format" WindowScope KString
        "#{pane_current_command}"
    , live $ scalar "clock-mode-colour" WindowScope KColourString "themeblue"
    , live $ scalar "clock-mode-style" WindowScope
        (KChoice ["12", "24", "12-with-seconds", "24-with-seconds"]) "24"
    , live $ scalar "display-panes-active-colour" WindowScope KColourString "themered"
    , notYet $ scalar "display-panes-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
    , live $ scalar "display-panes-colour" WindowScope KColourString "themeblue"
    , live $ scalar "display-panes-format" WindowScope KString
        "#[align=right]#{pane_unzoomed_width}x#{pane_unzoomed_height}"
    , live $ scalar "display-panes-time" WindowScope (KNumber 1 intMax) "1000"
    , live $ scalar "copy-mode-match-style" WindowScope KStyle
        "bg=themecyan,fg=themeblack"
    , live $ scalar "copy-mode-current-match-style" WindowScope KStyle
        "bg=thememagenta,fg=themeblack"
    , live $ scalar "copy-mode-mark-style" WindowScope KStyle
        "bg=themeyellow,fg=themeblack"
    , paneLocal $ live $ scalar "copy-mode-position-format" WindowPaneScope KString
        "#[align=right]#{t/p:top_line_time}\
        \#{?#{e|>:#{top_line_time},0}, ,}\
        \[#{copy_position}/#{copy_position_limit}]\
        \#{?search_timed_out, (timed out),\
        \#{?search_count, (#{search_count}\
        \#{?search_count_partial,+,} results),}}"
    , live $ scalar "copy-mode-position-style" WindowScope KStyle
        "#{E:mode-style}"
    , live $ scalar "copy-mode-selection-style" WindowScope KStyle
        "#{E:mode-style}"
    , live $ scalar "copy-mode-current-line-number-style" WindowScope KStyle
        "fg=themeyellow"
    , live $ scalar "copy-mode-current-line-style" WindowScope KStyle "default"
    , live $ scalar "copy-mode-line-number-style" WindowScope KStyle
        "fg=themelightgrey,dim"
    , live $ scalar "copy-mode-line-numbers" WindowScope
        (KChoice ["off", "default", "absolute", "relative", "hybrid"]) "off"
    , live $ scalar "fill-character" WindowScope KString
        "#{?is_inside,#[bg=themedarkgrey] ,#[fg=themelightgrey]#[acs]~}"
    , live $ scalar "main-pane-height" WindowScope KString "24"
    , live $ scalar "main-pane-width" WindowScope KString "80"
    , live $ scalar "mode-keys" WindowScope (KChoice ["emacs", "vi"]) "emacs"
    , live $ scalar "mode-style" WindowScope KStyle "reverse"
    , live $ scalar "monitor-activity" WindowScope KFlag "off"
    , live $ scalar "monitor-bell" WindowScope KFlag "on"
    , live $ scalar "monitor-silence" WindowScope (KNumber 0 intMax) "0"
    , live $ scalar "other-pane-height" WindowScope KString "0"
    , live $ scalar "other-pane-width" WindowScope KString "0"
    , paneLocal $ live $ scalar "pane-active-border-style" WindowPaneScope KStyle
        "fg=#{?pane_modal_flag,themeblue,\
        \#{?pane_marked,thememagenta,\
        \#{?synchronize-panes,themered,\
        \#{?pane_in_mode,themeyellow,themegreen}}}}"
    , live $ scalar "pane-base-index" WindowScope (KNumber 0 ushrtMax) "0"
    , paneLocal $ live $ scalar "pane-border-format" WindowPaneScope KString
        "#{?pane_active,#[reverse],}#{pane_index}#[default] \"#{pane_title}\"\
        \#{?#{mouse},#[align=right]\
        \#[range=control|7][#{?#{pane_floating_flag},t,f}]#[norange]\
        \#[range=control|8][#{?#{window_zoomed_flag},u,z}]#[norange]\
        \#[range=control|9][x]#[norange],}"
    , live $ scalar "pane-border-indicators" WindowScope
        (KChoice ["off", "colour", "arrows", "both"]) "colour"
    , paneLocal $ live $ scalar "pane-border-lines" WindowPaneScope
            (KChoice ["single", "double", "heavy", "simple", "number", "spaces", "none"])
            "single"
    , paneLocal $ live $ scalar "pane-border-status" WindowPaneScope
        (KChoice ["off", "top", "bottom", "top-floating", "bottom-floating"]) "off"
    , paneLocal $ live $ scalar "pane-border-style" WindowPaneScope KStyle
        "fg=themelightgrey"
    , paneLocal $ live $ arrayOf "pane-colours" WindowPaneScope KColour " ," []
    , live $ scalar "pane-scrollbars" WindowScope
        (KChoice ["off", "modal", "on", "auto-hide"]) "off"
    , live $ scalar "pane-scrollbars-timeout" WindowScope (KNumber 0 intMax) "500"
    , paneLocal $ live $ scalar "pane-scrollbars-style" WindowPaneScope KStyle
        "bg=themedarkgrey,fg=themelightgrey,width=1,pad=0"
    , live $ scalar "pane-scrollbars-position" WindowScope
        (KChoice ["right", "left"]) "right"
    , live $ scalar "popup-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themewhite"
    , live $ scalar "popup-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
    , live $ scalar "popup-border-lines" WindowScope (KChoice popupLines) "single"
    , paneLocal $ live $
        scalar "remain-on-exit" WindowPaneScope
            (KChoice ["off", "on", "failed", "key"]) "off"
    , paneLocal $ live $ scalar "remain-on-exit-format" WindowPaneScope KString
        "Pane is dead (\
        \#{?#{!=:#{pane_dead_status},},status #{pane_dead_status},}\
        \#{?#{!=:#{pane_dead_signal},},signal #{pane_dead_signal},}, \
        \#{t:pane_dead_time})"
    , paneLocal $ live $ scalar "scroll-on-clear" WindowPaneScope KFlag "on"
    -- Read only by switch-mode (tmux's window-switch.c), which hat lacks.
    , notYet $ scalar "switch-mode-match-style" WindowPaneScope KStyle
        "bg=cyan fg=black"
    , paneLocal $ live $ scalar "synchronize-panes" WindowPaneScope KFlag "off"
    , live $ scalar "tiled-layout-max-columns" WindowScope
        (KNumber 0 ushrtMax) "0"
    , notYet $ scalar "tree-mode-border-style" WindowScope KStyle
        "bg=themedarkgrey,fg=themelightgrey"
    , notYet $ scalar "tree-mode-preview-format" WindowPaneScope KString
        "#{?pane_format,#{pane_index}:#{pane_title},\
        \#{window_index}:#{window_name}}"
    , notYet $ scalar "tree-mode-preview-style" WindowScope KStyle
        "fg=#{?#{||:#{&&:#{pane_format},#{pane_active}},\
        \#{&&:#{window_format},#{window_active}}},themered,themeblue}"
    , notYet $ scalar "tree-mode-selection-style" WindowScope KStyle
        "#{E:mode-style}"
    , refusing (dimmed "window-active-style") $ paneLocal $ live $
        scalar "window-active-style" WindowPaneScope KStyle "default"
    , live $ scalar "window-pane-current-status-format" WindowScope KString
        "#P:[#T]#{?pane_flags,#{pane_flags}, }"
    , live $ scalar "window-pane-status-format" WindowScope KString
        "#P:[#T]#{?pane_flags,#{pane_flags}, }"
    , live $ scalar "window-size" WindowScope
        (KChoice ["largest", "smallest", "manual", "latest"]) "latest"
    , refusing (dimmed "window-style") $ paneLocal $ live $
        scalar "window-style" WindowPaneScope KStyle "default"
    , live $ scalar "window-status-activity-style" WindowScope KStyle "reverse"
    , live $ scalar "window-status-bell-style" WindowScope KStyle "reverse"
    , live $ scalar "window-status-current-format" WindowScope KString
        "#I:#W#{?window_flags,#{window_flags}, }"
    , live $ scalar "window-status-current-style" WindowScope KStyle "underscore"
    , live $ scalar "window-status-format" WindowScope KString
        "#I:#W#{?window_flags,#{window_flags}, }"
    , live $ scalar "window-status-last-style" WindowScope KStyle "default"
    , live $ scalar "window-status-separator" WindowScope KString " "
    , live $ scalar "window-status-style" WindowScope KStyle "default"
    , live $ scalar "wrap-search" WindowScope KFlag "on"
    -- tmux keeps this only for compatibility: it has no effect there either.
    , live $ scalar "xterm-keys" WindowScope KFlag "on"

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
    , hook "alert-bell" SessionScope
    , hook "alert-silence" SessionScope
    , hook "client-active" SessionScope
    , hook "client-attached" SessionScope
    , hook "client-created" SessionScope
    , hook "client-closed" SessionScope
    , hook "client-detached" SessionScope
    , hook "client-focus-in" SessionScope
    , hook "client-focus-out" SessionScope
    , hook "client-resized" SessionScope
    , hook "client-session-changed" SessionScope
    , hook "client-light-theme" SessionScope
    , hook "client-dark-theme" SessionScope
    , hook "command-error" SessionScope
    , hook "marked-pane-changed" SessionScope
    , hook "pane-activity" WindowPaneScope
    , hook "pane-bell" WindowPaneScope
    , hook "pane-command-finished" WindowPaneScope
    , hook "pane-command-started" WindowPaneScope
    , hook "pane-created" WindowPaneScope
    , hook "pane-died" WindowPaneScope
    , hook "pane-exited" WindowPaneScope
    , hook "pane-focus-in" WindowPaneScope
    , hook "pane-focus-out" WindowPaneScope
    , hook "pane-mode-changed" WindowPaneScope
    , hook "pane-mode-entered" WindowPaneScope
    , hook "pane-mode-exited" WindowPaneScope
    , hook "pane-moved" WindowPaneScope
    , hook "pane-prompt-closed" WindowPaneScope
    , hook "pane-prompt-opened" WindowPaneScope
    , hook "pane-resized" WindowPaneScope
    , hook "pane-set-clipboard" WindowPaneScope
    , hook "pane-shell-prompt" WindowPaneScope
    , hook "pane-title-changed" WindowPaneScope
    , hook "session-closed" SessionScope
    , hook "session-created" SessionScope
    , hook "session-added-to-group" SessionScope
    , hook "session-renamed" SessionScope
    , hook "session-removed-from-group" SessionScope
    , hook "session-window-changed" SessionScope
    , hook "window-created" WindowScope
    , hook "window-closed" WindowScope
    , hook "window-layout-changed" WindowScope
    , hook "window-linked" SessionScope
    , hook "window-pane-changed" WindowScope
    , hook "window-renamed" WindowScope
    , hook "window-resized" WindowScope
    , hook "window-unzoomed" WindowScope
    , hook "window-zoomed" WindowScope
    , hook "window-unlinked" SessionScope
    ]
  where
    themeColour n rich basic = scalar n ServerScope KColourString
        ("#{?#{e|>=:#{client_colours},256}," <> rich <> "," <> basic <> "}")
    -- A pane's colours are not yet dimmed (tmux's @dim=@).
    dimmed n v
        | any ("dim=" `T.isPrefixOf`) (T.split (`elem` (" ," :: String)) (T.toLower v)) =
            Just (n <> ": dim= not yet supported by hat")
        | otherwise = Nothing
