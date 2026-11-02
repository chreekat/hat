-- | The built-in key bindings: what a server starts with before any config
-- runs. Each is a @bind-key@ line as tmux's @key_bindings_init@ has it,
-- where hat can run it, so the tables and a user's @bind-key@ speak exactly
-- the same language.
module Hat.Server.Keymap
    ( defaultKeymap
    , DefaultBinding (..)
    , defaultBindings
    , defaultKey
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Parser (parseScript)
import Hat.Command.Script (Script (..), ScriptItem (..))
import Hat.Model (tshow)
import Hat.Server.Command.Bind (bindArgs, isBlock, splitOnSemis)
import Hat.Server.Keys (Binding (..), Key (..), Keymap, Repeat, parseKeyName)

-- | One of tmux's default bindings: bound as written, or left out for the
-- blocker named.
data DefaultBinding
    = Bound Text
    | Unbound Text Text  -- ^ the tmux line, what hat lacks to run it

defaultKeymap :: Keymap
defaultKeymap = Map.fromListWith Map.union
    [ (table, Map.singleton key b)
    | Bound line <- defaultBindings, let (table, key, b) = parseDefault line ]

-- | A default @bind-key@ line's table and key, as written.
defaultKey :: Text -> (Text, Text)
defaultKey line = case bindWords line of
    (table, _, _, keyName : _) -> (table, keyName)
    _ -> badDefault line

-- | A default @bind-key@ line's table, key and binding.
parseDefault :: Text -> (Text, Text, Binding)
parseDefault line = case bindWords line of
    (table, note, repeats, keyName : rest)
        | Just key <- parseKeyName keyName
        , Just commands <- commandsOf rest ->
            (table, key.name, Binding { commands, repeats, note })
    _ -> badDefault line
  where
    commandsOf = \case
        [block] | isBlock block -> case parseScript block of
            Script items Nothing -> traverse (commandOf . snd) items
            _ -> Nothing
        rest -> Just (splitOnSemis rest)
    -- A nested block's text, unpadded.
    commandOf = \case
        ScriptCommand argv -> Just (map T.strip argv)
        _ -> Nothing

bindWords :: Text -> (Text, Maybe Text, Repeat, [Text])
bindWords line = case parseScript line of
    Script [(_, ScriptCommand (_ : args))] Nothing -> bindArgs args
    _ -> badDefault line

-- | A default line that does not parse is fatal, as in tmux.
badDefault :: Text -> a
badDefault line = error ("bad default key: " <> T.unpack line)

-- | tmux's default bindings in its order, each table's ending with Hat's own.
defaultBindings :: [DefaultBinding]
defaultBindings = prefixKeys <> moveKeys <> menuKeys <> rootKeys <> copyKeys <> copyViKeys

prefixKeys :: [DefaultBinding]
prefixKeys =
    [ Bound "bind -N 'Send the prefix key' C-b { send-prefix }"
    , Bound "bind -N 'Rotate through the panes' C-o { rotate-window }"
    , Unbound "bind -N 'Suspend the current client' C-z { suspend-client }" "suspend-client"
    , Bound "bind -N 'Select next layout' Space { next-layout }"
    , Bound "bind -N 'Break pane to a new window' ! { break-pane }"
    , Bound "bind -N 'Split window vertically' '\"' { split-window }"
    , Bound "bind -N 'List all paste buffers' '#' { list-buffers }"
    , Bound "bind -N 'Rename current session' '$' { command-prompt -I'#S' { rename-session -- '%%' } }"
    , Bound "bind -N 'Split window horizontally' % { split-window -h }"
    , unconfirmed "bind -N 'Kill current window' & { kill-window }"
    , Bound "bind -N 'Prompt for window index to select' \"'\" { command-prompt -pindex { select-window -t ':%%' } }"
    , Bound "bind -N 'New floating pane' * { new-pane }"
    , Bound "bind -N 'Toggle pane between floating and tiled' @ { if-shell -F '#{pane_floating_flag}' { join-pane } { break-pane -W } }"
    , Bound "bind -N 'Switch to previous client' ( { switch-client -p }"
    , Bound "bind -N 'Switch to next client' ) { switch-client -n }"
    , Bound "bind -N 'Rename current window' , { command-prompt -I'#W' { rename-window -- '%%' } }"
    , Bound "bind -N 'Delete the most recent paste buffer' - { delete-buffer }"
    , Bound "bind -N 'Move the current window' . { command-prompt { move-window -t '%%' } }"
    , Bound "bind -N 'Describe key binding' '/' { command-prompt -kpkey  { list-keys -1N '%%' } }"
    ]
    <> [ Bound ("bind -N 'Select window " <> i <> "' " <> i <> " { select-window -t:=" <> i <> " }")
       | i <- map tshow [0 .. 9 :: Int] ]
    <>
    [ Bound "bind -N 'Prompt for a command' : { command-prompt }"
    , Bound "bind -N 'Move to the previously active pane' \\; { last-pane }"
    , Bound "bind -N 'Choose a paste buffer from a list' = { choose-buffer -Z }"
    , Bound "bind -N 'List key bindings' ? { list-keys -N }"
    , Bound "bind -N 'Choose and detach a client from a list' D { choose-client -Z }"
    , Bound "bind -N 'Spread panes out evenly' E { select-layout -E }"
    , Bound "bind -N 'Switch to the last client' L { switch-client -l }"
    , Bound "bind -N 'Clear the marked pane' M { select-pane -M }"
    , Bound "bind -N 'Enter copy mode' [ { copy-mode }"
    , Bound "bind -N 'Paste the most recent paste buffer' ] { paste-buffer -p }"
    , Bound "bind -N 'Create a new window' c { new-window }"
    , Bound "bind -N 'Detach the current client' d { detach-client }"
    , Unbound "bind -N 'Search for a pane' f { command-prompt { find-window -Z -- '%%' } }" "find-window"
    , Bound "bind -N 'Display window information' i { display-message }"
    , Bound "bind -N 'Select the previously current window' l { last-window }"
    , Bound "bind -N 'Toggle the marked pane' m { select-pane -m }"
    , Bound "bind -N 'Select the next window' n { next-window }"
    , Bound "bind -N 'Select the next pane' o { select-pane -t:.+ }"
    , Bound "bind -N 'Customize options' C { customize-mode -Z }"
    , Bound "bind -N 'Select the previous window' p { previous-window }"
    , Bound "bind -N 'Display pane numbers' q { display-panes }"
    , Bound "bind -N 'Redraw the current client' r { refresh-client }"
    , Bound "bind -N 'Choose a session from a list' s { choose-tree -Zs }"
    , Bound "bind -N 'Show a clock' t { clock-mode }"
    , Unbound "bind -N 'Switch to a window' Tab { new-pane -E -x75% -y30% -X0 -Y0; move-pane -P bottom-centre; switch-mode -wk }" "switch-mode"
    , Unbound "bind -N 'Switch to a session' BTab { new-pane -E -x75% -y30% -X0 -Y0; move-pane -P bottom-centre; switch-mode -sk }" "switch-mode"
    , Bound "bind -N 'Choose a window from a list' w { choose-tree -Zw }"
    , unconfirmed "bind -N 'Kill the active pane' x { kill-pane }"
    , Bound "bind -N 'Zoom the active pane' z { resize-pane -Z }"
    , Bound "bind -N 'Swap the active pane with the pane above' '{' { swap-pane -U }"
    , Bound "bind -N 'Swap the active pane with the pane below' '}' { swap-pane -D }"
    , Bound "bind -N 'Show messages' '~' { show-messages }"
    , Bound "bind -N 'Enter copy mode and scroll up' PPage { copy-mode -u }"
    , Bound "bind -N 'Select the pane above the active pane' -r Up { select-pane -U }"
    , Bound "bind -N 'Select the pane below the active pane' -r Down { select-pane -D }"
    , Bound "bind -N 'Select the pane to the left of the active pane' -r Left { select-pane -L }"
    , Bound "bind -N 'Select the pane to the right of the active pane' -r Right { select-pane -R }"
    , Bound "bind -N 'Set the even-horizontal layout' M-1 { select-layout even-horizontal }"
    , Bound "bind -N 'Set the even-vertical layout' M-2 { select-layout even-vertical }"
    , Bound "bind -N 'Set the main-horizontal layout' M-3 { select-layout main-horizontal }"
    , Bound "bind -N 'Set the main-vertical layout' M-4 { select-layout main-vertical }"
    , Bound "bind -N 'Select the tiled layout' M-5 { select-layout tiled }"
    , Bound "bind -N 'Set the main-horizontal-mirrored layout' M-6 { select-layout main-horizontal-mirrored }"
    , Bound "bind -N 'Set the main-vertical-mirrored layout' M-7 { select-layout main-vertical-mirrored }"
    , Bound "bind -N 'Select the next window with an alert' M-n { next-window -a }"
    , Bound "bind -N 'Rotate through the panes in reverse' M-o { rotate-window -D }"
    , Bound "bind -N 'Select the previous window with an alert' M-p { previous-window -a }"
    , Bound "bind -N 'Move the visible part of the window up' -r S-Up { refresh-client -U 10 }"
    , Bound "bind -N 'Move the visible part of the window down' -r S-Down { refresh-client -D 10 }"
    , Bound "bind -N 'Move the visible part of the window left' -r S-Left { refresh-client -L 10 }"
    , Bound "bind -N 'Move the visible part of the window right' -r S-Right { refresh-client -R 10 }"
    , Bound "bind -N 'Reset so the visible part of the window follows the cursor' -r DC { refresh-client -c }"
    -- tmux wraps the up and left resizes in @if -F '#{?floating_pane_flag}'@,
    -- always true, around @resizep -D-5@ and kin: resize-pane flags taking
    -- an optional value, which hat lacks. These are what they amount to.
    , Bound "bind -N 'Resize the pane up by 5' -r M-Up { resize-pane -U 5 }"
    , Bound "bind -N 'Resize the pane down by 5' -r M-Down { resize-pane -D 5 }"
    , Bound "bind -N 'Resize the pane left by 5' -r M-Left { resize-pane -L 5 }"
    , Bound "bind -N 'Resize the pane right by 5' -r M-Right resize-pane -R 5"
    , Bound "bind -N 'Resize the pane up' -r C-Up { resize-pane -U }"
    , Bound "bind -N 'Resize the pane down' -r C-Down { resize-pane -D }"
    , Bound "bind -N 'Resize the pane left' -r C-Left { resize-pane -L }"
    , Bound "bind -N 'Resize the pane right' -r C-Right { resize-pane -R }"
    -- Hat's own.
    , Bound "bind -N 'Select a window with activity, else the last window' a { activity-window }"
    ]
  where
    -- tmux asks first, with confirm-before, which hat lacks.
    unconfirmed = Bound

-- | The @move@ table, which @g@ in the prefix table switches to: keys that
-- move (and resize) a floating pane.
moveKeys :: [DefaultBinding]
moveKeys = map Bound
    [ "bind -N 'Move a floating pane' g { switch-client -Tmove }"
    , "bind -Tmove -N 'Move pane to top-left corner' 1 { move-pane -P top-left }"
    , "bind -Tmove -N 'Move pane to top-right corner' 2 { move-pane -P top-right }"
    , "bind -Tmove -N 'Move pane to bottom-left corner' 3 { move-pane -P bottom-left }"
    , "bind -Tmove -N 'Move pane to bottom-right corner' 4 { move-pane -P bottom-right }"
    , "bind -Tmove -N 'Move pane to top-left corner and resize' M-1 { resize-pane -x50% -y50%; move-pane -P top-left }"
    , "bind -Tmove -N 'Move pane to top-right corner and resize' M-2 { resize-pane -x50% -y50%; move-pane -P top-right }"
    , "bind -Tmove -N 'Move pane to bottom-left corner and resize' M-3 { resize-pane -x50% -y50%; move-pane -P bottom-left }"
    , "bind -Tmove -N 'Move pane to bottom-right corner and resize' M-4 { resize-pane -x50% -y50%; move-pane -P bottom-right }"
    , "bind -Tmove -N 'Move pane to top' 'Up' { move-pane -P top-centre }"
    , "bind -Tmove -N 'Move pane to bottom' 'Down' { move-pane -P bottom-centre  }"
    , "bind -Tmove -N 'Move pane to left' 'Left' { move-pane -P centre-left }"
    , "bind -Tmove -N 'Move pane to right' 'Right' { move-pane -P centre-right }"
    , "bind -Tmove -N 'Move pane to top and resize' 'M-Up' { resize-pane -x100% -y50%; move-pane -P top-centre }"
    , "bind -Tmove -N 'Move pane to bottom and resize' 'M-Down' { resize-pane -x100% -y50%; move-pane -P bottom-centre  }"
    , "bind -Tmove -N 'Move pane to left and resize' 'M-Left' { resize-pane -x50% -y100%; move-pane -P centre-left }"
    , "bind -Tmove -N 'Move pane to right and resize' 'M-Right' { resize-pane -x50% -y100%; move-pane -P centre-right }"
    , "bind -Tmove -N 'Move pane to fill the window' 0 { resize-pane -x100% -y100%; move-pane -P top-left }"
    , "bind -Tmove -N 'Display move menu' , { if-shell -F '#{pane_floating_flag}' { display-menu -xP -yP -T '#[align=centre]Move' " <> moveMenu <> " } }"
    , "bind -Tmove -N 'Display move and resize menu' . { if-shell -F '#{pane_floating_flag}' { display-menu -xP -yP -T '#[align=centre]Move & Resize' " <> moveResizeMenu <> " } }"
    ]

menuKeys :: [DefaultBinding]
menuKeys = map Bound
    [ "bind -N 'Display window menu' < { display-menu -xW -yW -T '#[align=centre]#{window_index}:#{window_name}' " <> windowMenu <> " }"
    , "bind -N 'Display pane menu' > { display-menu -xP -yP -T '#[align=centre]#{pane_index} (#{pane_id})' " <> paneMenu <> " }"
    ]

rootKeys :: [DefaultBinding]
rootKeys =
    [ Bound "bind -n MouseDown1Pane { select-pane -t=; send-keys -M }"
    , Bound "bind -n MouseDrag1Pane { if-shell -F '#{||:#{pane_in_mode},#{mouse_any_flag}}' { send -M } { copy-mode -M } }"
    , Bound "bind -n C-MouseDrag1Pane { new-pane -M }"
    , Bound "bind -n C-MouseDrag1Empty { new-pane -M }"
    , Bound "bind -n M-MouseDrag1Pane { move-pane -M }"
    , Bound "bind -n WheelUpPane { if-shell -F '#{||:#{alternate_on},#{pane_in_mode},#{mouse_any_flag}}' { send -M } { copy-mode -e } }"
    , Bound "bind -n MouseDown2Pane { select-pane -t=; if-shell -F '#{||:#{pane_in_mode},#{mouse_any_flag}}' { send -M } { paste -p } }"
    , Bound "bind -n DoubleClick1Pane { select-pane -t=; if-shell -F '#{||:#{pane_in_mode},#{mouse_any_flag}}' { send -M } { copy-mode -H; send -X select-word; run -d0.3; send -X copy-pipe-and-cancel } }"
    , Bound "bind -n TripleClick1Pane { select-pane -t=; if-shell -F '#{||:#{pane_in_mode},#{mouse_any_flag}}' { send -M } { copy-mode -H; send -X select-line; run -d0.3; send -X copy-pipe-and-cancel } }"
    , Bound "bind -n MouseDown1Border { select-pane -M }"
    , Bound "bind -n MouseDrag1Border { resize-pane -M }"
    , Bound "bind -n M-MouseDrag1Border { move-pane -M }"
    , Bound "bind -n MouseDown1Status { switch-client -t= }"
    , Bound "bind -n C-MouseDown1Status { swap-window -t@ }"
    , Bound "bind -n MouseDown1Control9 { display-menu -t= -xM -yM -O -T 'Kill pane #{pane_index}?' 'Yes' 'y' { kill-pane -t= } 'No' 'n' {}}"
    , Bound "bind -n MouseDown1Control8 { resize-pane -Z }"
    , Bound "bind -n MouseDown1Control7 { if-shell -Ft= '#{pane_floating_flag}' { join-pane } { break-pane -W } }"
    , Bound "bind -n WheelDownStatus { next-window }"
    , Bound "bind -n WheelUpStatus { previous-window }"
    , Bound ("bind -n MouseDown3StatusLeft { run-shell -C \"display-menu -t= -xM -yW -T '#[align=centre]#{session_name}' " <> sessionMenu <> "\" }")
    , Bound ("bind -n M-MouseDown3StatusLeft { run-shell -C \"display-menu -t= -xM -yW -T '#[align=centre]#{session_name}' " <> sessionMenu <> "\" }")
    , Bound ("bind -n MouseDown3Status { display-menu -t= -xW -yW -T '#[align=centre]#{window_index}:#{window_name}' " <> windowMenu <> "}")
    , Bound ("bind -n M-MouseDown3Status { display-menu -t= -xW -yW -T '#[align=centre]#{window_index}:#{window_name}' " <> windowMenu <> "}")
    , Bound ("bind -n MouseDown3Pane { if-shell -Ft= '#{||:#{mouse_any_flag},#{&&:#{pane_in_mode},#{?#{m/r:(copy|view)-mode,#{pane_mode}},0,1}}}' { select-pane -t=; send -M } { display-menu -t= -xM -yM -T '#[align=centre]#{pane_index} (#{pane_id})' " <> paneMenu <> " } }")
    , Bound ("bind -n M-MouseDown3Pane { display-menu -t= -xM -yM -T '#[align=centre]#{pane_index} (#{pane_id})' " <> paneMenu <> " }")
    , Bound ("bind -n MouseDown3Empty { display-menu -t= -xM -yM -T '#[align=centre]#{window_index}:#{window_name}' " <> emptyMenu <> " }")
    , Bound ("bind -n M-MouseDown3Empty { display-menu -t= -xM -yM -T '#[align=centre]#{window_index}:#{window_name}' " <> emptyMenu <> " }")
    , Unbound "bind -n MouseDown1ScrollbarUp { if -Ft= '#{pane_in_mode}' { send -X page-up } {copy-mode -u } }" noScrollbarKeys
    , Unbound "bind -n MouseDown1ScrollbarDown { if -Ft= '#{pane_in_mode}' { send -X page-down } {copy-mode -d } }" noScrollbarKeys
    , Unbound "bind -n MouseDrag1ScrollbarSlider { if -Ft= '#{pane_in_mode}' { send -X scroll-to-mouse } { copy-mode -S } }" noScrollbarKeys
    ]
  where
    noScrollbarKeys = "scrollbar mouse keys, scroll-to-mouse"

-- | The copy-mode tables' mouse keys, the same in both.
copyMouseKeys :: Text -> [DefaultBinding]
copyMouseKeys table = map (Bound . (("bind -T" <> table <> " ") <>))
    [ "MouseDown1Pane select-pane"
    , "MouseDrag1Pane { select-pane; send-keys -X begin-selection }"
    , "MouseDragEnd1Pane { send-keys -X copy-pipe-and-cancel }"
    , "WheelUpPane { select-pane; send-keys -N5 -X scroll-up }"
    , "WheelDownPane { select-pane; send-keys -N5 -X scroll-down }"
    , "DoubleClick1Pane { select-pane; send-keys -X select-word; run-shell -d0.3; send-keys -X copy-pipe-and-cancel }"
    , "TripleClick1Pane { select-pane; send-keys -X select-line; run-shell -d0.3; send-keys -X copy-pipe-and-cancel }"
    ]

-- | The emacs copy-mode table, @copy-mode@.
copyKeys :: [DefaultBinding]
copyKeys = map copyMode
    [ "C-Space { send-keys -X begin-selection }"
    , "C-a { send-keys -X start-of-line }"
    , "C-c { send-keys -X cancel }"
    , "C-e { send-keys -X end-of-line }"
    , "C-f { send-keys -X cursor-right }"
    , "C-b { send-keys -X cursor-left }"
    , "C-g { send-keys -X clear-selection }"
    ]
    <> [ lacking "C-k { send -X copy-pipe-end-of-line-and-cancel }" "copy-pipe-end-of-line-and-cancel" ]
    <> map copyMode [ "C-l { send-keys -X recentre-top-bottom }" ]
    <> [ lacking "M-l { send -X cursor-centre-horizontal }" "cursor-centre-horizontal" ]
    <> map copyMode
    [ "C-n { send-keys -X cursor-down }"
    , "C-p { send-keys -X cursor-up }"
    ]
    <>
    [ lacking "C-r { command-prompt -P -T search -ip'(search up)' -I'#{pane_search_string}' { send -X search-backward-incremental -- '%%' } }" incremental
    , lacking "C-s { command-prompt -P -T search -ip'(search down)' -I'#{pane_search_string}' { send -X search-forward-incremental -- '%%' } }" incremental
    ]
    <> map copyMode
    [ "C-v { send-keys -X page-down }"
    , "C-w { send-keys -X copy-pipe-and-cancel }"
    , "Escape { send-keys -X cancel }"
    , "C-[ { send-keys -X cancel }"
    -- tmux pages down; hat begins a selection, as vi's Space does.
    , "Space { send-keys -X begin-selection }"
    , ", { send-keys -X jump-reverse }"
    , "\\; { send-keys -X jump-again }"
    ]
    <> jumpKeys "copy-mode"
    <> map copyMode
    [ "L { send-keys -X line-numbers-toggle }"
    , "N { send-keys -X search-reverse }"
    , "P { send-keys -X toggle-position }"
    , "R { send-keys -X rectangle-toggle }"
    , "X { send-keys -X set-mark }"
    ]
    <> [ lacking "g { command-prompt -P -p'(goto line)' { send -X goto-line -- '%%' } }" "goto-line" ]
    <> map copyMode
    [ "n { send-keys -X search-again }"
    , "q { send-keys -X cancel }"
    ]
    <> [ lacking "r { send -X refresh-toggle }" "refresh-toggle" ]
    <> map copyMode
    [ "Home { send-keys -X start-of-line }"
    , "End { send-keys -X end-of-line }"
    ]
    <> copyMouseKeys "copy-mode"
    <> map copyMode
    [ "NPage { send-keys -X page-down }"
    , "PPage { send-keys -X page-up }"
    , "Up { send-keys -X cursor-up }"
    , "Down { send-keys -X cursor-down }"
    , "Left { send-keys -X cursor-left }"
    , "Right { send-keys -X cursor-right }"
    ]
    <> [ lacking ("M-" <> i <> " { command-prompt -P -Np'(repeat)' -I" <> i <> " { send -N '%%' } }")
            "send-keys -N without keys"
       | i <- map tshow [1 .. 9 :: Int] ]
    <> map copyMode
    [ "M-< { send-keys -X history-top }"
    , "M-> { send-keys -X history-bottom }"
    ]
    <> [ lacking "M-R { send -X top-line }" "a name for M-R apart from M-r" ]
    <> map copyMode [ "M-b { send-keys -X previous-word }" ]
    <> [ lacking "C-M-b { send -X previous-matching-bracket }" "previous-matching-bracket" ]
    <> map copyMode [ "M-f { send-keys -X next-word-end }" ]
    <> [ lacking "C-M-f { send -X next-matching-bracket }" "next-matching-bracket" ]
    <> map copyMode
    [ "M-m { send-keys -X back-to-indentation }"
    , "M-r { send-keys -X middle-line }"
    , "M-v { send-keys -X page-up }"
    , "M-w { send-keys -X copy-pipe-and-cancel }"
    , "M-x { send-keys -X jump-to-mark }"
    , "'M-{' { send-keys -X previous-paragraph }"
    , "'M-}' { send-keys -X next-paragraph }"
    , "M-Up { send-keys -X halfpage-up }"
    , "M-Down { send-keys -X halfpage-down }"
    , "C-Up { send-keys -X scroll-up }"
    , "C-Down { send-keys -X scroll-down }"
    ]
    <>
    [ lacking "M-C-Up { send -X previous-prompt }" "previous-prompt"
    , lacking "M-C-Down { send -X next-prompt }" "next-prompt"
    -- Hat's own.
    , copyMode "Enter { send-keys -X copy-selection-and-cancel }"
    ]
  where
    copyMode = Bound . ("bind -Tcopy-mode " <>)
    lacking line = Unbound ("bind -Tcopy-mode " <> line)
    incremental = "search-backward-incremental, search-forward-incremental"

-- | The vi copy-mode table, @copy-mode-vi@.
copyViKeys :: [DefaultBinding]
copyViKeys =
    [ lacking "'#' { send -FX search-backward -- '#{copy_cursor_word}' }" "send-keys -F"
    , lacking "* { send -FX search-forward -- '#{copy_cursor_word}' }" "send-keys -F"
    ]
    <> map vi
    [ "C-c { send-keys -X cancel }"
    , "C-d { send-keys -X halfpage-down }"
    , "C-e { send-keys -X scroll-down }"
    , "C-b { send-keys -X page-up }"
    , "C-f { send-keys -X page-down }"
    , "C-h { send-keys -X cursor-left }"
    , "C-j { send-keys -X copy-pipe-and-cancel }"
    , "Enter { send-keys -X copy-pipe-and-cancel }"
    , "C-u { send-keys -X halfpage-up }"
    , "C-v { send-keys -X rectangle-toggle }"
    , "C-y { send-keys -X scroll-up }"
    , "Escape { send-keys -X clear-selection }"
    , "C-[ { send-keys -X clear-selection }"
    , "Space { send-keys -X begin-selection }"
    , "'$' { send-keys -X end-of-line }"
    , ", { send-keys -X jump-reverse }"
    -- tmux's search prompts, here and at ?, end their flags with @--@, which
    -- hat's copy-mode commands would read as part of the query.
    , "/ { command-prompt -P -T search -p'(search down)' { send-keys -X search-forward '%%' } }"
    ]
    -- tmux's digits open a @(repeat)@ prompt for @send -N@; hat's build the
    -- count in place, and a bare 0 is start-of-line.
    <> [ vi (i <> " { send-keys -X digit " <> i <> " }") | i <- map tshow [0 .. 9 :: Int] ]
    <> [ lacking ": { command-prompt -P -p'(goto line)' { send -X goto-line -- '%%' } }" "goto-line" ]
    <> map vi
    [ "\\; { send-keys -X jump-again }"
    , "? { command-prompt -P -T search -p'(search up)' { send-keys -X search-backward '%%' } }"
    ]
    <> [ lacking "A { send -X append-selection-and-cancel }" "append-selection-and-cancel" ]
    <> map vi [ "B { send-keys -X previous-space }" ]
    <> [ lacking "D { send -X copy-pipe-end-of-line-and-cancel }" "copy-pipe-end-of-line-and-cancel" ]
    <> map vi [ "E { send-keys -X next-space-end }" ]
    <> jumpKeys "copy-mode-vi"
    <> map vi
    [ "G { send-keys -X history-bottom }"
    , "H { send-keys -X top-line }"
    , "J { send-keys -X scroll-down }"
    , "K { send-keys -X scroll-up }"
    , "L { send-keys -X bottom-line }"
    , "M { send-keys -X middle-line }"
    , "N { send-keys -X search-reverse }"
    , "P { send-keys -X toggle-position }"
    , "V { send-keys -X select-line }"
    , "W { send-keys -X next-space }"
    , "X { send-keys -X set-mark }"
    , "^ { send-keys -X back-to-indentation }"
    , "b { send-keys -X previous-word }"
    , "e { send-keys -X next-word-end }"
    , "g { send-keys -X history-top }"
    , "h { send-keys -X cursor-left }"
    , "j { send-keys -X cursor-down }"
    , "k { send-keys -X cursor-up }"
    , "z { send-keys -X scroll-middle }"
    , "l { send-keys -X cursor-right }"
    , "n { send-keys -X search-again }"
    , "o { send-keys -X other-end }"
    , "q { send-keys -X cancel }"
    ]
    <> [ lacking "r { send -X refresh-toggle }" "refresh-toggle" ]
    <> map vi
    -- tmux toggles a rectangle; hat begins a selection, as vim's v does.
    [ "v { send-keys -X begin-selection }"
    , "w { send-keys -X next-word }"
    , "'{' { send-keys -X previous-paragraph }"
    , "'}' { send-keys -X next-paragraph }"
    ]
    <> [ lacking "% { send -X next-matching-bracket }" "next-matching-bracket" ]
    <> map vi
    [ "Home { send-keys -X start-of-line }"
    , "End { send-keys -X end-of-line }"
    ]
    <> copyMouseKeys "copy-mode-vi"
    <> map vi
    [ "BSpace { send-keys -X cursor-left }"
    , "NPage { send-keys -X page-down }"
    , "PPage { send-keys -X page-up }"
    , "Up { send-keys -X cursor-up }"
    , "Down { send-keys -X cursor-down }"
    , "Left { send-keys -X cursor-left }"
    , "Right { send-keys -X cursor-right }"
    , "M-x { send-keys -X jump-to-mark }"
    , "C-Up { send-keys -X scroll-up }"
    , "C-Down { send-keys -X scroll-down }"
    -- Hat's own.
    , "y { send-keys -X copy-selection-and-cancel }"
    ]
  where
    vi = Bound . ("bind -Tcopy-mode-vi " <>)
    lacking line = Unbound ("bind -Tcopy-mode-vi " <> line)

-- | The jump keys: tmux's prompt for the target character with
-- @command-prompt -1@; hat's commands take the next key as it.
jumpKeys :: Text -> [DefaultBinding]
jumpKeys table = map (Bound . (("bind -T" <> table <> " ") <>))
    [ "F { send-keys -X jump-backward }"
    , "T { send-keys -X jump-to-backward }"
    , "f { send-keys -X jump-forward }"
    , "t { send-keys -X jump-to-forward }"
    ]

-- tmux's DEFAULT_SESSION_MENU and its kin (@key-bindings.c@).
sessionMenu, windowMenu, emptyMenu, paneMenu, moveMenu, moveResizeMenu :: Text
sessionMenu = T.concat
    [ " #{S/t:#{?#{&&:#{<:#{loop_index},6},#{!:#{session_active}}},'Switch To #[underscore]#{session_name}' '' {switch-client -t=#{session_id}#} ,}}"
    , " ''"
    , " 'Renumber' 'N' {move-window -r}"
    , " 'Rename' 'r' {command-prompt -I '#S' {rename-session -- '%%'}}"
    , " 'Detach' 'd' {detach-client}"
    , " ''"
    , " 'New Session' 's' {new-session}"
    , " 'New Window' 'w' {new-window}" ]
windowMenu = T.concat
    [ " '#{?#{>:#{session_windows},1},,-}Swap Left' 'l' {swap-window -t:-1}"
    , " '#{?#{>:#{session_windows},1},,-}Swap Right' 'r' {swap-window -t:+1}"
    , " '#{?pane_marked_set,,-}Swap Marked' 's' {swap-window}"
    , " ''"
    , " 'Kill' 'X' {kill-window}"
    , " 'Respawn' 'R' {respawn-window -k}"
    , " '#{?pane_marked,Unmark,Mark}' 'm' {select-pane -m}"
    , " 'Rename' 'n' {command-prompt -FI \"#W\" {rename-window -t '#{window_id}' -- '%%'}}"
    , " ''"
    , " 'New After' 'w' {new-window -a}"
    , " 'New At End' 'W' {new-window}" ]
emptyMenu = T.concat
    [ " 'New Pane' 'p' {new-pane; join-pane}"
    , " 'New Window' 'w' {new-window}" ]
paneMenu = T.concat
    [ " '#{?#{m/r:(copy|view)-mode,#{pane_mode}},Go To Top,}' '<' {send -X history-top}"
    , " '#{?#{m/r:(copy|view)-mode,#{pane_mode}},Go To Bottom,}' '>' {send -X history-bottom}"
    , " ''"
    , " '#{?#{==:#{pane_mode},copy-mode},#{?copy_line_numbers,Hide Line Numbers,Show Line Numbers},}' 'L' {send -X line-numbers-toggle}"
    , " '#{?#{==:#{pane_mode},copy-mode},#{?refresh_active,Refresh Off,Refresh On},}' 'r' {send -X refresh-toggle}"
    , " ''"
    , " '#{?#{&&:#{buffer_size},#{!:#{pane_in_mode}}},Paste #[underscore]#{=/9/...:buffer_sample},}' 'p' {paste-buffer}"
    , " ''"
    , " '#{?mouse_word,Search For #[underscore]#{=/9/...:mouse_word},}' 'C-r' {if -F '#{?#{m/r:(copy|view)-mode,#{pane_mode}},0,1}' 'copy-mode -t='; send -Xt= search-backward -- \"#{q:mouse_word}\"}"
    , " '#{?mouse_word,Type #[underscore]#{=/9/...:mouse_word},}' 'C-y' {copy-mode -q; send-keys -l -- \"#{q:mouse_word}\"}"
    , " '#{?mouse_word,Copy #[underscore]#{=/9/...:mouse_word},}' 'c' {copy-mode -q; set-buffer -- \"#{q:mouse_word}\"}"
    , " '#{?mouse_line,Copy Line,}' 'l' {copy-mode -q; set-buffer -- \"#{q:mouse_line}\"}"
    , " ''"
    , " '#{?mouse_hyperlink,Type #[underscore]#{=/9/...:mouse_hyperlink},}' 'C-h' {copy-mode -q; send-keys -l -- \"#{q:mouse_hyperlink}\"}"
    , " '#{?mouse_hyperlink,Copy #[underscore]#{=/9/...:mouse_hyperlink},}' 'h' {copy-mode -q; set-buffer -- \"#{q:mouse_hyperlink}\"}"
    , " ''"
    , " '#{?#{#{pane_floating_flag}},Move,}' '' {display-menu -xL -yL -T '#[align=centre]Move' " <> moveMenu <> " }"
    , " '#{?#{#{pane_floating_flag}},Move & Resize,}' '' {display-menu -xL -yL -T '#[align=centre]Move & Resize' " <> moveResizeMenu <> " }"
    , " '#{?#{#{pane_floating_flag}},Tile,}' 't' { join-pane }"
    , " '#{?#{!:#{pane_floating_flag}},Float,}' 'f' { break-pane -W }"
    , " '#{?#{!:#{pane_floating_flag}},Horizontal Split,}' 'h' {split-window -h}"
    , " '#{?#{!:#{pane_floating_flag}},Vertical Split,}' 'v' {split-window -v}"
    , " ''"
    , " '#{?#{&&:#{!:#{pane_floating_flag}},#{>:#{window_panes},1}},Swap Up,}' 'u' {swap-pane -U}"
    , " '#{?#{&&:#{!:#{pane_floating_flag}},#{>:#{window_panes},1}},Swap Down,}' 'd' {swap-pane -D}"
    , " '#{?pane_marked_set,,-}Swap Marked' 's' {swap-pane}"
    , " ''"
    , " 'Kill' 'X' {kill-pane}"
    , " 'Respawn' 'R' {respawn-pane -k}"
    , " '#{?pane_marked,Unmark,Mark}' 'm' {select-pane -m}"
    , " '#{?#{>:#{window_panes},1},,-}#{?window_zoomed_flag,Unzoom,Zoom}' 'z' {resize-pane -Z}" ]
moveMenu = T.concat
    [ " 'Centre' 'c' {move-pane -P centre}"
    , " ''"
    , " 'Top Left' '1' {move-pane -P top-left}"
    , " 'Top Right' '2' {move-pane -P top-right}"
    , " 'Bottom Left' '3' {move-pane -P bottom-left}"
    , " 'Bottom Right' '4' {move-pane -P bottom-right}"
    , " ''"
    , " 'Top' 't' {move-pane -P top-centre}"
    , " 'Bottom' 'b' {move-pane -P bottom-centre}"
    , " 'Left' 'l' {move-pane -P centre-left}"
    , " 'Right' 'r' {move-pane -P centre-right}" ]
moveResizeMenu = T.concat
    [ " 'Fill' '0' {resize-pane -x100% -y100%; move-pane -P top-left}"
    , " ''"
    , " 'Top Left' '1' {resize-pane -x50% -y50%; move-pane -P top-left}"
    , " 'Top Right' '2' {resize-pane -x50% -y50%; move-pane -P top-right}"
    , " 'Bottom Left' '3' {resize-pane -x50% -y50%; move-pane -P bottom-left}"
    , " 'Bottom Right' '4' {resize-pane -x50% -y50%; move-pane -P bottom-right}"
    , " ''"
    , " 'Top' 't' {resize-pane -x100% -y50%; move-pane -P top-centre}"
    , " 'Bottom' 'b' {resize-pane -x100% -y50%; move-pane -P bottom-centre}"
    , " 'Left' 'l' {resize-pane -x50% -y100%; move-pane -P centre-left}"
    , " 'Right' 'r' {resize-pane -x50% -y100%; move-pane -P centre-right}" ]
