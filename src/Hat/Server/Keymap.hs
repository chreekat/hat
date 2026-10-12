-- | The built-in key bindings: what a server starts with before any config
-- runs. Every binding is a command argv, so the key tables and a user's
-- @bind-key@ speak exactly the same language.
module Hat.Server.Keymap
    ( defaultKeymap
    , menuBindings
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Parser (parseCommandLine)
import Hat.Model (tshow)
import Hat.Server.Keys (Binding (..), Keymap, Repeat (..))

defaultKeymap :: Keymap
defaultKeymap = Map.fromList
    [ ("prefix", Map.fromList (map bindArgv prefixBindings <> map repeatArgv repeatBindings
        <> menusIn "prefix"))
    , ("root", Map.fromList (map once rootMouseBindings <> menusIn "root"))
    , ("copy-mode", Map.fromList (map copyBind copyModeBindings <> map once copyMouseBindings))
    , ("copy-mode-vi", Map.fromList (map copyBind copyModeViBindings
        <> map once (digitBinds <> searchBinds <> copyMouseBindings)))
    ]
  where
    menusIn table =
        [ (k, Binding { commands = parsed cmd, repeats = Once
                      , note = if table == "prefix" then lookup k prefixNotes else Nothing })
        | (t, k, cmd) <- menuBindings, t == table ]
    parsed cmd = either (error . T.unpack) id (parseCommandLine cmd)
    -- tmux's default mouse bindings, less those whose commands Hat lacks
    -- (the scrollbars); those opening menus are 'menuBindings'.
    inModeOrTracking = "#{||:#{pane_in_mode},#{mouse_any_flag}}"
    clickCopy sel =
        "copy-mode -H; send-keys -X " <> sel
            <> "; run-shell -d 0.3; send-keys -X copy-pipe-and-cancel"
    rootMouseBindings =
        [ ("MouseDown1Pane", [["select-pane", "-t", "="], ["send-keys", "-M"]])
        , ("MouseDrag1Pane", [["if-shell", "-F", inModeOrTracking, "send-keys -M", "copy-mode -M"]])
        , ("C-MouseDrag1Pane", [["new-pane", "-M"]])
        , ("C-MouseDrag1Empty", [["new-pane", "-M"]])
        , ("M-MouseDrag1Pane", [["move-pane", "-M"]])
        , ("WheelUpPane", [["if-shell", "-F"
            , "#{||:#{alternate_on},#{pane_in_mode},#{mouse_any_flag}}"
            , "send-keys -M", "copy-mode -e"]])
        , ("MouseDown2Pane", [["select-pane", "-t", "="]
            , ["if-shell", "-F", inModeOrTracking, "send-keys -M", "paste-buffer -p"]])
        , ("DoubleClick1Pane", [["select-pane", "-t", "="]
            , ["if-shell", "-F", inModeOrTracking, "send-keys -M", clickCopy "select-word"]])
        , ("TripleClick1Pane", [["select-pane", "-t", "="]
            , ["if-shell", "-F", inModeOrTracking, "send-keys -M", clickCopy "select-line"]])
        , ("MouseDown1Border", [["select-pane", "-M"]])
        , ("MouseDrag1Border", [["resize-pane", "-M"]])
        , ("M-MouseDrag1Border", [["move-pane", "-M"]])
        , ("MouseDown1Status", [["switch-client", "-t", "="]])
        , ("MouseDown1Control8", [["resize-pane", "-Z"]])
        , ("MouseDown1Control7", [["if-shell", "-F", "-t", "=", "#{pane_floating_flag}", "join-pane", "break-pane -W"]])
        , ("WheelDownStatus", [["next-window"]])
        , ("WheelUpStatus", [["previous-window"]])
        ]
    copyMouseBindings =
        [ ("MouseDown1Pane", [["select-pane"]])
        , ("MouseDrag1Pane", [["select-pane"], ["send-keys", "-X", "begin-selection"]])
        , ("MouseDragEnd1Pane", [["send-keys", "-X", "copy-pipe-and-cancel"]])
        , ("WheelUpPane", [["select-pane"], ["send-keys", "-N", "5", "-X", "scroll-up"]])
        , ("WheelDownPane", [["select-pane"], ["send-keys", "-N", "5", "-X", "scroll-down"]])
        , ("DoubleClick1Pane", [["select-pane"], ["send-keys", "-X", "select-word"]
            , ["run-shell", "-d", "0.3"], ["send-keys", "-X", "copy-pipe-and-cancel"]])
        , ("TripleClick1Pane", [["select-pane"], ["send-keys", "-X", "select-line"]
            , ["run-shell", "-d", "0.3"], ["send-keys", "-X", "copy-pipe-and-cancel"]])
        ]
    bindArgv (k, cmd) =
        (k, Binding { commands = [cmd], repeats = Once, note = lookup k prefixNotes })
    -- @bind -r@: these keep the prefix table open for @repeat-time@.
    repeatArgv (k, cmd) =
        (k, Binding { commands = [cmd], repeats = Repeats, note = lookup k prefixNotes })
    once (k, cmds) = (k, Binding { commands = cmds, repeats = Once, note = Nothing })
    -- A copy-mode key runs a single @send-keys -X <name>@ command.
    copyBind (k, name) = once (k, [["send-keys", "-X", name]])
    -- Digit keys feed the vi @[count]@ prefix; a bare @0@ is start-of-line.
    digitBinds =
        [ (tshow d, [["send-keys", "-X", "digit", tshow d]]) | d <- [0 .. 9 :: Int] ]
    -- @/@ and @?@ open the command prompt to collect a search query, then
    -- run the search on submit (the @%%@ splice carries the typed line).
    searchBinds =
        [ ("/", [["command-prompt", "-p", "(search down)"
                 , "send-keys -X search-forward '%%'"]])
        , ("?", [["command-prompt", "-p", "(search up)"
                 , "send-keys -X search-backward '%%'"]])
        ]
    prefixBindings =
        [ ("a", ["activity-window"])
        , ("d", ["detach-client"])
        , ("c", ["new-window"])
        , ("w", ["choose-tree", "-Zw"])
        , ("%", ["split-window", "-h"])
        , ("\"", ["split-window", "-v"])
        , ("x", ["kill-pane"])
        , ("&", ["kill-window"])
        , ("!", ["break-pane"])
        , ("{", ["swap-pane", "-U"])
        , ("}", ["swap-pane", "-D"])
        , (",", ["command-prompt", "-I", "#W", "rename-window '%%'"])
        , (".", ["command-prompt", "-p", "(index)", "move-window -t '%%'"])
        , ("$", ["command-prompt", "-I", "#S", "rename-session '%%'"])
        , ("z", ["resize-pane", "-Z"])
        , ("Space", ["next-layout"])
        , ("M-1", ["select-layout", "even-horizontal"])
        , ("M-2", ["select-layout", "even-vertical"])
        , ("M-3", ["select-layout", "main-horizontal"])
        , ("M-4", ["select-layout", "main-vertical"])
        , ("M-5", ["select-layout", "tiled"])
        , ("q", ["display-panes"])
        , ("o", ["select-pane", "-t", ":.+"])
        , ("O", ["select-pane", "-t", ":.-"])
        , ("n", ["next-window"])
        , ("p", ["previous-window"])
        , ("l", ["last-window"])
        , (";", ["last-pane"])
        , ("C-b", ["send-prefix"])
        , ("[", ["copy-mode"])
        , ("]", ["paste-buffer"])
        , (":", ["command-prompt"])
        ]
        <> [ (tshow i, ["select-window", "-t", tshow (i :: Int)])
           | i <- [0 .. 9]
           ]
    repeatBindings =
        [ ("Left", ["select-pane", "-L"])
        , ("Right", ["select-pane", "-R"])
        , ("Up", ["select-pane", "-U"])
        , ("Down", ["select-pane", "-D"])
        , ("C-Left", ["resize-pane", "-L"])
        , ("C-Right", ["resize-pane", "-R"])
        , ("C-Up", ["resize-pane", "-U"])
        , ("C-Down", ["resize-pane", "-D"])
        ]
    copyModeViBindings =
        [ ("h", "cursor-left"), ("j", "cursor-down")
        , ("k", "cursor-up"), ("l", "cursor-right")
        , ("$", "end-of-line")
        , ("w", "next-word"), ("b", "previous-word"), ("e", "next-word-end")
        , ("W", "next-space"), ("B", "previous-space"), ("E", "next-space-end")
        , ("^", "back-to-indentation")
        , ("{", "previous-paragraph"), ("}", "next-paragraph")
        , ("f", "jump-forward"), ("F", "jump-backward")
        , ("t", "jump-to-forward"), ("T", "jump-to-backward")
        , (";", "jump-again"), (",", "jump-reverse")
        , ("n", "search-again"), ("N", "search-reverse")
        , ("g", "history-top"), ("G", "history-bottom")
        , ("H", "top-line"), ("M", "middle-line"), ("L", "bottom-line")
        , ("C-f", "page-down"), ("PgDn", "page-down")
        , ("C-b", "page-up"), ("PgUp", "page-up")
        , ("C-d", "halfpage-down"), ("C-u", "halfpage-up")
        , ("v", "begin-selection"), ("Space", "begin-selection")
        , ("V", "select-line"), ("C-v", "rectangle-toggle")
        , ("o", "other-end"), ("Escape", "clear-selection")
        , ("y", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]
    copyModeBindings =
        [ ("C-b", "cursor-left"), ("C-f", "cursor-right")
        , ("C-p", "cursor-up"), ("C-n", "cursor-down")
        , ("C-a", "start-of-line"), ("C-e", "end-of-line")
        , ("M-f", "next-word-end"), ("M-b", "previous-word")
        , ("M-<", "history-top")
        , ("Space", "begin-selection"), ("C-g", "clear-selection")
        , ("R", "rectangle-toggle")
        , ("M-w", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel"), ("Escape", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]

-- | tmux's notes on its default prefix bindings (@key-bindings.c@), for the
-- keys Hat binds to the same action.
prefixNotes :: [(Text, Text)]
prefixNotes =
    [ ("C-b", "Send the prefix key")
    , ("Space", "Select next layout")
    , ("!", "Break pane to a new window")
    , ("\"", "Split window vertically")
    , ("$", "Rename current session")
    , ("%", "Split window horizontally")
    , ("&", "Kill current window")
    , (",", "Rename current window")
    , (".", "Move the current window")
    , (":", "Prompt for a command")
    , (";", "Move to the previously active pane")
    , ("[", "Enter copy mode")
    , ("]", "Paste the most recent paste buffer")
    , ("c", "Create a new window")
    , ("d", "Detach the current client")
    , ("l", "Select the previously current window")
    , ("n", "Select the next window")
    , ("o", "Select the next pane")
    , ("p", "Select the previous window")
    , ("q", "Display pane numbers")
    , ("w", "Choose a window from a list")
    , ("x", "Kill the active pane")
    , ("z", "Zoom the active pane")
    , ("{", "Swap the active pane with the pane above")
    , ("}", "Swap the active pane with the pane below")
    , ("<", "Display window menu")
    , (">", "Display pane menu")
    , ("Up", "Select the pane above the active pane")
    , ("Down", "Select the pane below the active pane")
    , ("Left", "Select the pane to the left of the active pane")
    , ("Right", "Select the pane to the right of the active pane")
    , ("M-1", "Set the even-horizontal layout")
    , ("M-2", "Set the even-vertical layout")
    , ("M-3", "Set the main-horizontal layout")
    , ("M-4", "Set the main-vertical layout")
    , ("M-5", "Select the tiled layout")
    , ("C-Up", "Resize the pane up")
    , ("C-Down", "Resize the pane down")
    , ("C-Left", "Resize the pane left")
    , ("C-Right", "Resize the pane right")
    ]
    <> [ (tshow i, "Select window " <> tshow i) | i <- [0 .. 9 :: Int] ]

-- | tmux's default bindings that open menus, by table and key, in its own
-- command syntax.
menuBindings :: [(Text, Text, Text)]
menuBindings =
    [ ("prefix", "<", "display-menu -xW -yW -T '#[align=centre]#{window_index}:#{window_name}' "
        <> windowMenu)
    , ("prefix", ">", "display-menu -xP -yP -T '#[align=centre]#{pane_index} (#{pane_id})' "
        <> paneMenu)
    , ("root", "MouseDown1Control9", "display-menu -t= -xM -yM -O -T 'Kill pane #{pane_index}?' "
        <> "'Yes' 'y' { kill-pane -t= } 'No' 'n' {}")
    , ("root", "MouseDown3StatusLeft", sessionMenuCommand)
    , ("root", "M-MouseDown3StatusLeft", sessionMenuCommand)
    , ("root", "MouseDown3Status", windowMenuCommand)
    , ("root", "M-MouseDown3Status", windowMenuCommand)
    , ("root", "MouseDown3Pane", "if -Ft= '#{||:#{mouse_any_flag},#{&&:#{pane_in_mode},"
        <> "#{?#{m/r:(copy|view)-mode,#{pane_mode}},0,1}}}' { select-pane -t=; send -M } { "
        <> paneMenuCommand <> " }")
    , ("root", "M-MouseDown3Pane", paneMenuCommand)
    , ("root", "MouseDown3Empty", emptyMenuCommand)
    , ("root", "M-MouseDown3Empty", emptyMenuCommand)
    ]
  where
    sessionMenuCommand = "run -C \"display-menu -t= -xM -yW -T '#[align=centre]#{session_name}' "
        <> sessionMenu <> "\""
    windowMenuCommand = "display-menu -t= -xW -yW -T '#[align=centre]#{window_index}:#{window_name}' "
        <> windowMenu
    paneMenuCommand = "display-menu -t= -xM -yM -T '#[align=centre]#{pane_index} (#{pane_id})' "
        <> paneMenu
    emptyMenuCommand = "display-menu -t= -xM -yM -T '#[align=centre]#{window_index}:#{window_name}' "
        <> emptyMenu

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
