-- | The built-in key bindings: what a server starts with before any config
-- runs. Every binding is a command argv, so the key tables and a user's
-- @bind-key@ speak exactly the same language.
module Hat.Server.Keymap
    ( defaultKeymap
    ) where

import Data.Map.Strict qualified as Map

import Hat.Model (tshow)
import Hat.Server.Keys (Binding (..), Keymap, Repeat (..))

defaultKeymap :: Keymap
defaultKeymap = Map.fromList
    [ ("prefix", Map.fromList (map bindArgv prefixBindings <> map repeatArgv repeatBindings))
    , ("root", Map.fromList (map once rootMouseBindings))
    , ("copy-mode", Map.fromList (map copyBind copyModeBindings <> map once copyMouseBindings))
    , ("copy-mode-vi", Map.fromList (map copyBind copyModeViBindings
        <> map once (digitBinds <> searchBinds <> copyMouseBindings)))
    ]
  where
    -- tmux's default mouse bindings, less those whose commands Hat lacks
    -- (display-menu, new-pane, move-pane -M, the scrollbars).
    inModeOrTracking = "#{||:#{pane_in_mode},#{mouse_any_flag}}"
    clickCopy sel =
        "copy-mode -H; send-keys -X " <> sel
            <> "; run-shell -d 0.3; send-keys -X copy-pipe-and-cancel"
    rootMouseBindings =
        [ ("MouseDown1Pane", [["select-pane", "-t", "="], ["send-keys", "-M"]])
        , ("MouseDrag1Pane", [["if-shell", "-F", inModeOrTracking, "send-keys -M", "copy-mode -M"]])
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
        , ("MouseDown1Status", [["switch-client", "-t", "="]])
        , ("MouseDown1Control8", [["resize-pane", "-Z"]])
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
    bindArgv (k, cmd) = (k, Binding { commands = [cmd], repeats = Once })
    -- @bind -r@: these keep the prefix table open for @repeat-time@.
    repeatArgv (k, cmd) = (k, Binding { commands = [cmd], repeats = Repeats })
    once (k, cmds) = (k, Binding { commands = cmds, repeats = Once })
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
