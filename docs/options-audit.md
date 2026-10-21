# Options behavior audit

A living catalog of every option Hat accepts, tracking whether the stored value
*actually drives the behavior tmux gives it* — not merely that it parses and
lands in an `Options` field. Built so we never re-derive "which option is
unimplemented / easiest to pick off next": read this table, pick a row, fix it.

## What the verdicts mean

- **implemented** — a consumer reads the field and produces the tmux behavior
  the value controls, pinned by a behavior test (the ✅ *tested* mark).
- **defect: no consumer** — the field is stored and read by nothing. Setting it
  does nothing.
- **defect: wrong semantics** — a consumer reads it, but not the way tmux means
  it, so the observable effect is wrong.

The **seam** column is how a row is behavior-verified: `pure` = an exported
pure function takes the value, tested prefix-style in `OptionEffectSpec`;
`integration` = exercised end-to-end in `IntegrationSpec`; `rendered` = a format
string the FormatSpec-verified `renderFormat` expands.

Which options exist and which hat implements is not tracked here: that is
the registry's gate (`live`/`notYet`/`unsupported` rows in
`src/Hat/Model/OptionTable.hs`), which `set`/`show` enforce. This catalog
covers the live rows; "Left out of hat" lists the `unsupported` ones.

## Method

Each `Options` field (`src/Hat/Model/Options.hs`) was traced to its read-sites
across `src/` and `app/`. The `Hat.Server` `show-options` echo block and the
`set -a` append block are *not* behavior — they only reflect or re-parse the
value — so they are excluded. Verdicts below are from that consumer trace plus a
spot-read of each suspect consumer.

## Catalog

| Option | Consumer | Verdict | Seam |
|---|---|---|---|
| `prefix` / `prefix2` | `keyContext` → `routeKey` (Conn.hs, Keys.hs); `send-prefix [-2]`; `None` is no key | implemented ✅ tested | pure (KeysSpec) |
| `key-table` | `keyContext` → `routeKey`: the client's default table | implemented ✅ tested | pure (KeysSpec) |
| `repeat-time` / `initial-repeat-time` | `routeKey` (a `bind -r` binding keeps its table open), `expireKeyRepeat` timer in `inputLoop` (Conn.hs) | implemented ✅ tested | pure (KeysSpec) |
| `allow-set-title` | pane reader `TitleRequest` (Pane.hs): OSC 0/2 and APC, cleaned by `cleanName` | implemented ✅ tested | integration (check-names) |
| `allow-rename` | pane reader `RenameRequest` → `renameFromApp` (Pane.hs): `ESC k` names the window | implemented ✅ tested | integration (check-names) |
| `alternate-screen` | `hostPolicyOf` → `scrubHost` drops ?47/?1047/?1049 (HostProtocol.hs) | implemented ✅ tested | pure (EmulatorSpec) |
| `scroll-on-clear` | `hostPolicyOf` → `scrollScreenIntoHistory` before ED 2 / ED 0 at home (Emulator.hs) | implemented ✅ tested | pure (EmulatorSpec) |
| `set-clipboard` | `setSelection` (copy-mode copies, Clipboard.hs); `clipboardAction` for an app's OSC 52 | implemented ✅ tested | pure (ClipboardSpec) |
| `get-clipboard` | `clipboardAction`/`appClipboard`: `buffer` answers from the newest buffer; `request`/`both` ask the terminal (`askTerminal`, InputRequestsIO.hs), `both` also buffering the reply (`terminalReplied`, Conn.hs) | implemented ✅ tested | pure (ClipboardSpec, InputRequestsSpec) + model (SessionSpec) |
| `allow-passthrough` | pane reader `UnhandledPassthrough` → `Notify` to `paneViewers` (`on`) or `paneHolders` (`all`) | implemented | integration |
| `extended-keys` / `extended-keys-format` | `keyEncoding` → `reencodeKey` → `paneKeyMode`/`extendedKeyBytes` (Pane.hs, Keys.hs); the app's `CSI > 4` request is tracked by `scrubHost` | implemented ✅ tested | pure (SessionSpec, input-keys) |
| `user-keys` | `userKeys` → `feedKeys` (Keys.hs): each sequence is key `User`/N/, bindable, never typed into a pane | implemented ✅ tested | pure (KeysSpec) |
| `mouse` | `keyContext` → `routeKey` (mouse keys bind only when on; off passes them to the pane); `placeMouse`/`checkMouse` (Conn.hs, Mouse.hs) make `MouseDown1Pane`… keys with `mouse_*` formats (`mouseFormatVars`); `syncMouseMode` sets the outer terminal's tracking (View.hs); default root/copy-mode mouse bindings (Keymap.hs) | implemented ✅ tested | pure (MouseSpec, KeysSpec), integration (format-mouse) |
| `focus-follows-mouse` | `runInputs` selects the pane a `MouseMovePane` is over; `outerMouseMode` asks for all-motion tracking | implemented | pure (MouseSpec) |
| `backspace` | `paneKeyBytes` → `backspaceBytes` (Pane.hs, Keys.hs): what BSpace sends to a pane | implemented ✅ tested | pure (KeysSpec) |
| `prefix-timeout` | `routeKey`: a prefix-table key past the timeout falls back to the default table | implemented ✅ tested | pure (KeysSpec) |
| `synchronize-panes` | `syncTargets` → `typeKeyIntoPane`/`typeIntoPaneSynced` (Pane.hs): typed keys, pastes, `send-keys`; `#{pane_synchronized}` | implemented ✅ tested | model (SessionSpec) |
| `base-index` | `nextFreeWindowIndex` for new-window + break-pane (Server.hs) | implemented ✅ tested | integration |
| `pane-base-index` | `cmdListPanes` pane numbering (Server.hs) | implemented ✅ tested | integration |
| `status-position` | `statusLayout` (View.hs) | implemented ✅ tested | pure |
| `mode-keys` | CopyMode motions (830,839,875), table (Server.hs:3265) | implemented ✅ tested | pure |
| `history-limit` | the emulator's history (`Hat.Term.Emulator.History`): tmux's grid trimming as rows scroll in, kept across reflows, over libghostty's scrollback; a live set trims as `session_update_history` does (`setScrollbackLimit`) | implemented ✅ tested | pure (HistorySpec) + emulator (EmulatorSpec) + integration |
| `default-terminal` | `$TERM` for new panes (Server.hs) | implemented ✅ tested | integration |
| `default-client-command` | what a bare `hat` runs: the client asks the server for it (`defaultClientCommand`, app/Main.hs) | implemented ✅ tested | integration |
| `word-separators` | `CopyMode.runMotion` (830,831) | implemented ✅ tested | pure |
| `wrap-search` | `findMatch` (CopyMode.hs): a search past the last match starts over from the other end only when on | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-command` | `pipeCommand` (CopyMode.hs): what `copy-pipe` runs when given no command | implemented ✅ tested | pure (CopyModeSpec) |
| `editor` | `spawnEditor` (Editor.hs): the program a mode's `e` opens in a modal floating pane (`cmdNewEditorPane`), as choose-buffer's buffer edit | implemented ✅ tested | integration |
| `status-left` | `#{T:status-left}` in `status-format[0]` (`statusRows`, Status.hs) | implemented ✅ tested | pure (StatusSpec) |
| `status-left-length` | `#{=/…:status-left}` in `status-format[0]` | implemented ✅ tested | rendered (FormatSpec) |
| `status-right` | `#{T:status-right}` in `status-format[0]` (`statusRows`, Status.hs) | implemented ✅ tested | pure (StatusSpec) |
| `status-right-length` | `#{=/…:status-right}` in `status-format[0]` | implemented ✅ tested | rendered (FormatSpec) |
| `window-status-format` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `window-status-current-format` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `status-style` | `statusBaseStyle` (Status.hs) | implemented ✅ tested | pure (StatusSpec) |
| `window-status-style` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `window-status-current-style` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `window-status-bell-style` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `status` | `statusLayout` (View.hs), `statusRows` (Status.hs): one row per line | implemented ✅ tested | pure (StatusSpec) |
| `status-format` | `statusRows` (Status.hs) draws entry *i* on line *i* via `formatDraw` | implemented ✅ tested | pure (StatusSpec) |
| `status-bg` / `status-fg` | `statusBaseStyle` (Status.hs) | implemented ✅ tested | pure (StatusSpec) |
| `status-justify` | `#[list=on align=#{status-justify}]` in `status-format[0..2]` | implemented ✅ tested | pure (StatusSpec) |
| `status-left-style` / `status-right-style` | `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `window-status-separator` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `window-status-last-style` / `window-status-activity-style` | `#{W:}` loop in `status-format[0]` | implemented ✅ tested | pure (StatusSpec) |
| `pane-status-style` / `pane-status-current-style` / `window-pane-status-format` / `window-pane-current-status-format` | `#{P:}` loop in `status-format[1]` | implemented ✅ tested | pure (StatusSpec) |
| `session-status-style` / `session-status-current-style` | `#{S:}` loop in `status-format[2]` | implemented (format-driven, as tmux) | rendered |
| `message-style` | `drawMessage` / `drawPrompt`, `messageArea` (Message.hs) | implemented ✅ tested | pure (MessageSpec) |
| `message-format` | `drawMessage` / `drawPrompt` (Message.hs) | implemented ✅ tested | pure (MessageSpec) |
| `message-line` | `messageLine` (View.hs) | implemented ✅ tested | pure |
| `message-command-style` | `drawPrompt` in vi command mode (Message.hs) | implemented ✅ tested | pure (MessageSpec) |
| `status-keys` | `editPrompt` (Prompt.hs), fixed when `command-prompt` opens | implemented ✅ tested | pure (PromptSpec) |
| `prompt-cursor-colour` / `prompt-cursor-style` / `prompt-command-cursor-colour` / `prompt-command-cursor-style` | fixed into `PromptKeys` when `command-prompt` opens (`promptCursor`, Interact.hs); `promptCursorOf` in `renderOnce` (View.hs) sends the colour (OSC 12/112) and the shape (DECSCUSR) while the prompt is open, the command-mode ones in vi command mode | implemented ✅ tested | pure (OptionEffectSpec) |
| `pane-border-style` | `sceneChrome` (Scene.hs) via `chromeInputs` (View.hs): each pane's own value | implemented ✅ tested | pure (SceneSpec, OptionEffectSpec) |
| `pane-active-border-style` | `sceneChrome` (Scene.hs) via `chromeInputs` (View.hs): the active pane's own value | implemented ✅ tested | pure (SceneSpec, OptionEffectSpec) |
| `pane-border-status` | `statusAdjust` in `windowArrange` (Resize.hs), `buildScene` (Scene.hs) | implemented ✅ tested | pure (SceneSpec) + regress `screen-redraw-tiled` |
| `pane-border-format` | `withStatusLines` (View.hs) over `statusBase` (Scene.hs) | implemented ✅ tested | regress `screen-redraw-tiled` |
| `mode-style` | through the copy-mode styles that default to `#{E:mode-style}` | implemented ✅ tested | pure |
| `copy-mode-selection-style` | `copyView` (CopyMode.hs) via `copyLook` (View.hs): the selection, laid over each cell; formats expanded in the pane | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-match-style` / `copy-mode-current-match-style` | `copyView`: the last search's matches on screen, the one under the cursor (before it, for an emacs forward search) as current, until a command clears them (`clearsMarks`) | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-mark-style` | `copyView`: the `set-mark`/`jump-to-mark` mark's line, inverted at the mark | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-current-line-style` | `copyView`: the cursor's line; `default` leaves it alone | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-line-numbers` | `copyView` (numbers beside the text, which moves over), `copyModeFormats` (`copy_position` from the top under absolute numbering), `textColumn` for the mouse; `line-numbers-on`/`-off`/`-toggle`; hidden when a mouse key opens copy mode | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-line-number-style` / `copy-mode-current-line-number-style` | `copyView`: the numbers, the cursor's line's in the latter | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-mode-position-format` / `copy-mode-position-style` | `copyView` via `copyLook`: expanded in the pane, drawn with `formatDraw` over the first row's text unless `copy-mode -H` or `toggle-position` hides it; each pane's own value | implemented ✅ tested | pure (CopyModeSpec) + model (SessionSpec) |
| `window-style` / `window-active-style` | `paneDefaults`/`paintDefaults` (Render.hs) via `paneStyleOption` (View.hs): each pane's own value, formats expanded in the pane, and the answer to an OSC 10/11 query when set (`queryDefaultColour`, PaneColours.hs); `dim=` refused | implemented ✅ tested | pure (RenderSpec) + regress `screen-redraw-window-style` |
| `pane-scrollbars` | `paneScrollbars` → `scrollbarReserve` in `windowArrange` (Resize.hs), `buildScene`/`sceneChrome` (Scene.hs): `on` beside each pane (hidden on the alternate screen); `modal` (in a mode) and `auto-hide` drawn over the pane's edge (`paneOverlays`, Scrollbars.hs; `overlays` in Scene.hs), the cursor hidden under them, shown once revealed: by a mouse move into their area (`scrollbarHover` → `hoverScrollbars`, with all-motion tracking asked of the terminal, `wantsMotion`) or a copy-mode scroll (`scrollShowsScrollbar` in `runCopyModeCommand`); a set hides them all | implemented ✅ tested | pure (SceneSpec, ScrollbarsSpec, MouseSpec) + integration + regress `screen-redraw-scrollbars` scenes |
| `pane-scrollbars-timeout` | `revealScrollbar`/`hoverScrollbars` (Scrollbars.hs): a revealed overlay scrollbar hides this many ms after it was revealed, a mouse move anywhere over the window restarting the count | implemented ✅ tested | integration |
| `pane-scrollbars-position` | `scrollbarReserve` (Resize.hs), `buildScene` (Scene.hs) | implemented ✅ tested | pure (OptionEffectSpec) |
| `pane-scrollbars-style` | `scrollbarStyle` (Scene.hs): colours, `width=`, `pad=`; each pane's own value | implemented ✅ tested | pure (SceneSpec) |
| `display-panes-format` | `panesNumber` (DisplayPanes.hs) via `panesScreen`: panes-mode draws it on each pane's top row, the mode's window's value expanded per pane | implemented ✅ tested | pure (DisplayPanesSpec) + model (PaneModeSpec) + regress `display-panes` |
| `display-panes-colour` / `display-panes-active-colour` | `panesNumber` via `panesScreen` (DisplayPanes.hs): the mode's window's values, theme colours resolved | implemented ✅ tested | pure (DisplayPanesSpec) |
| `display-panes-border-style` | `panesPicture` via `panesScreen` (DisplayPanes.hs): panes-mode's dividers and floating frames | implemented ✅ tested | model (PaneModeSpec) |
| `pane-colours` | `panePalette` → `paintPalette` (View.hs, Render.hs): each pane's own value under its program's OSC 4 colours; also the emulator's palette defaults (`setPaletteDefaults`) and the answer to an OSC 4 query (`queryPaletteColour`, PaneColours.hs; an unset entry is asked of the terminal, `askTerminal`) | implemented ✅ tested | pure (RenderSpec) + model (SessionSpec) |
| `cursor-colour` | `cursorColourOf` via `renderOnce` (View.hs): the program's OSC 12 colour, else the active pane's value, theme colours resolved, sent as `rgb:` | implemented ✅ tested | pure (OptionEffectSpec) |
| `cursor-style` | `terminalCursor` via `renderOnce` (View.hs): the program's DECSCUSR style, else the shown screen's option (the active pane's own; the global value in a mode or popup, as tmux sets those screens from `global_w_options`), its blink standing until DEC mode 12 sets one; sent as tmux's `cnorm` then DECSCUSR (`terminalCursorBytes`); also the DECRQSS and DECRQM 12 answers with no style set (`statusReply`, `cursorBlinkReport`) | implemented ✅ tested | pure (OptionEffectSpec, EmulatorSpec) + integration |
| `input-buffer-size` | `hostPolicyOf` (Pane.hs) → `scrubHost`/`scrubPassthrough` (HostProtocol.hs) via `inputStringLimit`: an OSC 0/2/52, APC, `ESC k`, DECRQSS or tmux passthrough longer than tmux's doubling input buffer holds is dropped whole; a popup's from the global value | implemented ✅ tested | emulator (EmulatorSpec) + pure (OptionEffectSpec) |
| `fill-character` | `withFills` (View.hs) through `fillCell` (Scene.hs), for scene cells outside the window or no pane reaches | implemented ✅ tested | pure (SceneSpec) + regress `screen-redraw-outside` scenes |
| `theme` / `dark-theme-*` / `light-theme-*` | `resolveTheme` (Theme.hs) via `clientTheme` (FormatEnv.hs): theme colours in every drawn style (`styleParse`, `formatDraw`) | implemented ✅ tested | pure (ThemeSpec) |
| `clock-mode-colour` / `clock-mode-style` | `clockCells`/`clockText` (Clock.hs) via `clockView` (View.hs): the `clock-mode` screen, each pane's window's values, theme colours resolved | implemented ✅ tested | pure (ClockSpec) |
| `tree-mode-selection-style` / `tree-mode-border-style` | `treeLook` (TreeMode.hs) → `drawTree` (ModeTree.hs): the selected line, the preview box, help and info rules; the mode's window's values, formats expanded in its pane (`#{E:tree-mode-border-style}` in the info and help lines too) | implemented ✅ tested | model (PaneModeSpec) |
| `tree-mode-preview-format` / `tree-mode-preview-style` | `previewOf` (TreeMode.hs): each previewed window's or pane's boxed label, in its own options and context, over its border's background | implemented ✅ tested | model (PaneModeSpec) |
| `pane-border-lines` | `borderGlyph` (Scene.hs) | implemented ✅ tested | pure (SceneSpec) |
| `pane-border-indicators` | two-pane split in `buildScene` (Scene.hs); arrows `edgeArrows` (View.hs) | implemented ✅ tested | pure (SceneSpec) |
| `set-titles` / `set-titles-string` | `refreshTitles` (Server.hs): each session with `set-titles` titles its clients with its `set-titles-string` expanded at the current pane | implemented ✅ tested | integration + rendered (TitleSpec) |
| `escape-time` | `escTiming`/`feedKeys`/`flushEscape` (Server.hs, Keys.hs) | implemented ✅ tested | pure |
| `display-time` | `toastDeadline`/`toastExpired` (Server.hs) | implemented ✅ tested | pure |
| `focus-events` | `deliversKey` (Server.hs:1595) | implemented ✅ tested | pure |
| `window-size` | `resizeModeOf`/`effectiveWindowSize` (Resize.hs, Layout.hs) | implemented ✅ tested | pure |
| `default-shell` | `paneShell`/`paneProgram` (Pane.hs) for every spawn; `checkValue` (Command/Option.hs) refuses an unsuitable shell; seeded from `$SHELL` at start (`seedDefaultShell`) | implemented ✅ tested | pure |
| `default-command` | `shellStart` (Pane.hs) at new-session, new-window, split-window | implemented ✅ tested | pure |
| `default-size` | `defaultSizeOf` (Resize.hs): detached `new-session`, `resize-window -A`/`-a` | implemented ✅ tested | integration |
| `bell-action` / `activity-action` / `silence-action` | `alertApplies` (Alerts.hs) in `bellAlerts`/`activityAlert` (Pane.hs) and `silenceSweep` (HookMonitor.hs): whether a window's alert fires its hook and reaches the session's clients | implemented ✅ tested | pure (AlertsSpec) + regress `alerts` |
| `visual-bell` / `visual-activity` / `visual-silence` | `alertNotice` via `deliverAlert` (Alerts.hs): each client of the session hears the bell, sees "… in window N", or both | implemented ✅ tested | pure (AlertsSpec) + model (SessionSpec) |
| `monitor-activity` | activity-flag gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename` | auto-rename gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename-format` | `autoName`→`renderFormat` (Server.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `update-environment` | `applyUpdateEnvironment` (Server.hs) | implemented ✅ tested | pure |
| `history-file` | `loadHistory` after the config, `saveHistory` at exit and before a reload's exec (PromptHistory.hs) | implemented ✅ tested | pure + model (PromptHistorySpec) |
| `prompt-history-limit` | `pushHistory` (Prompt.hs) on each submitted line, `restoreHistory` (PromptHistory.hs) as the `history-file` loads: the newest entries of each type are kept | implemented ✅ tested | pure (PromptSpec, PromptHistorySpec) |
| `message-limit` | `logMessage` (MessageLog.hs) trims the message log as each message is added (tmux's `server_add_message`); `show-messages` lists what is left | implemented ✅ tested | pure + model (MessageLogSpec, SessionSpec) |
| `main-pane-width` | `namedSizesOf` → `namedLayout` (Command/Layout.hs, Layout.hs): cells or `N%` | implemented ✅ tested | pure |
| `main-pane-height` | `namedSizesOf` → `namedLayout` (Command/Layout.hs, Layout.hs): cells or `N%` | implemented ✅ tested | pure |
| `other-pane-width` | `namedSizesOf` → `namedLayout` (Command/Layout.hs, Layout.hs): cells or `N%` | implemented ✅ tested | pure |
| `other-pane-height` | `namedSizesOf` → `namedLayout` (Command/Layout.hs, Layout.hs): cells or `N%` | implemented ✅ tested | pure |
| `tiled-layout-max-columns` | `namedSizesOf` → `namedLayout` (Command/Layout.hs, Layout.hs) | implemented ✅ tested | pure |
| `destroy-unattached` | `checkUnattached` (Command/Session.hs) on every attach, switch, and client exit; a rebuilt session is spared until first attached | implemented ✅ tested | pure + model (SessionSpec) |
| `detach-on-destroy` | `destroyDestination`/`sendClientsAway` (ClientSession.hs), at every session destruction | implemented ✅ tested | pure + model (SessionSpec) |
| `exit-empty` | `serverIdle`/`waitIdle` (Server.hs) | implemented ✅ tested | pure |
| `exit-unattached` | `serverIdle`/`waitIdle` → `shutDownServer` (Server.hs, Dispatch.hs) | implemented ✅ tested | pure + integration |
| `renumber-windows` | `renumberIfSet` (Winlink.hs): window destruction, `move-window`, `kill-window -a` | implemented ✅ tested | model (SessionSpec) |
| `xterm-keys` | none — tmux keeps it only for compatibility, with no effect | implemented (by design) | — |
| `menu-style` / `menu-selected-style` / `menu-border-style` | `withMenu` (View.hs) → `menuGrid` (Menu.hs): re-read and format-expanded on every frame, the `display-menu` `-s`/`-H`/`-S` colours over them (`overrideColours`) | implemented ✅ tested | pure (MenuSpec) |
| `menu-border-lines` | `cmdDisplayMenu` (Command/Menu.hs) when `-b` is not given → `drawBox`/`drawHLine` (Box.hs) | implemented ✅ tested | pure (MenuSpec) |
| `popup-style` / `popup-border-style` | `popupView` (View.hs) → `popupGrid` (Popup.hs): re-read and format-expanded on every frame, the `display-popup` `-s`/`-S` colours over them | implemented ✅ tested | pure (PopupSpec) |
| `popup-border-lines` | `cmdDisplayPopup` (Command/Popup.hs) when neither `-b` nor `-B` is given → `popupGrid`/`innerSize` (Popup.hs) | implemented ✅ tested | pure (PopupSpec) |

## Hat's own defaults

Where hat had a value or behaviour of its own before it took on tmux's
option table, the option keeps hat's value; options hat did not have take
tmux's default. These are the live options whose default differs from
tmux's (each row's comment in `OptionTable.hs` names tmux's):

| Option | Hat default | tmux default |
|---|---|---|
| `escape-time` | `0` | `10` |
| `display-time` | `3000` | `750` |
| `history-limit` | `50000` | `2000` |
| `extended-keys` | `on` | `off` |
| `command-alias` | `split-pane`, `splitp` | also aliases for commands hat lacks |
| `update-environment` | the classic eight | also `MSYSTEM`, `WAYLAND_DISPLAY`, `XDG_*` |
| `status-style` | `fg=black,bg=green` | `bg=themegreen,fg=themeblack` |
| `status-right` | `%H:%M %d-%b-%y #H` | pane title and window offset, then the clock |
| `window-status-format` / `-current-format` | `#I:#W#F` | `#I:#W#{?window_flags,#{window_flags}, }` |
| `window-status-style` | `fg=black,bg=green` | `default` |
| `window-status-current-style` | `fg=black,bg=green,underscore` | `underscore` |
| `window-status-bell-style` | `fg=black,bg=green` | `reverse` |
| `window-status-activity-style` | `default` | `reverse` |
| `pane-border-style` | `default` | `fg=themelightgrey` |
| `pane-active-border-style` | `fg=green` | a format: theme colours by pane state |
| `display-panes-colour` / `-active-colour` | `blue` / `red` | `themeblue` / `themered` |
| `message-style` | `bg=yellow,fg=black,…fill=yellow` | the same in `themeyellow`/`themeblack` |
| `copy-mode-position-style` | `bg=yellow,fg=black` | `#{E:mode-style}` |
| `mode-style` | `reverse` | `noattr,bg=themeyellow,fg=themeblack` |
| `fill-character` | ` ` (blank) | a format: shaded inside the window, `~` outside |
| `automatic-rename-format` | `#{pane_current_command}` | also marks panes in a mode or dead |
| `default-client-command` | `new-session -A` | `new-session` |
| `set-titles-string` | hat's title (`hat` and the non-default names, see below) | `#S:#I:#W - "#T" #{session_alerts}` |

## Left out of hat

tmux features hat leaves out by design. Their options are `unsupported` rows,
so `set` and `show` refuse them (`option not supported by hat`). Their
commands still parse, so a config that binds one loads, but running one
fails (`not supported by hat`).

| Feature | Options | Commands |
|---|---|---|
| Locking clients | `lock-after-time`, `lock-command`, the `after-lock-server` hook | `lock-server`, `lock-session`, `lock-client` |
| Guessing a paste from key timing | `assume-paste-time` | — |

Hat never guesses that fast keys are a paste: every hat client turns on
bracketed paste, so a terminal that brackets its pastes marks them, and keys
from one that does not are looked up in the key tables however fast they
arrive.

## Defects (the backlog)

*(none open)*

## Fixed during the audit

- **`escape-time`** was a silent no-op (tracked as **bug `fd`**): stored in
  `Options` but read by nothing, with `tokenizeKeys` hardcoded to `escape-time
  0` (a lone trailing ESC is the Escape key). The pure coalescing core now lives
  in `Hat.Server.Keys` (`EscTiming`/`feedKeys`/`flushEscape`): a non-zero value
  holds a lone trailing ESC (`EscBuffered`) so `inputLoop` coalesces it with the
  next chunk or flushes it to Escape when the `escape-time`-ms timer fires.
  Pinned by an `OptionEffectSpec` behavior test over the pure seam.


- **`pane-base-index`** was a partial no-op: honored in the `autoName` path but
  not in `cmdListPanes`, so `#{pane_index}` (and `list-panes`,
  `display-panes`) numbered panes from 0 regardless. Now `cmdListPanes` numbers
  from `pane-base-index`; pinned by an integration test.

- **`base-index`** was honored by `new-window` but not `break-pane` (tracked as
  **bug `a4`**): `cmdBreakPane` numbered the broken-out window from a hardcoded 0.
  Both now share `nextFreeWindowIndex`, numbering from `base-index`; pinned by an
  integration test.

## Minor divergences (not defects)

- **`tree-mode-selection-style`** and **`tree-mode-border-style`** expand
  their formats in the mode's pane, as `copy-mode-selection-style` does;
  tmux expands them with no pane, so `#{E:mode-style}` reads the global
  `mode-style` there even where a window sets its own.

- **Tree-mode search** (`/`, `?`, `C-s` in choose-tree, choose-buffer and
  choose-client) is a deliberate divergence: Hat's fuzzy finder narrows and
  ranks the list as the query is typed and Enter chooses the selected match,
  where tmux jumps to the next substring match and Enter only ends the
  search (see Note [Fuzzy tree search] in `Hat.Server.ModeTree`). `n` and
  `N` still repeat a search tmux's way.

- **`theme`** `detect` reads the desktop colour scheme hat already follows
  (`Hat.Server.ColorScheme`), not each client terminal's own report, so
  every client shares one theme. Hat clients draw RGB directly, so
  `client_colours` is always 16777216 and the palettes' rich colours apply.
  Styles decoded into `Options` record fields (`mode-style`) still draw
  theme colours as the terminal default.

- **`copy-mode-position-format`**: `#{top_line_time}` is always 0 (so the
  default format shows no time), because the emulator does not stamp lines
  with the time they scroll into the history as tmux's grid does.

- **`copy-mode-selection-style`** lays its attributes over a selected cell
  as `applyModeStyle` does, toggling `reverse` where tmux's
  `screen_select_cell` adds it, so the default `mode-style` of `reverse`
  still shows a selection over reverse-video text; and `noattr` is not
  modelled (the style parser accepts and drops it).

- **`pane-border-indicators`** `arrows` draws hat's triangles at each edge's
  midpoint, where tmux draws ACS arrows next to the pane's corner (a
  deliberate design choice; see `border-arrows.sh` in the xfail list).

- **`pane-border-status`** takes its row from the edge panes when drawn, but
  the layout's minimum sizes (tmux's `layout_resize_check` and split check
  count the status row) do not yet, so a split or resize can leave an edge
  pane one row smaller than tmux allows.

- **Border ownership** where several panes' borders meet follows tmux's
  marking order, newest pane first; hat orders by pane id, which matches
  until `swap-pane`/`join-pane` reorder tmux's z-order.

- **`default-shell`** runs as a plain interactive shell named by its path,
  where tmux starts a login shell (`-name`) and runs `-c` commands under the
  shell's base name (see `paneProgram`). The shell chosen is tmux's; only the
  login-ness differs. `respawn-pane` re-reads `default-shell` where tmux
  reuses the pane's original shell.

- **`input-buffer-size`** bounds the strings hat parses above the grid
  engine (titles, `ESC k`, OSC 52, DECRQSS, tmux passthrough). The strings
  libghostty parses itself (OSC 4/7/8/10/11/104…, other DCS) are bounded by
  libghostty's own limits, not this option; an over-long one of those is not
  necessarily dropped as tmux drops it. tmux also stops storing C0 controls
  inside an OSC, which hat counts.

- **`cursor-style`** in a mode or popup reads the current global value; tmux
  fixes it when the mode's screen is made, and a later set re-reads it from
  the pane's options.

- **`pane-scrollbars`**: a click or drag on a scrollbar (tmux's
  `ScrollbarUp`/`ScrollbarSlider`/`ScrollbarDown` mouse locations and their
  default bindings, which need `copy-mode -S` and `scroll-to-mouse`) is not
  placed: it lands as on the pane. Only copy-mode commands (`send-keys -X`,
  so the wheel bindings and `copy-mode -u` too) reveal an overlay
  scrollbar, not a mouse drag's scrolling.

- **`history-file`**: hat has no search prompt, so a file's `search:`
  entries are only carried through to the next save. Entries are text: a byte that is
  not UTF-8 in a tmux-written entry is rewritten as U+FFFD.

## Hat extensions (deliberate, beyond tmux)

- **`pane-border-indicators`** also takes `color` for `colour` (the
  registry's `aliasing`); `show-options` prints the canonical `colour`.

- **`set-titles-string`** defaults to hat's own title rather than tmux's
  `#S:#I:#W - "#T" #{session_alerts}`: `hat: <session>: <window>: <path>:
  <program>`, skipping numeric session and window names and an
  automatic-rename window's, the path with `$HOME` as `~`, the program the
  pane's own title when it set one, else its command; collapsed from the
  left to 80 characters. It is an ordinary format, so a user can write it
  (or tmux's) in a config. It needs two format modifiers tmux lacks, in
  characters tmux's `format.c` leaves unused:
  - `#{~/home:key}` — the value with `home` (exactly, or as a leading
    directory) written as `~`; an empty `home` changes nothing.
  - `#{J/width/separator:part,part,…}` — each part expanded, the empty ones
    skipped, joined by the separator (`#:` escapes a colon in it). While the
    join is wider than `width` characters, parts drop from the left; the one
    before the last first shortens from the left at `/` (`…/b/c`, then
    `…/c`) before it drops; the last part stands whole even over the width.

## The effect-test sweep

Every row is pinned by a prefix-style **effect test**: set a non-default value,
drive the consumer, assert the *observable* result. This is what keeps the
audit self-maintaining: a silent no-op (or wrong-semantics) option fails a test
instead of needing another manual audit. An option whose consumer is
`ServerState`-IO gets its decision extracted as an exported pure function first
(the `deliversKey` pattern); a new option is not done until its row lands here
with a ✅.
