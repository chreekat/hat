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
the registry's gate (`live`/`notYet` rows in `src/Hat/Model/OptionTable.hs`),
which `set`/`show` enforce. This catalog covers the live rows.

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
| `assume-paste-time` | none — tmux skips paste detection when the client terminal has bracketed paste, and every hat client enables it (`enterAltScreen`); pastes arrive bracketed and bypass the key tables (`routeKey`) | implemented (by design) | — |
| `allow-set-title` | pane reader `TitleRequest` (Pane.hs): OSC 0/2 and APC, cleaned by `cleanName` | implemented ✅ tested | integration (check-names) |
| `allow-rename` | pane reader `RenameRequest` → `renameFromApp` (Pane.hs): `ESC k` names the window | implemented ✅ tested | integration (check-names) |
| `alternate-screen` | `hostPolicyOf` → `scrubHost` drops ?47/?1047/?1049 (HostProtocol.hs) | implemented ✅ tested | pure (EmulatorSpec) |
| `scroll-on-clear` | `hostPolicyOf` → `scrollScreenIntoHistory` before ED 2 / ED 0 at home (Emulator.hs) | implemented ✅ tested | pure (EmulatorSpec) |
| `set-clipboard` | `setSelection` (copy-mode copies, Clipboard.hs); `clipboardAction` for an app's OSC 52 | implemented ✅ tested | pure (ClipboardSpec) |
| `get-clipboard` | `clipboardAction`/`appClipboard`: `buffer` answers from the newest buffer; `request`/`both` refused (no outer-terminal clipboard query yet) | partial ✅ tested | pure (ClipboardSpec) |
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
| `history-limit` | emulator scrollback cap (Server.hs) | implemented ✅ tested | integration |
| `default-terminal` | `$TERM` for new panes (Server.hs) | implemented ✅ tested | integration |
| `word-separators` | `CopyMode.runMotion` (830,831) | implemented ✅ tested | pure |
| `wrap-search` | `findMatch` (CopyMode.hs): a search past the last match starts over from the other end only when on | implemented ✅ tested | pure (CopyModeSpec) |
| `copy-command` | `pipeCommand` (CopyMode.hs): what `copy-pipe` runs when given no command | implemented ✅ tested | pure (CopyModeSpec) |
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
| `pane-border-style` | `sceneChrome` (Scene.hs) via `chromeInputs` (View.hs): each pane's own value | implemented ✅ tested | pure (SceneSpec, OptionEffectSpec) |
| `pane-active-border-style` | `sceneChrome` (Scene.hs) via `chromeInputs` (View.hs): the active pane's own value | implemented ✅ tested | pure (SceneSpec, OptionEffectSpec) |
| `pane-border-status` | `statusAdjust` in `windowArrange` (Resize.hs), `buildScene` (Scene.hs) | implemented ✅ tested | pure (SceneSpec) + regress `screen-redraw-tiled` |
| `pane-border-format` | `withStatusLines` (View.hs) over `statusBase` (Scene.hs) | implemented ✅ tested | regress `screen-redraw-tiled` |
| `mode-style` | `CopyMode.overlaySelection` (View.hs:364) | implemented ✅ tested | pure |
| `window-style` / `window-active-style` | `paneDefaults`/`paintDefaults` (Render.hs) via `paneStyleOption` (View.hs): each pane's own value, formats expanded in the pane, and the answer to an OSC 10/11 query when set (`queryDefaultColour`, PaneColours.hs); `dim=` refused | implemented ✅ tested | pure (RenderSpec) + regress `screen-redraw-window-style` |
| `pane-scrollbars` | `paneScrollbars` → `scrollbarReserve` in `windowArrange` (Resize.hs), `buildScene`/`sceneChrome` (Scene.hs): `off` and `on` (hidden on the alternate screen); `modal` and `auto-hide` refused | implemented ✅ tested | pure (SceneSpec, OptionEffectSpec) + regress `screen-redraw-scrollbars` scenes |
| `pane-scrollbars-position` | `scrollbarReserve` (Resize.hs), `buildScene` (Scene.hs) | implemented ✅ tested | pure (OptionEffectSpec) |
| `pane-scrollbars-style` | `scrollbarStyle` (Scene.hs): colours, `width=`, `pad=`; each pane's own value | implemented ✅ tested | pure (SceneSpec) |
| `display-panes-format` | `panesStamp` (DisplayPanes.hs) via the overlay in `renderOnce` (View.hs): drawn on each pane's top row | implemented ✅ tested | pure (DisplayPanesSpec) |
| `display-panes-colour` / `display-panes-active-colour` | `panesStamp` via `renderOnce` (View.hs): the window's values, theme colours resolved | implemented ✅ tested | pure (DisplayPanesSpec) |
| `pane-colours` | `panePalette` → `paintPalette` (View.hs, Render.hs): each pane's own value under its program's OSC 4 colours; also the emulator's palette defaults (`setPaletteDefaults`) and the answer to an OSC 4 query (`queryPaletteColour`, PaneColours.hs) | implemented ✅ tested | pure (RenderSpec) + model (SessionSpec) |
| `cursor-colour` | `cursorColourOf` via `renderOnce` (View.hs): the program's OSC 12 colour, else the active pane's value, theme colours resolved, sent as `rgb:` | implemented ✅ tested | pure (OptionEffectSpec) |
| `fill-character` | `withFills` (View.hs) through `fillCell` (Scene.hs), for scene cells outside the window or no pane reaches | implemented ✅ tested | pure (SceneSpec) + regress `screen-redraw-outside` scenes |
| `theme` / `dark-theme-*` / `light-theme-*` | `resolveTheme` (Theme.hs) via `clientTheme` (FormatEnv.hs): theme colours in every drawn style (`styleParse`, `formatDraw`) | implemented ✅ tested | pure (ThemeSpec) |
| `pane-border-lines` | `borderGlyph` (Scene.hs) | implemented ✅ tested | pure (SceneSpec) |
| `pane-border-indicators` | two-pane split in `buildScene` (Scene.hs); arrows `edgeArrows` (View.hs) | implemented ✅ tested | pure (SceneSpec) |
| `set-titles` | `refreshTitles` gate (Server.hs) | implemented ✅ tested | integration |
| `escape-time` | `escTiming`/`feedKeys`/`flushEscape` (Server.hs, Keys.hs) | implemented ✅ tested | pure |
| `display-time` | `toastDeadline`/`toastExpired` (Server.hs) | implemented ✅ tested | pure |
| `focus-events` | `deliversKey` (Server.hs:1595) | implemented ✅ tested | pure |
| `window-size` | `resizeModeOf`/`effectiveWindowSize` (Resize.hs, Layout.hs) | implemented ✅ tested | pure |
| `default-shell` | `paneShell`/`paneProgram` (Pane.hs) for every spawn; `checkValue` (Command/Option.hs) refuses an unsuitable shell; seeded from `$SHELL` at start (`seedDefaultShell`) | implemented ✅ tested | pure |
| `default-command` | `shellStart` (Pane.hs) at new-session, new-window, split-window | implemented ✅ tested | pure |
| `default-size` | `defaultSizeOf` (Resize.hs): detached `new-session`, `resize-window -A`/`-a` | implemented ✅ tested | integration |
| `monitor-activity` | activity-flag gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename` | auto-rename gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename-format` | `autoName`→`renderFormat` (Server.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `update-environment` | `applyUpdateEnvironment` (Server.hs) | implemented ✅ tested | pure |
| `history-file` | `loadHistory` after the config, `saveHistory` at exit and before a reload's exec (PromptHistory.hs) | implemented ✅ tested | pure + model (PromptHistorySpec) |
| `prompt-history-limit` | `pushHistory` (Prompt.hs) on each submitted line, `restoreHistory` (PromptHistory.hs) as the `history-file` loads: the newest entries of each type are kept | implemented ✅ tested | pure (PromptSpec, PromptHistorySpec) |
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

- **`theme`** `detect` reads the desktop colour scheme hat already follows
  (`Hat.Server.ColorScheme`), not each client terminal's own report, so
  every client shares one theme. Hat clients draw RGB directly, so
  `client_colours` is always 16777216 and the palettes' rich colours apply.
  Styles decoded into `Options` record fields (`mode-style`) still draw
  theme colours as the terminal default.

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

- **`aggressive-resize`** is accepted but read by nothing. tmux uses it to
  size a window only from clients whose *current* window it is; hat sizes all
  of a session's windows together from that session's own clients, so the
  option could only matter for a window linked into several sessions.

- **`default-shell`** runs as a plain interactive shell named by its path,
  where tmux starts a login shell (`-name`) and runs `-c` commands under the
  shell's base name (see `paneProgram`). The shell chosen is tmux's; only the
  login-ness differs. `respawn-pane` re-reads `default-shell` where tmux
  reuses the pane's original shell.

- **`history-file`**: hat has no search prompt, so a file's `search:`
  entries are only carried through to the next save. Entries are text: a byte that is
  not UTF-8 in a tmux-written entry is rewritten as U+FFFD.

## The effect-test sweep

Every row is pinned by a prefix-style **effect test**: set a non-default value,
drive the consumer, assert the *observable* result. This is what keeps the
audit self-maintaining: a silent no-op (or wrong-semantics) option fails a test
instead of needing another manual audit. An option whose consumer is
`ServerState`-IO gets its decision extracted as an exported pure function first
(the `deliversKey` pattern); a new option is not done until its row lands here
with a ✅.
