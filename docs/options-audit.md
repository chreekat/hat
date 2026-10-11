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
| `prefix` | `routeKeys` (Server.hs:1693), `parseKeyName` (3124) | implemented ✅ tested | pure |
| `base-index` | `nextFreeWindowIndex` for new-window + break-pane (Server.hs) | implemented ✅ tested | integration |
| `pane-base-index` | `cmdListPanes` pane numbering (Server.hs) | implemented ✅ tested | integration |
| `status-position` | `statusLayout` (View.hs) | implemented ✅ tested | pure |
| `mode-keys` | CopyMode motions (830,839,875), table (Server.hs:3265) | implemented ✅ tested | pure |
| `history-limit` | emulator scrollback cap (Server.hs) | implemented ✅ tested | integration |
| `default-terminal` | `$TERM` for new panes (Server.hs) | implemented ✅ tested | integration |
| `word-separators` | `CopyMode.runMotion` (830,831) | implemented ✅ tested | pure |
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
| `pane-border-style` | `borderCells` (View.hs) | implemented ✅ tested | pure |
| `pane-active-border-style` | `borderCells` (View.hs) | implemented ✅ tested | pure |
| `mode-style` | `CopyMode.overlaySelection` (View.hs:364) | implemented ✅ tested | pure |
| `pane-border-lines` | `mapGlyph`/`borderCells` (View.hs) | implemented ✅ tested | pure |
| `pane-border-indicators` | `borderCells` (View.hs) | implemented ✅ tested | pure |
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

- **`aggressive-resize`** is accepted but read by nothing. tmux uses it to
  size a window only from clients whose *current* window it is; hat sizes all
  of a session's windows together from that session's own clients, so the
  option could only matter for a window linked into several sessions.

- **`default-shell`** runs as a plain interactive shell named by its path,
  where tmux starts a login shell (`-name`) and runs `-c` commands under the
  shell's base name (see `paneProgram`). The shell chosen is tmux's; only the
  login-ness differs. `respawn-pane` re-reads `default-shell` where tmux
  reuses the pane's original shell.

- **`history-file`** loads every entry, where tmux keeps the newest
  `prompt-history-limit` (default 100) of each type; hat's prompt history has
  no limit yet. Hat has no search prompt, so a file's `search:` entries are
  only carried through to the next save. Entries are text: a byte that is
  not UTF-8 in a tmux-written entry is rewritten as U+FFFD.

## The effect-test sweep

Every row is pinned by a prefix-style **effect test**: set a non-default value,
drive the consumer, assert the *observable* result. This is what keeps the
audit self-maintaining: a silent no-op (or wrong-semantics) option fails a test
instead of needing another manual audit. An option whose consumer is
`ServerState`-IO gets its decision extracted as an exported pure function first
(the `deliversKey` pattern); a new option is not done until its row lands here
with a ✅.
