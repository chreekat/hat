# Options behavior audit

A living catalog of every option HAT accepts, tracking whether the stored value
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
| `status-left` | `expandFormat`→`renderFormat` (View.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `status-left-length` | `assembleStatusRow` (View.hs) | implemented ✅ tested | pure |
| `status-right` | `expandFormat`→`renderFormat` (View.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `status-right-length` | `assembleStatusRow` (View.hs) | implemented ✅ tested | pure |
| `window-status-format` | `windowEntryFormat` (View.hs) | implemented ✅ tested | pure |
| `window-status-current-format` | `windowEntryFormat` (View.hs) | implemented ✅ tested | pure |
| `status-style` | `assembleStatusRow` (View.hs) | implemented ✅ tested | pure |
| `window-status-style` | `windowEntryStyle` (View.hs) | implemented ✅ tested | pure |
| `window-status-current-style` | `windowEntryStyle` (View.hs) | implemented ✅ tested | pure |
| `window-status-bell-style` | `windowEntryStyle` (View.hs) | implemented ✅ tested | pure |
| `pane-border-style` | `borderCells` (View.hs) | implemented ✅ tested | pure |
| `pane-active-border-style` | `borderCells` (View.hs) | implemented ✅ tested | pure |
| `mode-style` | `CopyMode.overlaySelection` (View.hs:364) | implemented ✅ tested | pure |
| `pane-border-lines` | `mapGlyph`/`borderCells` (View.hs) | implemented ✅ tested | pure |
| `pane-border-indicators` | `borderCells` (View.hs) | implemented ✅ tested | pure |
| `set-titles` | `refreshTitles` gate (Server.hs) | implemented ✅ tested | integration |
| `escape-time` | `escTiming`/`feedKeys`/`flushEscape` (Server.hs, Keys.hs) | implemented ✅ tested | pure |
| `display-time` | `toastDeadline`/`toastExpired` (Server.hs) | implemented ✅ tested | pure |
| `focus-events` | `deliversKey` (Server.hs:1595) | implemented ✅ tested | pure |
| `aggressive-resize` | `resizeModeOf` (Server.hs) | implemented ✅ tested | pure |
| `monitor-activity` | activity-flag gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename` | auto-rename gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename-format` | `autoName`→`renderFormat` (Server.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `update-environment` | `applyUpdateEnvironment` (Server.hs) | implemented ✅ tested | pure |
| `main-pane-width` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |
| `main-pane-height` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |

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

- **`main-pane-width`/`-height`** correctly implement tmux's *absolute cell
  count* (the ratio `mainPaneWidth / cols` yields `mainPaneWidth` cells), pinned
  by `mainPaneRatio` effect tests. Two small divergences remain as follow-ups,
  not silent no-ops: the main pane is clamped to 10–90% of the window (tmux
  clamps only to leave the other panes room), and a `%`-suffixed percentage
  value isn't parsed.

## The effect-test sweep

Every row is pinned by a prefix-style **effect test**: set a non-default value,
drive the consumer, assert the *observable* result. This is what keeps the
audit self-maintaining: a silent no-op (or wrong-semantics) option fails a test
instead of needing another manual audit. An option whose consumer is
`ServerState`-IO gets its decision extracted as an exported pure function first
(the `deliversKey` pattern); a new option is not done until its row lands here
with a ✅.
