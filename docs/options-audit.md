# Options behavior audit

A living catalog of every option HAT accepts, tracking whether the stored value
*actually drives the behavior tmux gives it* — not merely that it parses and
lands in an `Options` field. Built so we never re-derive "which option is
unimplemented / easiest to pick off next": read this table, pick a row, fix it.

## What the verdicts mean

- **implemented** — a consumer reads the field and produces the tmux behavior
  the value controls. Only rows with a ✅ *test* column are behavior-verified;
  the rest are consumer-verified (a real consumer exists and looks right) but
  not yet pinned by a behavior test, so a subtle read-but-partial bug could
  still hide there.
- **defect: no consumer** — the field is stored and read by nothing. Setting it
  does nothing.
- **defect: wrong semantics** — a consumer reads it, but not the way tmux means
  it, so the observable effect is wrong.

The **seam** column is what a real effect-test needs: `pure` = an exported pure
function already takes the value (testable today, prefix-style); `needs seam` =
the consumer is an IO function over `ServerState`, so a pure core must be
extracted first (the `deliversKey` pattern).

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
| `base-index` | initial + next-free window index (Server.hs:1146,2337) | implemented | needs seam |
| `pane-base-index` | pane numbering (Server.hs:2878) | implemented | needs seam |
| `status-position` | `statusLayout` (View.hs) | implemented ✅ tested | pure |
| `mode-keys` | CopyMode motions (830,839,875), table (Server.hs:3265) | implemented ✅ tested | pure |
| `history-limit` | emulator scrollback cap (Server.hs:1253,741) | implemented | needs seam |
| `default-terminal` | `$TERM` for new panes (Server.hs:1233) | implemented | needs seam |
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
| `set-titles` | `refreshTitles` gate (Server.hs:2831) | implemented | needs seam |
| `escape-time` | — | **defect: no consumer** | — |
| `display-time` | toast duration (Server.hs:1752) | implemented | needs seam |
| `focus-events` | `deliversKey` (Server.hs:1595) | implemented | pure |
| `aggressive-resize` | `resizeModeOf` (Server.hs) | implemented ✅ tested | pure |
| `monitor-activity` | activity-flag gate (Server.hs:1337) | implemented | needs seam |
| `automatic-rename` | auto-rename gate (Server.hs:1185,2769) | implemented | needs seam |
| `automatic-rename-format` | `refreshAutoNames` (Server.hs:2809) | implemented | needs seam |
| `update-environment` | `refreshSessionEnv` (Server.hs:1107) | implemented | needs seam |
| `main-pane-width` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |
| `main-pane-height` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |

## Defects (the backlog)

1. **`escape-time` — no consumer.** ESC disambiguation in
   `Hat.Server.Keys.tokenizeKeys` is hardcoded to `escape-time 0` semantics (a
   lone trailing ESC is the Escape key). A non-zero value is stored and ignored.
   A real fix buffers a trailing lone ESC and arms an `escape-time`-ms timer in
   the input path: coalesce with the next bytes if they arrive first, else emit
   Escape. Needs event-loop timing.

## Minor divergences (not defects)

- **`main-pane-width`/`-height`** correctly implement tmux's *absolute cell
  count* (the ratio `mainPaneWidth / cols` yields `mainPaneWidth` cells), pinned
  by `mainPaneRatio` effect tests. Two small divergences remain as follow-ups,
  not silent no-ops: the main pane is clamped to 10–90% of the window (tmux
  clamps only to leave the other panes room), and a `%`-suffixed percentage
  value isn't parsed.

## Turning consumer-verified into behavior-verified: the sweep

The catalog above is a consumer trace; only `prefix` is pinned by a behavior
test. The durable way to close the gap — and to catch any remaining
read-but-partial bug — is a prefix-style **effect test per option**: set a
non-default value, drive the consumer, assert the *observable* result.

- Behavior-verified so far (the ✅ rows): `prefix` (KeysSpec), the copy-mode
  trio `mode-keys`/`word-separators`/`mode-style` (CopyModeSpec), and
  `focus-events`, `main-pane-width`/`-height`, and the four `pane-border-*` in
  `OptionEffectSpec`.
- Every `needs seam` option first needs a pure core extracted from its
  `ServerState`-IO consumer (e.g. `statusCells` → a pure
  `Options -> … -> [Cell]`). Each extraction is a small, self-contained step;
  the resulting effect test is the permanent regression guard *and* flips the
  row to behavior-verified.

This sweep is the milestone that makes the audit self-maintaining: once every
row has a passing effect test, a future silent no-op (or wrong-semantics) option
fails a test instead of needing another manual audit.
