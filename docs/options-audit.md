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

The **seam** column is how a row is (or would be) behavior-verified: `pure` = an
exported pure function takes the value, tested prefix-style in `OptionEffectSpec`;
`integration` = exercised end-to-end in `IntegrationSpec`; `rendered` = a format
string the FormatSpec-verified `renderFormat` expands; `plumbed` = handed
straight to a sink with no branching logic, so there is nothing to unit-test —
verified by inspection, **not yet pinned by a behavior test**; `needs seam` = the
consumer is an IO function over `ServerState` whose pure core is not yet
extracted (the `deliversKey` pattern).

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
| `base-index` | initial + next-free window index (Server.hs) | implemented ✅ tested | integration |
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
| `escape-time` | — | **defect: no consumer** | — |
| `display-time` | toast duration (Server.hs) | implemented | plumbed |
| `focus-events` | `deliversKey` (Server.hs:1595) | implemented | pure |
| `aggressive-resize` | `resizeModeOf` (Server.hs) | implemented ✅ tested | pure |
| `monitor-activity` | activity-flag gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename` | auto-rename gate (Server.hs) | implemented ✅ tested | integration |
| `automatic-rename-format` | `autoName`→`renderFormat` (Server.hs) | implemented ✅ tested | rendered (FormatSpec) |
| `update-environment` | `applyUpdateEnvironment` (Server.hs) | implemented ✅ tested | pure |
| `main-pane-width` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |
| `main-pane-height` | `mainPaneRatio` (Server.hs) | implemented ✅ tested | pure |

## Defects (the backlog)

1. **`escape-time` — no consumer.** ESC disambiguation in
   `Hat.Server.Keys.tokenizeKeys` is hardcoded to `escape-time 0` semantics (a
   lone trailing ESC is the Escape key). A non-zero value is stored and ignored.
   A real fix buffers a trailing lone ESC and arms an `escape-time`-ms timer in
   the input path: coalesce with the next bytes if they arrive first, else emit
   Escape. Needs event-loop timing.

## Fixed during the audit

- **`pane-base-index`** was a partial no-op: honored in the `autoName` path but
  not in `cmdListPanes`, so `#{pane_index}` (and `list-panes`,
  `display-panes`) numbered panes from 0 regardless. Now `cmdListPanes` numbers
  from `pane-base-index`; pinned by an integration test.

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

- Behavior-verified so far are the ✅ rows in the table above (via
  `OptionEffectSpec`, `CopyModeSpec`, `KeysSpec`, `FormatSpec`, or
  `IntegrationSpec`). The only rows without a behavior test are the four
  `plumbed` option `display-time` (its effect is a timed toast dismissal, not
  synchronously observable without a sleep) and the `escape-time` defect.
- Every `needs seam` option first needs a pure core extracted from its
  `ServerState`-IO consumer (e.g. `statusCells` → a pure
  `Options -> … -> [Cell]`). Each extraction is a small, self-contained step;
  the resulting effect test is the permanent regression guard *and* flips the
  row to behavior-verified.

This sweep is the milestone that makes the audit self-maintaining: once every
row has a passing effect test, a future silent no-op (or wrong-semantics) option
fails a test instead of needing another manual audit.
