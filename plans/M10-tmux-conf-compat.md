# M10 — Faithful `~/.tmux.conf` compatibility

Make hat load the author's real `~/.tmux.conf` (174 lines) and behave
the way tmux does for every line of it. This is Phase 1 of "daily
driver"; tmux-resurrect support is Phase 2 ([[M11-resurrect]]).

**The governing rule (from the user):** never accept a config option or
command without also implementing what it is expected to do. Parsing an
option and silently ignoring it is the anti-pattern — it makes the
config look supported when it isn't. Anything not yet implemented must
**fail loudly**, not vanish.

## Acceptance target

`hat -f ~/.tmux.conf` loads with **zero** config errors, and each
binding and option observably behaves as it does under tmux. Because
`loadConfig` logs an error per bad line and continues
(`src/Hat/Server.hs:177-189`), the config-error list is a **burn-down
list**: it starts long and each sub-milestone retires entries. "Zero
errors" is the final state of M10, reached incrementally — never faked.

The six end-to-end workflows in `FEATURES.md` that don't need resurrect
(#1 open-in-same-dir, #2 move-pane-to-edge, #3 theme toggle, #4 status
line, #5 scratch window) are the human-facing acceptance demos.

## Facts of the ground (verified)

- **`setOption` silently swallows unknowns.** `src/Hat/Server.hs:1274`:
  known options set structured fields; `@foo` goes to `Options.user`;
  **everything else falls through to `insertRawOption`** (stored in
  `Options.raw`, does nothing). This fall-through is the honest-failure
  edit point. `@`-options stay preserved (that IS their behavior).
- **Target resolution is a stub.** `src/Hat/Server.hs:1737-1738`:
  `targetPane st mclient _ = ...` **ignores the target argument** and
  always returns the current active pane. So `-t !`, `-t ~`, `-t %N`,
  `-t sess:win.pane` do nothing special. Every `-t !` binding in the
  config (`Z`, `H/J/K/L`) currently hits the wrong pane.
- **`split-window` has `-h`/`-b` but no `-f`.**
  `src/Hat/Server.hs:1514` parses `parseArgs "ctlp"` and computes
  `orient`/`before`; there is no full-window (`-f`) split. The config's
  `splitw -fh`/`-fv` (H/J/K/L) need it.
- **No style-string parser; styles are hardcoded.** `applyBorders`
  (`src/Hat/Server.hs:575`) draws borders with a fixed glyph and no
  color; `statusStyle`/`toastStyle` are literal `Cell.Style`
  (`src/Hat/Server.hs:692-711`). `pane-border-style`,
  `window-status-current-style`, etc. are parsed into `raw` and ignored.
- **Bell is tracked; activity is not.** `Window.bellFlag`
  (`src/Hat/Model.hs:89`) is set on BEL (`src/Hat/Server.hs:418`) and
  surfaces as `!` in `window_flags` (`src/Hat/Server.hs:855-860`). There
  is no per-window *activity* flag, so `monitor-activity` /
  `next-window -a` / `window_activity` have nothing to read.
- **The format engine is capable.** `src/Hat/Server/Format.hs` handles
  `#{?..}`, `#{=N:..}`, `#{e|..}`, `#S/#I/#W/#F/#P/#H/#T`, `#(shell)`;
  variable lookup is `var name = Map.findWithDefault "" name env`
  (`:55`). `#{@foo}` resolves by looking up `"@foo"` in the FormatEnv,
  so the server must seed `Options.user` into the env (verify in M10e).
- **Options model** (`src/Hat/Model/Options.hs:23-52`): structured
  fields + `user :: Map` (`@`) + `raw :: Map` (everything else).
- **Command-prompt `%%` templates were deferred** in M9; `choose-window`
  (M10d) is the first real consumer, so they land there.

## Wire-protocol impact

None expected. Pickers render as server-side overlays inside existing
`Draw` ops (same approach as copy mode / the command prompt). Style
changes are just different `Cell.Style` values in the frames already
sent. `set-titles` reuses the existing `SetTitle` server→client message
(`src/Hat/Client.hs`).

## Sub-milestones

Ordered by dependency. Each ends with a demoable behavior and shortens
the M10 burn-down list. Commits within a sub-milestone stay small; TDD
throughout (a failing test before behavior).

### M10a — Honest failure + config-load harness

*The demo:* `hat -f ~/.tmux.conf` starts and logs an error for exactly
the not-yet-implemented options/commands — nothing is silently accepted.
An integration test snapshots that error list as the burn-down baseline.

1. **Make `setOption` reject unknown non-`@` options.** Change the
   `otherwise` fall-through at `src/Hat/Server.hs:1274` from
   `insertRawOption` to `Left ("unimplemented option: " <> name)`. Keep
   the `@`-prefix branch (stored in `Options.user`). Retire
   `insertRawOption`/`Options.raw` if nothing else reads them (check
   first).
2. **Config-load test harness.** Copy the real `~/.tmux.conf` into a
   fixture; load it via the server; capture the logged `ConfigError`
   lines. Assert against a checked-in expected list — the living
   burn-down record. Each later sub-milestone deletes lines from it.

*Anti-pattern:* do not "pass" this milestone by re-adding silent
acceptance. The error list is supposed to be long here.

### M10b — Target resolution

*The demo:* `resize-pane -t ! -Z` (the config's `Z`) zooms the
*alternate* pane, not the current one.

1. **Implement `targetPane`/`targetWindow`/`targetSession`** at
   `src/Hat/Server.hs:1737`. Resolve the tmux target grammar the config
   uses: `!` (last pane/window), `~`/`{marked}`, `%N`/`@N`/`$N` ids,
   `+`/`-`, and `sess:win.pane` with the `=`/prefix/glob lookup rules.
   Introduce a small `Target` domain type rather than passing `Maybe
   Text` around (avoid boolean/stringly-typed blindness).
2. **Mark-pane state.** Add the server-level "marked pane" (`select-pane
   -m`/`-M`, token `~`) that `join-pane` and the config's `V`/`S` rely
   on.

### M10c — Pane-management commands + `split -f`

*The demo:* `prefix L` moves the current pane to the right edge
(workflow #2); `prefix C-k` clears the pane's history.

1. **`swap-pane [-t target] [-U/-D]`** and **`clear-history`** (bound
   `C-k`).
2. **`join-pane [-h|-v] [-b] -s src -t dst`** and **`break-pane`**,
   using M10b targets.
3. **`split-window -f`** (full-window split) at
   `src/Hat/Server.hs:1514`; verify `-b`/`-c` compose with it. Wire the
   H/J/K/L composition end-to-end.

### M10d — Interactive pickers (`choose-tree` / `choose-window`)

*The demo:* `prefix /` opens a searchable session/window tree; typing
filters; Enter switches. `V`/`S` pick a window and `join-pane` it.

1. **`choose-tree [-GZw]` overlay** — a server-side modal list (same
   overlay pattern as the command prompt): render the session→window
   tree, cursor navigation, Enter runs the per-item command.
2. **Type-to-search** + the pre-typed `send-keys /` entry (`bind /
   choose-tree -GZw \; send-keys /`).
3. **`choose-window <template>`** with **`%%`** substitution (the
   command-prompt template work deferred in M9), so `choose-window
   'join-pane -hs "%%"'` works.

### M10e — Styling

*The demo:* `prefix R` (theme toggle, workflow #3) visibly swaps the
pane border colors and updates `@pane-theme`.

1. **Style-string parser** — `fg=`, `bg=`, `bold`, named + `colourN` +
   `brightX`, comma-separated. Produce a `Cell.Style`.
2. **Apply styles in render**: `pane-border-style`,
   `pane-active-border-style`, `window-status-style`,
   `window-status-current-style`, `window-status-bell-style`,
   `status-style`. Thread through `applyBorders`
   (`src/Hat/Server.hs:575`) and the status/window-status builders.
3. **`pane-border-lines heavy`** (glyph set) and
   **`pane-border-indicators both`** (direction arrows).
4. **Confirm `#{@pane-theme}` reads** — seed `Options.user` into the
   FormatEnv so the theme conditional in `if-shell` resolves.

### M10f — Behavioral options + activity

*The demo:* activity in a background window marks it in the status line;
reattaching after `ssh -X` refreshes `DISPLAY` in new panes.

1. **Cheap honors:** `set-titles` (via `SetTitle`), `display-time`
   (toast duration), `escape-time` (accept; document that 0 is the
   current behavior), `focus-events` (forward `\e[I`/`\e[O`).
2. **`update-environment`** — refresh the listed vars on each attach.
3. **`aggressive-resize`** — size a window to the smallest *attached*
   client rather than per-session.
4. **Activity tracking** — per-window activity flag + `monitor-activity`,
   surfaced as `window_activity`/`#F`, consumed by `next-window -a`
   (config's `C-a`).

### M10g — `main-pane-width` + named layouts (heavy tail)

*The demo:* `select-layout main-vertical` honors `main-pane-width 100`.

Named layouts (`main-vertical`, `main-horizontal`, `even-*`, `tiled`)
plus `main-pane-width`/`main-pane-height`. This is the heaviest option
in the config (FEATURES tags layouts P3) and shares the layout-tree work
with resurrect's layout *strings* ([[M11-resurrect]] M11b/c). **Likely a
revision-round split** — carry it as the last item and re-scope once
M10a–f land and the burn-down list is short.

## Not part of M10 (deliberately)

- tmux-resurrect (@-option round-trip completeness, `list-* -aF` fields,
  `window_layout` strings, `select-layout "<string>"`, `move-window`) —
  Phase 2, [[M11-resurrect]].
- Mouse support, `display-menu`/`display-popup`, OSC 52 — P2/P3.
- Hooks — not needed by the config or by resurrect (continuum only).

## Order of operations

M10a first (it defines the burn-down list everything else is graded
against). M10b before M10c (targets gate the pane-move commands). M10d/e/f
can interleave. M10g last, and likely re-scoped.
