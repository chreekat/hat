# M8 — Copy mode + paste buffers

Bring hat to feature parity for the tmux "copy mode" workflow: enter a
mode where a per-pane cursor navigates the pane's scrollback + visible
grid, extends a selection, and yanks the selection into a named
paste-buffer. Also the buffer primitives (`set-buffer`, `list-buffers`,
`save-buffer`, `paste-buffer`, `copy-pipe`, `pipe-pane -I`) so the mode
plugs into the rest of the shell.

Acceptance targets (upstream regress):
- `regress/copy-mode-test-vi.sh` — **passing**.
- `regress/copy-mode-test-emacs.sh` — blocked on tab preservation.
  libvterm expands tabs to spaces in the grid, so a copied tab-indented
  line comes back as spaces where upstream keeps a literal `\t`. Needs
  emulator-level tab tracking; stays xfailed until then.

Both tests drive copy mode entirely via `send-keys -X <cmd>` and read
back with `show-buffer`. They don't attach a client, so **passing them
requires the data model + command dispatch only** — client-side
rendering is not on the critical path for the regress green light and
is deferred to a later milestone.

## Zero — Facts of the ground

**Hat internals** (all confirmed present at cited spots):
- Emulator already has a scrollback ring: `sbRef :: IORef (Seq [Cell])`
  with `scrollbackLength`, `scrollbackLine`, `screenRowText`, `screenCell`
  (`src/Hat/Term/Emulator.hsc:141,309,313,316,321`).
- `Emu.snapshot -> Screen { size, cells, cursor, cursorVisible }`
  (`src/Hat/Term/Emulator.hsc:295–301`).
- `Pane` has no mode-state field yet; `Window` has no mode-state field
  (`src/Hat/Model.hs:69–87`).
- Key routing: `handleInput` → `tokenizeKeys` → `routeKeys` →
  `Passthrough | RunCommands` (`src/Hat/Server.hs:726–768`,
  `src/Hat/Server/Keys.hs:151–173`).
- Keymap is `Map Text (Map Text [[Text]])` — table name → key → argv
  list (`src/Hat/Model/Options.hs:36`).
- `Options` already has `modeKeys :: ModeKeys = KeysVi | KeysEmacs`
  (`src/Hat/Model/Options.hs:22–32`).
- Wire has `Input ByteString` client→server and `Draw [DrawOp]`
  server→client (`src/Hat/Wire.hs:58–92`). **No new wire messages are
  needed** — copy-mode entry/exit is server-side, keys are still
  `Input`, selection renders as reverse-video cells inside existing
  `Draw` ops.
- No copy-mode or buffer commands exist in `commandTable`
  (`src/Hat/Server.hs:826–864`). `[` and `]` are unbound at prefix.

**tmux semantics** (from `/home/b/src/tmux/`):
- Copy-mode cursor space is the union of scrollback + visible grid.
  Absolute row 0 = oldest scrollback line; rows `hsize..hsize+sy-1` are
  the live screen. Selection uses the same absolute coordinates
  (`window-copy.c:250–354`).
- Selection kinds: `SEL_CHAR | SEL_WORD | SEL_LINE`
  (`window-copy.c:294–298`).
- `-X` names the upstream tests exercise, all of them:
  `history-top`, `start-of-line`, `begin-selection`, `copy-selection`,
  `previous-word`, `previous-space`, `next-word`, `next-word-end`,
  `next-space`, `next-space-end` (`window-copy.c:3128–3676`).
- The tests differ only by `mode-keys` and `word-separators`. The vi
  test uses the default separators (non-alphanumerics act as
  boundaries); the emacs test sets `word-separators ""` (only literal
  space separates; symbols become word-chars).
- Buffer commands: `show-buffer [-b name]`, `set-buffer [-aw] [-b name]
  [-n new] data`, `list-buffers`, `delete-buffer [-b name]`, `save-buffer
  [-a] [-b name] path`, `paste-buffer [-dpr] [-b name] [-t target]`
  (`cmd-save-buffer.c`, `cmd-set-buffer.c`).

## Wire-protocol impact

None for the acceptance tests. If we later want the client to display a
"-- COPY --" banner or a distinct cursor style *without* the server
re-drawing every frame, we can add a `SetMode Text` server→client
message, but that's an M8c optimisation and not required.

## Milestones

Each milestone is BDD-flavoured: it ends with a demoable observable
behaviour and (where possible) an upstream regress or an integration
test as its green light. Commits within a milestone stay small.

### M8a — Data model, `-X` dispatch, buffers, and the two upstream tests

*The demo:* running `nix develop --command tools/run-upstream-tests.sh
~/src/tmux` reports `pass=13 xfail=38` — `copy-mode-test-vi` graduates
from the xfail list. `copy-mode-test-emacs` stays xfailed pending
emulator tab tracking (see Acceptance targets).

Steps (roughly one commit each):

1. **Add copy-mode state to `Pane`.** Extend `Hat.Model.Pane` with
   `mode :: TVar (Maybe CopyModeState)` where `CopyModeState` carries
   `cursor :: !AbsPos`, `selection :: !(Maybe (AbsPos, SelKind))`,
   `keyTable :: !Text`. `AbsPos { row, col }` is in absolute
   grid coords (row 0 = oldest scrollback line).

2. **Add server-side paste-buffer store.** Extend `ServerState` with
   `buffers :: TVar (Seq (Text, Text))` — an ordered `[(name, body)]`
   with the newest at the front, so `paste-buffer` without `-b` grabs
   the head. Auto-name new buffers `bufferN` where N is a
   monotonically-increasing counter also on `ServerState`.

3. **Implement `copy-mode` command.** Parse `[-deuHMq] [-t target]`;
   only `-d`/`-u` (page down/up on entry) and `-t` need to do
   anything today. Set the target pane's `mode` to a fresh
   `CopyModeState` seeded from the current visible cursor and the
   current `mode-keys` option. `copy-mode` with `-q` on a pane
   already in mode → clear it. Add `word-separators` as an option
   (default: `" -_@"` per tmux's default) so the emacs test's
   `setw -g word-separators ""` lands somewhere.

4. **Wire `send-keys -X <name>` to a copy-mode command runner.**
   Extend `cmdSendKeys` to notice `-X`. Route to a dispatch table
   `Map Text (CopyModeCommand)`. Each `-X` handler takes the pane and
   its `CopyModeState`, returns a new state (or a side effect like
   "append text to the top buffer").

5. **Implement the ten `-X` commands the regress tests need**, plus
   `cancel` (which clears `mode`):
   - Motion over the absolute grid: `cursor-{left,right,up,down}`,
     `start-of-line`, `end-of-line`, `history-top`, `history-bottom`,
     `top-line`, `bottom-line`, `middle-line`.
   - Selection: `begin-selection`, `clear-selection`, `select-line`,
     `rectangle-toggle`.
   - Word/space motion respecting `word-separators`:
     `next-word`, `next-word-end`, `previous-word`,
     `next-space`, `next-space-end`, `previous-space`.
   - Yank: `copy-selection` and `copy-selection-and-cancel` — extract
     the cells covered by the current selection (row-major, join
     rows with `\n`), push onto the buffer store, clear the
     selection, and (for the `-and-cancel` variant) clear `mode`.

6. **Buffer commands**: `show-buffer [-b name]` outputs the buffer's
   body via `ROutput`, defaults to the head. `set-buffer [-a] [-b
   name] data` replaces or appends. `list-buffers` prints one line per
   buffer (`name: N bytes`). `delete-buffer [-b name]`. Register all
   under the command table.

**Verification for M8a:**
- Unit test `Hat.Server.CopyModeSpec`: build a fake pane grid, drive
  each `-X` command through pure state transitions, assert
  cursor/selection/buffer outcomes for each of the ~30 cases in the
  regress scripts.
- Integration test: run `regress/copy-mode-test-vi.sh` under
  `run-upstream-tests.sh`, confirm exit 0, remove from
  `tools/upstream-xfail.txt`. Baseline moves to `pass=13`.
  `copy-mode-test-emacs.sh` stays xfailed (tab preservation).

**Anti-patterns to avoid:**
- Do not add wire messages. Server-side state only.
- Do not couple copy-mode logic to a specific client's render loop —
  the tests never attach.
- Do not use `T.length` on wide-character rows for cursor arithmetic;
  use `Cell.width` (`Hat/Term/Cell.hs:46`).

---

### M8b — Prefix bindings and pane-in-mode key routing

*The demo:* attach a client; `prefix [` puts the active pane into copy
mode; hjkl (vi) or C-p/C-n/C-b/C-f (emacs) move the cursor; `q` /
`Escape` exit. Keys that aren't bound in the copy-mode table are
**swallowed** rather than sent to the pty (this is how upstream tmux
prevents "typing while browsing").

Steps:

1. **Route input through a pane-mode-aware table.** In
   `Hat.Server.Keys.routeKeys`, when the active pane has `mode = Just
   s`, look up the incoming key in `keymap[s.keyTable]` first; on
   miss, swallow (do not passthrough to pty). The existing prefix
   state machine stays — prefix keys still work.

2. **Populate the `copy-mode` and `copy-mode-vi` tables** in
   `defaultKeymap`. Cover the keys the tests and normal use need
   (see the reference tables in the discovery report: `h j k l w b e
   B W E g G 0 $ v V y q Space Escape` for vi;
   `C-a/C-e/C-b/C-f/C-n/C-p/M-b/M-f/M-</M->/Space/Enter/C-c` for
   emacs; arrows in both).

3. **Bind `[` at prefix to `copy-mode`** and **`]` at prefix to
   `paste-buffer`**. Update `prefixBindings` in
   `defaultKeymap`.

**Verification:**
- Integration test in `test/Hat/IntegrationSpec.hs`: drive a real
  hat client through a pty; press `C-b [`, then `h`, then `y`.
  Assert the pane's contents don't reflect the `h`/`y` (they were
  swallowed) and `show-buffer` returns the yanked selection.

---

### M8c — Selection rendering (server side, no wire change)

*The demo:* attach a client, enter copy mode, press `v`, move the
cursor — the selected span visibly reverse-videos on screen. The copy
cursor also shows up (distinct from the shell cursor).

Steps:

1. **Overlay the selection in `Hat.Server.Render`.** When the active
   pane is in copy mode, after `overlayGrid` builds the base frame,
   compose each selected cell's style with `reverse = True` (SGR 7).
   The style already exists in `Hat.Term.Cell.Style`.

2. **Draw the copy cursor** at the copy-mode cursor position instead
   of the pane's shell cursor. Piggyback on the existing `CursorAt`
   `DrawOp`.

3. **Handle scrolled viewport.** When the copy-mode cursor is outside
   the visible grid (moved into scrollback), scroll the viewport
   accordingly and render the scrollback cells into the base frame.
   The emulator already has `scrollbackLine` and `screenRowText` —
   compose them into a synthetic grid for that pane's slot.

**Verification:**
- Golden test in `Hat.Term.GoldenSpec`: feed a canned pty stream,
  enter copy mode, drive `history-top`, `begin-selection`, four
  `next-word`s, then render one frame. Compare to a checked-in
  golden `.result` file.

---

### M8d — Buffer plumbing beyond bare yank

*The demo:*
- `hat save-buffer /tmp/x` writes the top buffer to disk.
- `prefix ]` (bound in M8b) pastes into the active pane's pty as if
  typed.
- `hat copy-pipe 'wc -c'` yanks selection and pipes it to a shell.

Steps:

1. **`save-buffer [-a] [-b name] path`** — write buffer body to disk.
   `-a` appends. Path expansion for `~`.
2. **`paste-buffer [-dpr] [-b name] [-t target]`** — write the buffer
   body into the target pane's pty via `Hat.Pty.writePty`. `-d`
   deletes after pasting. `-p` bracketed paste (wraps in
   `\033[200~`/`\033[201~`). `-r` replaces `\r` with `\n`.
3. **`copy-pipe [-C] <cmd>` and `copy-pipe-and-cancel`** — like
   `copy-selection`, but also spawn `sh -c <cmd>` with the selection
   on stdin (via `readCreateProcess`).

**Verification:**
- Integration test: create a session, feed it a shell command,
  yank into a buffer, `save-buffer /tmp/x`, assert file contents.
- Integration test: `paste-buffer` writes to a pty running `cat`,
  assert `cat`'s output.

---

### M8e — `pipe-pane` and status-line "in mode" formats

*The demo:* `hat pipe-pane 'tee /tmp/log'` tees an interactive pane's
output to a file until `hat pipe-pane` (with no args) turns it off.
Status line shows `[copy]` for panes in copy mode.

Steps:

1. **`pipe-pane [-IOo] [-t target] [shell-command]`** — start a
   subprocess. `-O` (default): pane pty output → subprocess stdin.
   `-I`: subprocess stdout → pane pty. Both allowed simultaneously.
   Empty command / no args: stop the pipe on the target pane. Store
   the subprocess handle in the pane so kill-pane can reap it.

2. **Add `pane_in_mode` and `pane_mode`** to `sessionFormatEnv` /
   `windowFormatEnv`. `pane_in_mode = "1"` if the pane's `mode` is
   `Just _`, `"0"` otherwise. `pane_mode = "copy-mode"` or empty.
   Also add `copy_cursor_x` / `copy_cursor_y` / `copy_cursor_line`
   for completeness.

3. **Optional: `SetMode Text` wire message** so the client can print
   a status banner without waiting for the next Draw. Only add this
   if the render feels laggy in M8c.

**Verification:**
- Integration test for `pipe-pane`: send text through a pane,
  assert the tee file contains it.
- Format spec test: assert `#{?pane_in_mode,COPY,--}` expands
  correctly in both states.

---

## Summary of scope shifts vs. tmux

- Copy mode is **per-pane** in tmux; we keep that.
- `mode-keys` is server-scoped in tmux (well, session-scoped with `-w`);
  hat already only has a server-level `modeKeys`. Fine for now.
- `word-separators` — new option in hat; default `" -_@"` (upstream default).
- Rectangle mode, incremental search (`/`, `?`, `n`, `N`), search-back,
  and mouse selection: stubbed but not implemented in M8a-b. They can
  land as separate small commits once selection semantics stabilise —
  none are required for the upstream regress green light.

## Not part of M8 (deliberately)

- `choose-tree` / `display-menu` / `display-popup` — that's M9.
- OSC 52 clipboard export via `set-buffer -w` — needs a passthrough
  path to the outer terminal; skip in v1.
- `pipe-pane` with hooks (`pane-focus-in/out`) — hooks are M9.

## Order of operations

Start with M8a as one plan-master issue with sub-commits 1–6 above.
Don't graduate the two upstream tests from xfail until M8a step 5's
integration tests pass locally. M8b–e can proceed in order or be
interleaved once M8a is green.
