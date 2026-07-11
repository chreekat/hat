# M11 — tmux-resurrect save & restore

Phase 2 of "daily driver": make the author's installed tmux-resurrect
(`~/.tmux/plugins/tmux-resurrect`) save and restore a hat session tree.
Depends on Phase 1 ([[M10-tmux-conf-compat]]) — resurrect is sourced from
`~/.tmux.conf` line 174 and its restore replays `new-window` /
`split-window` / `select-pane`, which M10 makes faithful.

## Acceptance target

`prefix M-s` writes a state file; `kill-server`; restart; `prefix M-r`
rebuilds every session, window, pane, working directory, and layout. The
green light is resurrect's own `save.sh` + `restore.sh` run end-to-end
against hat in an integration harness.

## Facts of the ground

**What resurrect actually uses** (from its scripts — verified, not
inferred):
- **Entrypoint** `resurrect.tmux`: only `set-option -gq @resurrect-*`,
  `bind-key … run-shell save.sh|restore.sh`, and `get_tmux_option`
  (which reads options back via `show-option -gqv`). **No `set-hook`** —
  hooks are a *continuum* dependency and are not installed.
- **`save.sh`** dumps `list-windows`/`list-panes` with `-F` formats
  (`pane_format`/`window_format`/`grouped_sessions_format`/
  `state_format`) and `capture-pane`. Fields referenced:
  `session_name`, `window_index`, `window_name`, `window_active`,
  `window_flags`, `window_layout`, `automatic_rename`, `pane_index`,
  `pane_id`, `pane_current_path`, `pane_current_command`, `pane_pid`,
  `history_size`, `cursor_x`, `cursor_y`, `session_grouped`.
- **`restore.sh`** recreates via `new-window`, `split-window`,
  `select-layout -t "$session:$window" "$window_layout"`, `move-window`,
  `send-keys`, `switch-client`, `has-session`.

**Hat internals:**
- `capture-pane` present; `list-panes`/`list-windows` present but with a
  partial field set and unverified `-a`.
- `@`-options round-trip through `Options.user` (`set`/`show`), finalized
  by [[M10-tmux-conf-compat]] M10a/M10e.
- `targetPane` token resolution (`sess:win.pane`, `%N`) lands in M10b —
  restore's `-t "$session:$window"` needs it.
- **No `window_layout` support** and **no layout-string codec** — the
  main new work here.

## Sub-milestones

### M11a — `@`-option round-trip

*The demo:* `set -gq @resurrect-save M-s`; `show -gqv @resurrect-save`
returns `M-s`; resurrect's `get_tmux_option` reads its config.

Verify/complete `set-option -gq` (quiet) and `show-option -gqv`
(value-only, quiet) against `Options.user`. Mostly wiring on top of M10.

### M11b — Rich `list-panes`/`list-windows -aF` (save side)

*The demo:* running resurrect's `save.sh` against hat produces a
complete state file — every pane, cwd, command, and pid present.

1. Implement the `-a` flag (all sessions/windows) on both commands.
2. Add every format field resurrect dumps (see Facts): `pane_pid`,
   `pane_current_command`, `cursor_x`/`cursor_y`, `history_size`,
   `automatic_rename`, `window_active`, `session_name`. `session_grouped`
   can be a constant `0` (hat has no session groups).
3. **Save-side harness:** run `save.sh` against a scripted hat session;
   assert the produced state file has a row per pane with correct cwds.

### M11c — `window_layout` emit

*The demo:* `list-windows -F '#{window_layout}'` prints a tmux-format
layout string (`<checksum>,WxH,x,y{...}`) for each window.

Build the layout-string codec's **emit** half: serialize hat's layout
tree to tmux's `WxH,x,y` grammar with the `{…}` (h-split) / `[…]`
(v-split) nesting, plus tmux's layout checksum. Shares the layout-tree
representation with [[M10-tmux-conf-compat]] M10g (named layouts).

### M11d — `select-layout "<string>"` + `move-window` (restore side)

*The demo:* `select-layout "<string>"` reshapes a window to match a
saved layout; `move-window -s -t` renumbers windows.

Parse the layout string (the codec's other half) and apply it to the
window's layout tree; implement `move-window`. Restore's `-t
"$session:$window"` targets rely on M10b.

### M11e — End-to-end save → kill → restore

*The demo (acceptance):* build a session (named windows, splits, cwds),
`prefix M-s`, `kill-server`, restart, `prefix M-r` → the tree returns.

Scripted integration test driving resurrect's real `save.sh` and
`restore.sh`/`restore.exp` against hat. Assert sessions/windows/panes/
working-dirs match. Pane *contents* restore (`capture-pane` replay) is
optional and can be a follow-up.

## Not part of M11

- Restoring running programs (resurrect's process strategies) beyond
  what `send-keys` of the command line achieves.
- Session groups (`session_grouped` reported as `0`).
- tmux-continuum autosave (needs hooks) — separate, and not installed.
