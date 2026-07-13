# Continuous session persistence (Firefox model)

Kill the server, restart it, get the whole session/window/pane tree back —
automatically, without tmux-resurrect scripting. The model is Firefox: quit
reopens the same windows.

## Decisions

- **No scrollback.** Panes restore as fresh shells; history is not saved.
- **Fresh shell per pane**, started in the pane's saved working directory.
  Re-running the foreground program (vim, htop) is a later follow-up.
- **Always-on.** No config gate. The store is per-socket and tests use
  isolated private HOMEs, so runs never cross-contaminate.
- **SQLite** via `sqlite-simple` (bundles its own SQLite through
  `direct-sqlite`; no system lib). Store lives in a reboot-surviving location,
  `$XDG_DATA_HOME/hat/<socket-name>.db`, keyed per socket.

## Forward/backward compatibility

The compatibility surface is the schema. Rules:
- A `meta` table carries `schema_version`.
- Core columns never change meaning. Evolving fields go in a per-row JSON
  `extra` column (aeson is already a dependency).
- DDL is additive only (`CREATE TABLE IF NOT EXISTS`, add columns/tables).
- Reads select explicit core columns and default anything missing, so a new
  binary reads an old store and an old binary reads a new one.

## How restore works

Restore is a *replay*, not a second tree-builder. `layoutFromString` already
ignores saved pane ids and maps geometry onto an ordered pane list, so a window
is rebuilt by: spawn its panes in layout order (first via `new-window`, rest via
`split-window`), each in its saved cwd, then `select-layout "<string>"`, then
`select-window` / `select-pane` to restore focus. This reuses the existing,
tested command paths.

## What is captured

- Session: name, startCwd, current window index, window order.
- Window: index, name, `emitLayout` string, active-pane ordinal.
- Pane: ordinal within the window's layout order, live cwd (`paneCurrentPath`).

## Milestones

Each milestone is demoable.

### A — Persistence substrate
Pure `Snapshot` type + SQLite schema + store codec.
- Add `sqlite-simple` dependency.
- `Hat.Persist`: `Snapshot`/`SessionSnap`/`WindowSnap`/`PaneSnap`, schema
  bootstrap, `saveSnapshot`/`loadSnapshot`.
- Property test: round-trip a snapshot through a temp DB (Arbitrary + shrink).
Demo: a snapshot survives a save/load through SQLite unchanged.

### B — Continuous save
- `captureSnapshot :: ServerState -> IO Snapshot` (reads the STM tree,
  `emitLayout` per window, `paneCurrentPath` per pane).
- Background saver thread: coalescing timer, structural-change detection,
  full-snapshot write. Wired into `runServer`.
- Guaranteed final save on shutdown (`waitIdle` exit and `kill-server`).
Demo: drive a running server; the DB tracks the tree.

### C — Restore
- `restoreSnapshot :: ServerState -> Snapshot -> IO ()` replays the creation
  commands.
- Hook into startup after config load, gated so exit-when-empty does not fire
  early.
Demo: rebuild a tree from a snapshot in a test.

### D — End-to-end + compatibility
- Integration test: build a multi-session/window/pane tree, `kill-server`,
  restart, attach, assert the tree returned (names, layout, cwds).
- Compatibility test: feed the reader an older/newer store variant (extra JSON
  keys, missing optional column) and assert it tolerates them.
Demo: kill and restart hat; everything is back.

### E — Restore running commands
Bring back the foreground program a pane was running, not just a shell.
- At capture, read each pane's foreground process command (via the pty's
  foreground pgid → `/proc/<pid>/{cmdline,cwd}`) and store it in the pane's
  `extra` JSON (keeping v1's core columns untouched — this is an additive
  compat change).
- A **whitelist** of commands worth re-running (`vim`, `nvim`, `top`, `htop`,
  `atop`, …), configurable via an `@`-option. A restored pane whose captured
  command matches the whitelist re-runs it (`split-window <cmd>` /
  `new-window <cmd>`); everything else falls back to a fresh shell.
- Old stores without the command field, and commands not on the whitelist,
  degrade cleanly to a shell.
Demo: a pane running vim comes back running vim; a random shell comes back as a
shell.
