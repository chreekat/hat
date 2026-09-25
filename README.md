# Hat

A tmux-compatible terminal multiplexer, written in Haskell around
[libghostty-vt](https://github.com/ghostty-org/ghostty). It reads your
`tmux.conf`, can be upgraded in place without interrupting your programs,
and brings your sessions back after a reboot.

## Status

**Alpha, and daily-driven.** Hat has been its author's only multiplexer since
the first week of development, so it is heavily dog-fooded — but by one
person, against one `tmux.conf`. The scope is literally that config (see
[FEATURES.md](FEATURES.md)): everything it uses works, and much of tmux beyond
it does too. What's still missing is listed under [Rough edges](#rough-edges).

**Written by an agent.** About 99% of the code was written by a coding agent.
The author supplied the features wanted, the architecture and testing
principles ([ARCHITECTURE.md](ARCHITECTURE.md), [CLAUDE.md](CLAUDE.md)), and a
lot of close back-and-forth — but reviewed very little of the code line by
line. Good results came from being meticulous about testing strategy and
from shipping small, user-visible improvements that were immediately
dog-fooded. The tests are unit, property, and integration tests that drive the
real binary through a pty; tmux's own `regress/` suite run against Hat; and a
performance gate that counts instructions retired per unit of work (keystrokes
typed, scrollback lines carried across a restart), so a regression shows up the
same on any machine. Nonetheless, if LLM-written code is a dealbreaker for you,
this is not your multiplexer.

> [!WARNING]
> While updating this README, we found that the forward-compatibility
> principle had been miscommunicated, and **server downgrades are currently
> broken**: `hat restart` into an *older* binary whose handover format
> predates the running one can't read the handover, falls back to a clean
> restart, and kills every pane. Upgrades are fine. Until this is fixed, don't
> restart into an older build with running programs you care about.
> Tracked in [#1](https://github.com/chreekat/hat/issues/1).

## Why Hat

- **libghostty-vt inside.** Each pane is emulated by Ghostty's VT core, so
  what renders in a pane is what renders in Ghostty, and it's fast. The
  renderer on top diffs frames by row provenance, so a busy pane doesn't
  repaint the screen.
- **`hat restart` upgrades in place.** The server `exec`s a new binary and
  hands over every pane's pty, child process, screen, and scrollback; attached
  clients re-exec along with it. Your shells, editors, and
  long-running jobs never notice. With tmux, upgrading means killing
  everything.
- **Sessions survive reboots.** The server continuously mirrors the
  session/window/pane tree (names, layouts, working directories) into SQLite.
  Relaunch after `kill-server` or a reboot and it's all back, with editors,
  pagers, and monitors re-run in place. Past trees are kept as history
  you can browse and restore. No tmux-resurrect, no save key.
- **Built to be upgraded.** The three things one binary version hands another
  — the client/server protocol, the SQLite store, and the restart handover —
  are all versioned and pinned by golden test corpora. The protocol and store
  tolerate both older and newer peers; the handover only older ones, for now
  (see the warning above). The previous two points are only safe because of this; see
  [CLAUDE.md](CLAUDE.md) for the rules.
- **Follows your desktop theme.** On GNOME (or anything exposing its
  `color-scheme` setting), Hat restyles its own chrome when you flip
  light/dark, sources a per-scheme config of yours, and tells apps that
  subscribe to theme reports (DEC mode 2031).
- **Speaks tmux.** Same commands, targets, key tables, format strings,
  `if-shell`, `{ }` blocks, hooks, copy mode (emacs and vi), paste buffers,
  and choose-tree. Point it at your existing `~/.tmux.conf`.
- **Fails loudly.** An option or command Hat doesn't implement is an error,
  never silently accepted — so a config that loads cleanly is a config that
  does what it says.

## History

- **2026-06-25** — first commit. Survey says: no viable Haskell terminal
  emulator, so wrap a C one behind our own interface.
- **2026-07-08** — the whole skeleton lands in a day: pty, emulator
  (libvterm), wire protocol, server/client, windows and panes, the command and
  format languages, config loading, and the harness for tmux's regress suite.
  Daily use starts that week.
- **July** — copy mode, the command prompt, full tmux.conf compatibility,
  resurrect primitives; then continuous SQLite persistence (07-13) and
  in-place restart (07-17).
- **2026-08-02** — libvterm swapped for libghostty-vt behind the same seam.
- **August** — hooks and alerts, snapshot history, desktop theme following.
- **September** — performance: frame diffing, the emulator bridge, and a
  much faster `hat restart`.

## Getting started

Hat builds with Nix. To try it:

```sh
nix run github:chreekat/hat                       # start a server if needed, and attach
nix run github:chreekat/hat -- -f ~/.tmux.conf    # ...using your tmux config
```

For a static musl binary you can copy to any Linux machine,
`nix build github:chreekat/hat#hat-static` — but expect it to take hours: with
nothing cached, it builds GHC from source.

The defaults are tmux's: prefix `C-b`, `d` detach, `c` new window, `%`/`"`
split, `[` copy mode, `:` command prompt. Commands also work from a shell:
`hat list-sessions`, `hat display-message -p '#{session_name}'`.

## Documentation

Hat has no manual of its own yet. `man tmux` is the reference for commands,
options, and formats; this section covers where Hat differs or adds.

### Files and environment

| What | Where |
|---|---|
| Config | `~/.config/hat/hat.conf`, or `-f path`. tmux syntax. `~/.tmux.conf` is *not* read by default. |
| Socket | `$TMUX_TMPDIR/hat-$UID/default` (default `/tmp/hat-$UID/`). Pick another with `-L name` or `-S path`. |
| Server log | `server.log` beside the socket, one JSON event per line. |
| Session store | `$XDG_DATA_HOME/hat/<socket>.db` (default `~/.local/share/hat/`), one per socket name. Override the directory with `HAT_STORE_DIR`. |
| In a pane | `TMUX` and `TMUX_PANE` are set as tmux sets them, so tmux-aware tools work; `HAT` and `HAT_PANE` mirror them. |

Invocation flags are `-f`, `-L`, `-S`, and `-C` (control mode, partial).

### Upgrading in place

```sh
hat restart             # re-exec the server and every attached client
                        # into the `hat` found on PATH
hat restart ./hat       # ...or into a specific binary
hat restart -C          # drop scrollback across the handover
```

A new build takes over from any older one (but see the downgrade warning
under [Status](#status)). `restart-server` and `restart-client` are the
lower-level halves, restarting only the server or only the current client.

### Persistence

On by default; `HAT_PERSIST=0` in the server's environment turns it off.

- Restored panes start a fresh shell in their saved directory, unless the
  pane was running a whitelisted program, which is re-run with its
  arguments. Set the whitelist (space-separated) with
  `set -g @restore-commands "vim nvim less htop"`; the default covers common
  editors, pagers, and monitors.
- Scrollback is not persisted across a server exit, only across
  `hat restart` ([#2](https://github.com/chreekat/hat/issues/2)).
- `list-snapshots` shows the stored history generations; `restore-snapshot N`
  recreates one beside the current tree, renaming sessions whose names are
  taken. Keep `N` generations with `set -g @snapshot-limit N` (default 10;
  `0` turns history off).

### Theme following

With `gsettings` available, Hat tracks
`org.gnome.desktop.interface color-scheme`. On a change it applies a default
light or dark palette to its chrome (your own `set` still wins) and sources a
file of your choosing:

```tmux
set -g @color-scheme-dark  ~/.config/hat/dark.conf
set -g @color-scheme-light ~/.config/hat/light.conf
```

## Rough edges

- **Not all of tmux.** Missing entirely: mouse support, `display-popup`,
  `display-menu`, `display-panes`, `find-window`, `list-keys`,
  `synchronize-panes`, OSC 52 clipboard, `window-size`, and
  `terminal-overrides`. Control mode is partial. Because Hat fails loudly, a
  config using any of these reports an error rather than half-working.
- **No manual.** The section above plus `man tmux` is it. There is no
  `--help`; unknown arguments are treated as a tmux command.
- **No releases.** Build from source with Nix; there are no tagged versions
  or distro packages.
- **Known bugs** (tracked in [`bugs`](bugs)): a pane can't be closed while a
  backgrounded daemon still holds its pty open; a rare first attach stays
  blank.
- **tmux's regress suite** still has scripts marked expected-to-fail in
  [`tools/upstream-xfail.txt`](tools/upstream-xfail.txt), each with a note
  on why.

## Contributing

Read [CLAUDE.md](CLAUDE.md) first: build and test norms (everything through
`cabal` inside `nix develop`; `cabal test` for the suite,
`cabal bench hat-perf` for the performance gate against
`tools/bench/perf-baseline`, `tools/run-upstream-tests.sh ~/src/tmux` for
tmux's), and the
compatibility rules any change to a serialized format must follow. Design is
in [ARCHITECTURE.md](ARCHITECTURE.md), scope in [FEATURES.md](FEATURES.md).
