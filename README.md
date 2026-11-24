# Hat

A tmux-compatible terminal multiplexer, written in Haskell around
[libghostty-vt](https://github.com/ghostty-org/ghostty). It reads your
`tmux.conf`, can be upgraded in place without interrupting your programs,
and brings your sessions back after a reboot.

[The human would like to add: It's hilarious to me that I found out about
libghostty-vt and Hashimoto's [Superlogical](https://www.superlogical.com/)
about two weeks after starting Hat. I wondered if I should just give up; who
can compete with oodles of industry connections, VC money, and all that genuine
hard work? But I've watched the retvrn to the console over the last decade with
delight, and I'm happy that terminal multiplexers are apparently a hot thing
right now. LLMs mean we can all scratch our own itches. Let's go!]

## Status

**Alpha, and daily-driven.** Hat has been its author's only multiplexer since
the first week of development, so it is heavily dog-fooded — but by one
person. It began as "whatever one `tmux.conf` needs"
([FEATURES.md](FEATURES.md) is that original list) and has since grown to
cover most of tmux: its option table, its default key bindings, mouse, menus,
popups, floating panes, and control mode. What's still missing is listed under
[Rough edges](#rough-edges).

**Where it is going.** The experiment is over and Hat stays. The aim now is a
multiplexer other people might want:

1. A drop-in replacement for tmux, for any config and not just the author's.
2. Documentation of its own.
3. Performance within 20% of tmux on any benchmark. It is not there yet.

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

> [!NOTE]
> **Going back a version.** `hat restart` into a newer build keeps your
> programs running. Into an *older* build it does too, unless the handover
> format changed in between: then the restart is refused, with the running
> server left as it was. To go back past such a change, exit the server and
> start the old one; your sessions are restored, your running programs are
> not. ([#1](https://github.com/chreekat/hat/issues/1))

## Why Hat

- **Built to be upgraded.** Your running programs survive an upgrade, and your
  sessions survive a reboot.
  - `hat restart` upgrades in place.
  - Sessions survive reboots.
  - Everything one version hands another is versioned from day one
    ([CLAUDE.md](CLAUDE.md) has the rules).
- **libghostty-vt inside.** Each pane is emulated by Ghostty's VT core, so
  what renders in a pane is what renders in Ghostty.
- **Follows your desktop theme.** On GNOME (or anything exposing its
  `color-scheme` setting), Hat restyles its own chrome when you flip
  light/dark, sources a per-scheme config of yours, and tells apps that
  subscribe to theme reports (DEC mode 2031).
- **Speaks tmux.** Same commands, targets, options, key tables, format
  strings, `if-shell`, `{ }` blocks, hooks, copy mode (emacs and vi), paste
  buffers, mouse, menus, popups, floating panes, choose-tree and customize
  mode. Point it at your existing `~/.tmux.conf`.
- **Fails loudly.** An option or command Hat doesn't implement is an error
  you see, never something silently stored and ignored.

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
- **October** — breadth: tmux's whole option table, mouse, menus, popups,
  floating panes, control mode, and a fuzzy finder in the tree modes.
- **November** — tmux's default key bindings line for line; a restart into a
  build that can't take over is refused instead of attempted; libghostty-vt
  1.0.0; copy mode on libghostty's own snapshots.

## Getting started

Hat builds with Nix or Cabal. To try it on x86_64 or aarch64:

```sh
nix run github:chreekat/hat                       # start a server if needed, and attach
nix run github:chreekat/hat -- -f ~/.tmux.conf    # ...using your tmux config
```

...or clone and use cabal.

For a static musl binary you can copy to any Linux machine,
`nix build github:chreekat/hat#hat-static` — but expect it to take hours: with
nothing cached, it builds GHC from source.

The key bindings are tmux's: prefix `C-b`, `d` detach, `c` new window, `%`/`"`
split, `[` copy mode, `:` command prompt, `?` to list them all. Commands also
work from a shell: `hat list-sessions`,
`hat display-message -p '#{session_name}'`.

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

Invocation flags are `-f`, `-L`, `-S`, `-C` (control mode) and `-V` (print the
version).

### Where Hat differs from tmux

- **Some defaults are Hat's own**: `escape-time 0`, `history-limit 50000`,
  `display-time 3000`, `extended-keys on`, every terminal drawn in RGB, a
  plain `hat` attaching to an existing session, and its own status-line
  colours and terminal title. The full table, with tmux's value beside each,
  is in [docs/options-audit.md](docs/options-audit.md) under "Hat's own
  defaults"; set any of them back in your config.
- **Searching a tree mode is a fuzzy finder.** In choose-tree, choose-buffer
  and choose-client, typing a search narrows and ranks the list, and Enter
  picks the best match. tmux jumps between substring matches.
- **A pane flashes when it takes focus**, so you can see where you landed.
- **The active pane's border arrow** is drawn Hat's way.
- **Left out by design**: locking (`lock-server` and its options) and
  `assume-paste-time`. Hat turns on bracketed paste and never guesses a paste
  from key timing.

### Upgrading in place

```sh
hat restart             # re-exec the server and every attached client
                        # into the `hat` found on PATH
hat restart ./hat       # ...or into a specific binary
hat restart -C          # drop scrollback across the handover
```

A new build takes over from any older one. Before the handover the server
asks the target binary whether it can read this server's state, and refuses
the restart if it can't (see the downgrade note under [Status](#status)).
Popups do not survive a restart. `restart-server` and `restart-client` are
the lower-level halves, restarting only the server or only the current
client.

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

`set -s desktop-theme off` stops following the desktop.

## Rough edges

- **Slower than tmux.** Hat is comfortable for daily use, but when last
  compared (September 2026) it spent about 13 times tmux's CPU per keystroke.
- **Not all of tmux.** Missing: `find-window`, `suspend-client`,
  `switch-mode`, clicking and dragging a pane's scrollbar, read-only clients
  (`attach -r`), and the options `codepoint-widths`,
  `variation-selector-always-wide` and `switch-mode-match-style`. A few
  other mouse features are missing too. Because Hat fails
  loudly, a config using any of these reports an error rather than
  half-working.
- **No manual.** The section above plus `man tmux` is it. There is no
  `--help`; unknown arguments are treated as a tmux command.
- **No releases.** Build from source with Nix; there are no tagged versions
  or distro packages.
- **Known bugs** (tracked in [`bugs`](bugs)): a rare first attach stays
  blank; after a restart a pane's cursor can sit in the wrong place; under
  heavy load stray characters can linger on screen
  until the window is switched away and back; `hat switch-client` run from a
  shell outside any client moves no one.
- **tmux's regress suite** still has 29 scripts marked expected-to-fail in
  [`tools/upstream-xfail.txt`](tools/upstream-xfail.txt), each with a note
  on why. Some never will pass: they assert details of tmux's own screen
  storage.

## Contributing

Read [CLAUDE.md](CLAUDE.md) first: build and test norms (everything through
`cabal` inside the Nix dev shell; `just test` for the suite, `just ghcid` for
the typecheck loop, `cabal bench hat-perf` for the performance gate against
`tools/bench/perf-baseline`, and `HAT_UPSTREAM=1 cabal test hat-upstream` for
tmux's regress suite), and the compatibility rules any change to a
serialized format must follow. Design is in
[ARCHITECTURE.md](ARCHITECTURE.md), the original scope in
[FEATURES.md](FEATURES.md).
