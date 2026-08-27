# HAT

A Haskell-Adapted Terminal multiplexer.

Sorry for the slop.

Stop reading now.

No, really.

Haskell around libghostty-vt; tmux-shaped
commands, config, and key bindings. See ARCHITECTURE.md for the design and
FEATURES.md for the scope.

## Status

Ready for alpha testing. Current priorities:

* add missing tmux features on demand
* performance and benchmarking
* fix bugs found in alpha

Not there yet: mouse, hooks, and popups. Upstream regress tests still fail;
the count going down is the progress metric.

## Try it

```sh
nix develop              # or `direnv allow` if you use direnv
cabal build hat
cabal exec hat           # attach (autostarts the server)
```

Prefix is `C-b` by default. `d` detach, `c`/`n`/`p`/`l`/`0-9` windows,
`%`/`"` split, arrows navigate panes, `z` zoom, `x` kill pane, `w`
choose-tree. `[` enters copy mode (emacs motions by default, or vi with
`set -g mode-keys vi`; `Enter` copies the selection), `]` pastes. `:`
opens the command prompt.

Config is read from `~/.config/hat/hat.conf` (or `-f path`) using tmux
syntax: `set -g prefix C-Space`, `bind`, `unbind`, `source-file`,
`if-shell`, format strings in the status line, and so on. Commands also
work from a shell: `hat list-sessions`, `hat kill-server`,
`hat display-message -p '#{session_name}'`.

Sockets live in `$TMUX_TMPDIR/hat-$UID/` (default `/tmp/hat-$UID/`);
pick one with `-L name` or `-S path`. The server logs JSON events to
`server.log` next to the socket.

## Tests

```sh
cabal test                                # unit + property + integration
./tools/run-upstream-tests.sh ~/src/tmux  # tmux's regress suite (xfail-tracked)
```

Both run inside `nix develop` — the devShell provides libghostty-vt plus the
coreutils/sed/awk/... that the upstream regress scripts assume in
`/bin:/usr/bin`.

The integration tests drive the real binary through a pty: detach and
reattach, two clients on one session running vim, htop, splits, and a
config with a custom prefix. `cabal run gen-fixtures` regenerates the
emulator golden files.

## Contributing

See [CLAUDE.md](CLAUDE.md) for the build/test norms and — importantly — the
forward/backward-compatibility rules that any change to a serialized format
(client↔server wire, persistence store, reload handover) must follow. In short:
everything runs through `cabal` inside `nix develop`, the suite stays fast and
green, and any format crossing a version boundary carries a version envelope,
evolves additively, and is pinned by a golden-byte test.
