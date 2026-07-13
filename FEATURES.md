# HAT Features

A list of features important for HAT to be a viable tmux replacement for *me*
(b@chreekat.net). Derived from `man tmux`, browsing `~/src/tmux` (upstream
source), and — most importantly — reading my own `~/.tmux.conf` and `~/.tmux/`
helpers. The config is the ground truth: anything I bind, I use.

Features are grouped by priority. P0 = "if this doesn't work, HAT isn't tmux."
P1 = "I'd notice immediately if it were missing." P2 = "I'd notice within a
week." P3 = "nice to have, can defer."

## Primary use case

I use tmux mainly to keep persistent sessions, windows, and panes for my
common working contexts — one session per project or recurring activity,
each window already named and placed, working directories preserved across
reboots (via tmux-resurrect). I detach and reattach all day long, almost
always to the same local terminal. SSH into a remote tmux is rare but not
zero.

Implications for scope:
- Persistence-as-organization is the dominant value. Save/restore is central.
- Network-disconnect survival is a happy side-effect, not the load-bearing
  reason. Features needed *only* for the SSH case (OSC 52 clipboard in
  particular) are not P1.
- Bandwidth/latency optimization of the wire protocol is not a v1 concern;
  client and server are almost always on the same machine.

## P0 — Core multiplexer model

These are the load-bearing architectural features. Everything else assumes them.

- **Detach and reattach.** Sessions outlive the client. `hat attach`,
  `prefix d`. The value is keeping pre-arranged work contexts persistent;
  reboot survival via a resurrect-equivalent is the corollary that matters
  most.
- **Server/client split over a Unix socket.** A long-lived server process owns
  sessions and PTYs; clients are thin renderers. Socket lives somewhere
  predictable (`$TMUX_TMPDIR` or `/tmp/hat-$UID/`).
- **Sessions, windows, panes** as a strict three-level hierarchy. Each gets a
  stable ID (`$1`, `@1`, `%1`) that doesn't change for its lifetime.
- **PTY allocation per pane** with proper signal forwarding, window-size
  propagation (`TIOCSWINSZ`), and `TMUX_PANE` / `TMUX` env vars exported.
- **Multiple simultaneous clients** attached to one session. The other killer
  feature — pair programming, attaching from a second terminal, the 👀
  indicator in my status line all depend on it.
- **VT100/xterm terminal emulation** in each pane: cursor positioning, SGR
  attributes, alternate screen buffer, scroll regions, OSC sequences for
  title, mouse reporting passthrough. Enough that vim, less, htop, fzf, and
  ncurses apps all work.
- **Scrollback buffer per pane**, large (`history-limit 50000`). Survives the
  full lifetime of the pane.
- **Configuration file** loaded at server start. Mine is ~150 lines and almost
  every line matters.

## P1 — Daily-driver features (I'd notice within minutes)

### Keybindings and the prefix

- **Prefix key**, configurable. Mine is `C-space`. The default `C-b` is fine
  too; the point is it must be remappable.
- **`send-prefix`** to pass the prefix through to the inner program.
- **Vim-mode key tables**: a `copy-mode-vi` table separate from `prefix` and
  `root` tables.
- **`bind` / `unbind`** at runtime (so `r` can reload the config) with optional
  **`-N note`** for self-documenting bindings.
- **Multi-table dispatch**: `prefix`, `root`, `copy-mode-vi`, plus the ability
  to bind under `M-Up` etc. at the `root` level (no prefix) for fast resize.
- **`?`** lists current bindings.

### Sessions

- **`new-session`**, **`attach-session`**, **`kill-session`**,
  **`list-sessions`**, **`rename-session`** (`$`).
- **`switch-client -l`** (last session). I have this on both `b` and `C-s`.
- **`choose-tree`** — interactive session/window picker with type-to-search.
  Mine has `/` opening it pre-typed in search mode (`choose-tree -GZw` then
  `send-keys /`).
- **`attach-session -c <path>`** — change the session's default working
  directory.

### Windows

- **`new-window`**, **`kill-window`** (`&`), **`rename-window`** (`,`),
  **`list-windows`**.
- **Window navigation**: `n`/`p`/`C-n`/`C-p` for next/previous,
  `last-window` (`l`, also `a` for me), `0`–`9` for direct index,
  `M-n`/`M-p` for next-window-with-activity (`-a`).
- **`base-index 1`** — windows start at 1, not 0. `pane-base-index 1` too.
- **`new-window -c "#{pane_current_path}"`** — open at the pane's CWD. This is
  on `C` (capital) for me.
- **Activity / bell tracking** per window, surfaced in the status line.
- **`set-titles on`** — propagate window/pane title to the outer terminal's
  title bar (urxvt, foot, etc.).
- **`break-pane`** (`!`) and the inverse via `join-pane` (see panes).

### Panes

- **`split-window -h`** / **`split-window -v`**, including `-b` for "split
  before" and `-c <path>` for working directory. My `v`/`s` bindings depend on
  all four combinations.
- **Directional pane navigation**: `select-pane -L/-D/-U/-R`. I have these on
  `h`/`j`/`k`/`l` and also their `C-` variants.
- **Move pane to edge**: `split-window -fh[b]` + `swap-pane -t !` + `kill-pane
  -t !` composition. The `-f` flag (full-width/height split of the window, not
  just the current pane) is essential — see my `H`/`J`/`K`/`L` bindings.
- **`swap-pane`**, **`kill-pane`** (`x`), **`break-pane`**, **`join-pane`**
  (interactively via `choose-window 'join-pane -hs "%%"'` for my `V`/`S`).
- **Pane zoom toggle** (`z`) and **zoom by target** (`resize-pane -t ! -Z` for
  my `Z` — zoom the *alternate* pane).
- **Resize**: `resize-pane -U/-D/-L/-R [N]`. Available without prefix via
  `M-Arrow` and `C-Arrow`.
- **`last-pane`** (`;`).
- **Mark pane / marked pane** (`m`/`M`, target token `~`). Used by `join-pane`.
- **`clear-history`** (`C-k` for me).
- **Pane border styling**, including active vs. inactive, and
  `pane-border-indicators` showing direction arrows. I have a dark/light
  theme toggle bound to `R`.
- **`aggressive-resize`** — size to the smallest *attached* client per window,
  not per session.

### Copy mode and buffers

- **vi-style copy mode** entered with `[`. Motions, visual selection, search
  forward/back. Mine is bound `set -gw mode-keys vi`.
- **Paste buffers**, a stack of them, with `paste-buffer` (`]`), `list-buffers`
  (`#`), `delete-buffer` (`-`), and **interactive paste picker** (`=`).
- **`copy-pipe`** and **`copy-pipe-and-cancel`** — pipe the selection to a
  shell command. Mine pipes to `xclip -selection clipboard` on `y`/`Y`.
- **`copy-selection`** without exiting copy mode (my `Enter` binding).
- **`pipe-pane -I`** — feed external data *into* a pane. I use this to paste
  the system clipboard via xclip.

### Status line

- **Configurable status line** at top *or* bottom (`status-position top` for
  me).
- **Format strings** rich enough to support my actual status:
  - `#{session_name}` with `=N:` truncation
  - `#(shell command)` interpolation, with sane caching (otherwise the status
    bar forks every second)
  - `#{?cond,then,else}` conditionals
  - `#{e|>:a,b}` math/comparison ops — I use `#{e|>:#{window_active_clients},1}`
    to show the 👀 emoji only when *another* client is viewing
  - `#I`, `#W`, `#F`, `#{window_bell_flag}`, `#{pane_current_path}`,
    `#{window_active_clients}`, `#{host}`, date/time `%V %a %d %b %Y %H:%M`
- **`status-left`** / **`status-right`** with independent length limits
  (`status-left-length 22`, `status-right-length 100`).
- **`window-status-format`** and **`window-status-current-format`** per-window.
- **Style strings**: `fg=`, `bg=`, `bold`, named colors and `colourNNN`.
- **`display-message`** / **`display-time`** — the toast notification used by
  my `r` (config reload) binding.

### Command system

- **Command prompt** (`:`) with history.
- **Command sequences**: `cmd1 ; cmd2 ; cmd3`.
- **Targets**: `-t session:window.pane` with all the lookup rules — exact `=`
  prefix, glob, prefix match, ID (`$N`/`@N`/`%N`), special tokens (`!`, `+`,
  `-`, `^`, `$`, `{last}`, `{mouse}`, `{marked}`, etc.).
- **`if-shell`** for conditional execution. I use it for the theme toggle.
- **`source-file`** for `r` reload.
- **`send-keys`** including `-X` for copy-mode commands.
- **Brace-quoted argument blocks** `{ ... }` so I don't have to escape vim
  command strings inside `if-shell`.

### Environment

- **`update-environment`** — list of env vars refreshed on each new client
  attach (`DISPLAY`, `SSH_CONNECTION`, `XAUTHORITY`, `DBUS_SESSION_BUS_ADDRESS`,
  etc.). This is what makes reattaching after `ssh -X` not be a disaster.
- **`focus-events`** — forward terminal focus in/out events to apps (vim
  autoread depends on this).
- **`escape-time 0`** so Esc doesn't lag in vim.

### Persistence substrate

Tmux-resurrect-equivalent save/restore — and a future native `hat save` /
`hat restore` — depend on every one of these.

- **`run-shell`** — run a command from a binding. Resurrect's entry point.
- **Hooks**: `client-attached`, `session-created`, `after-new-window`, etc.
  Resurrect uses these to autosave on configured events.
- **Custom user options**: `set -g @my-option value`, readable from format
  strings as `#{@my-option}`. Resurrect stores its config this way; I use
  `@pane-theme` for my theme toggle.
- **`list-panes -aF` / `list-windows -aF`** with rich format strings
  (`#{pane_current_path}`, `#{pane_current_command}`, `#{pane_pid}`,
  `#{window_layout}`, `#{history_size}`). These are what resurrect dumps to
  its state file.
- **`capture-pane -p[ J]`** for scrollback restore (optional but desired).
- **Idempotent reconstruction**: `new-session`, `new-window`, `split-window`,
  `select-layout`, `send-keys` must all behave well when issued in bulk from
  a script.

## P2 — Important, slightly less hot path

- **`display-menu`** / **`display-popup`** — overlay windows. Not in my config
  yet but increasingly the way modern tmux plugins (and I) want to work.
- **Mouse support** (`mouse on`): click to select pane/window, drag borders,
  scroll wheel → copy mode. Currently off in my config but I keep eyeing it.
- **Search across all panes** — `find-window` (`f`).
- **Scratch pattern**: `new-window -at 60 -n SCRATCH 'vim ...'`. This requires
  `-a` (insert after current), `-t 60` (target index), and the ability to run
  a command in the new pane. All standard, just calling it out because it
  matters to me.
- **OSC 52 clipboard** for the SSH case. xclip on my local terminal covers
  ~99% of my copy/paste; OSC 52 is the only clean path for remote apps to
  reach my clipboard.

## P3 — Eventually, or only if cheap

- **Layouts**: `even-horizontal`, `even-vertical`, `main-horizontal`,
  `main-vertical`, `tiled`, `M-1`…`M-7`, `Space` to cycle. I rarely use the
  presets but `main-vertical` with `main-pane-width 100` is in my config.
- **Layout strings** (the cryptic `f5e1,200x50,0,0{...}` form) for
  save/restore.
- **`confirm-before`** wrapper for destructive commands.
- **`display-panes`** (`q`) — overlay pane indexes briefly. Handy.
- **Control mode** (`tmux -C`). Used by iTerm2's native tmux integration. I
  don't use it personally.
- **`pipe-pane`** in the output direction (not just `-I`). Useful for logging.
- **`%if`/`%elif`/`%else`/`%endif`** parse-time conditionals in the config.
  I don't use them but they're cheap to support once the format engine exists.
- **`bind -r`** (repeatable bindings). I don't use it; some people live by it.
- **256-color and true-color** terminal feature negotiation
  (`terminal-features`, `terminal-overrides`). Needed for modern apps but the
  defaults can probably be reasonable.
- **UTF-8 width tables** including ambiguous-width handling and emoji. My
  status line literally contains 👀; this needs to render at the right width
  or my layout breaks.
- **Logging**: `tmux -v` server/client/output logs. Invaluable for debugging
  HAT itself even if no user ever uses it.

## Explicitly *not* P0/P1 for me

What I'll skip — this informs scope.

- **Window/pane synchronization** (`synchronize-panes`). Cool, never use it.
- **`choose-buffer`** beyond the basic `=` picker.
- **`if-shell -F`** (format-string conditional, vs. shell-exec conditional).
  Nice but not load-bearing.
- **Full mouse drag-to-resize.** I resize with keybindings.
- **iTerm2 / control-mode integration.**
- **`run-shell -b`** background mode, **`wait-for`** synchronization. Internal
  plumbing I never touch from config.
- **Style strings as full mini-language** with gradients etc. Basic
  fg/bg/bold/colour256 is enough.

## Cross-cutting non-functional requirements

- **Robust against terminal sizes changing** while detached. Reattach from a
  smaller window must not corrupt the scrollback.
- **Robust against the terminal dying mid-write.** Server keeps running; pane
  state stays coherent.
- **Startup latency**: `hat attach` should be indistinguishable from `tmux
  attach`. No JIT pauses, no lazy module loads on the hot path.
- **`-2` / 256-color forcing**, **`-u` / force UTF-8**, **`-L name`** and
  **`-S path`** socket selection.
- **Sane logging** off by default, opt-in with `-v`.
- **Single static binary** — or at least a Nix-friendly install. I run NixOS;
  if HAT can't be packaged cleanly I can't use it.

## Workflows from my config worth calling out

End-to-end behaviors I actively use; each exercises several features above.
If HAT supports the bullet list above but breaks one of these, I'll notice.

1. **"Open a new thing in the same directory"**: `prefix C` opens a new window
   at `#{pane_current_path}`; `prefix v` / `prefix s` split with the same.
   Requires: `pane_current_path` format, `-c` flag on `new-window` and
   `split-window`.
2. **"Move this pane to the right edge"**: `prefix L`. Composes
   `split-window -fh`, `swap-pane -t !`, `kill-pane -t !`. The full-window
   split (`-f`) is the key.
3. **Theme toggle**: `prefix R` checks `@pane-theme` and swaps border styles
   *and* the stored theme value, then displays a confirmation. Requires:
   `if-shell` with format-string condition, user options, multi-command
   blocks, `display-message`.
4. **Status line that survives**: `#(~/.tmux/timelog report)`,
   `#(cat $XDG_RUNTIME_DIR/pomodoro.status)`, `#(~/.tmux/bat.sh)` plus literal
   date/time. Requires: shell interpolation with reasonable refresh interval
   (probably 15s default) and *not* crashing if the script errors or doesn't
   exist.
5. **Scratch buffer in another window**: `prefix M` opens a new window at
   index 60 named SCRATCH running vim with an unnamed scratch buffer. Requires:
   `new-window -a -t N -n NAME 'cmd with spaces and args'`.
6. **Save and restore everything**: `prefix M-s` / `prefix M-r` via
   tmux-resurrect. Requires: run-shell, custom `@`-options, rich
   `list-panes/-windows -aF`, `window_layout` strings + `select-layout`,
   and the ability for an external process to enumerate and recreate the
   tree. (Resurrect itself needs no hooks — those are a continuum
   dependency.) A native equivalent would be better.

---

If HAT covers everything tagged P0 and P1, plus the six workflows above, I
can switch to it as my daily driver.

## Roadmap

The milestone-driven build-out is complete:

- **M0–M7** — multiplexer core (server/client split, sessions/windows/panes,
  PTY allocation, terminal emulation, scrollback, keybindings).
- **M8** — copy mode and paste buffers.
- **M9** — command prompt (`:`) with history.
- **M10** — tmux.conf compatibility: `hat -f ~/.tmux.conf` loads with zero
  config errors and behaves faithfully for every line. Governing rule held
  throughout: never accept an option/command without implementing its
  behavior.
- **M11** — tmux-resurrect save/restore primitives: `@`-option round-trip,
  rich `list-panes -aF`, the `window_layout` string codec,
  `select-layout "<string>"`, `move-window`.
- **Continuous persistence** — a departure from manual resurrect toward the
  Firefox model: the server continuously mirrors the session/window/pane
  tree to a per-socket SQLite store and rebuilds it on restart, so
  `kill-server` then relaunch brings everything back automatically (no
  scrollback). Whitelisted running commands (`vim`, `htop`, …, via
  `@restore-commands`) are re-run; the schema is forward/backward
  compatible; `HAT_PERSIST=0` disables it.

From here, work proceeds one feature at a time as gaps are noticed in daily
use. Each is a self-contained slice: a failing test, the implementation, and
a scripted regression that lands in the suite, with the write-up in git
history.
