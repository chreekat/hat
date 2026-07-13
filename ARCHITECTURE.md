# HAT Architecture (rough)

A working sketch of how HAT is laid out. This is a *guide*, not a spec —
expect to revise it as we discover things. The goal is to commit early
to the few decisions that ripple widely (process model, concurrency
substrate, interface boundaries) and defer the rest until the code asks
for them.

Confidence is marked where it varies:
- **(verified)** — derived from reading the upstream tmux source or
  your config.
- **(inferred)** — extrapolated from how tmux works and standard Haskell
  practice.
- **(speculative)** — best guess, no direct evidence yet.

## Guiding principles

These come from `~/.claude/CLAUDE.md` and
`~/.claude/architecture-defaults.md` — repeating them here so they're
load-bearing in design choices, not just in code review.

- **Functions are the unit of organization.** Types and records grow on
  demand around functions, not before them.
- **Pass dependencies as arguments.** No reader monads layered "just in
  case." Most code is plain `IO` over concrete handles passed in.
- **Small monad stacks.** If a function doesn't need a stack, don't give
  it one.
- **No test-only typeclasses or mock seams.** Find pure logic, test it
  directly. Integration tests hit real PTYs and real sockets.
- **BDD milestones.** Each step delivers a demoable behavior.
- **TDD.** Failing test first, even when the test is a one-line property
  check on the emulator parser.

## What this doc commits to

1. **Server/client split over a Unix domain socket.**
2. **Custom wire protocol** carried over a schema-based encoding (see
   "Wire protocol shape" for the CBOR proposal) — not tmux-compatible
   on the wire, but with explicit back-/forward-compat machinery from
   day one.
3. **STM for the session/window/pane tree**, with broadcast TChans for
   change notifications.
4. **One reader thread per PTY, one reader thread per client connection,
   one renderer thread per attached client.** Green threads by default;
   bound threads only where a specific FFI or syscall demands it.
5. **The terminal emulator is wrapped, not written from scratch (yet).**
   Strangler pattern: start by binding to a mature implementation in
   another language (most likely `libvterm` via FFI), behind our own
   narrow Haskell interface. A pure-Haskell emulator can grow inside
   that interface later if and when it's worth it.
6. **Megaparsec** for the config-language, command-language, and
   format-string parsers.
7. **Pure parsers and evaluators** for all three languages above.
8. **No effect system.** `IO` and concrete records; revisit if pain
   appears.
9. **Modern Haskell record style.** `NoFieldSelectors` +
   `OverloadedRecordDot` + `DuplicateRecordFields` everywhere. Field
   names are short and unprefixed; access is `server.sessions`,
   `pane.pty`. CLAUDE.md and architecture-defaults both call for this.

## Stability and compatibility (long-term goal)

After a stabilization period — *not* something we sweat during the
fleshing-out phase — HAT should support:

- **Back-compat**: new clients connect to older servers. Upgrade the
  client without touching the running server (i.e., without killing
  your work).
- **Forward-compat**: new servers accept older clients. Roll the
  server forward without forcing every client process to restart.

These are explicit big-picture goals, not v1 goals. They drive two
concrete decisions we make *now* so we don't have to retrofit later:

1. The wire protocol carries a version envelope on every connection
   greeting, and the encoding (CBOR — see "Wire protocol shape") has
   built-in unknown-field tolerance. New optional fields are additive,
   not breaking.
2. We avoid load-bearing wire features that can't be downgraded
   silently. If a new command needs new event types, the server
   negotiates capabilities at handshake and falls back when the
   client doesn't speak them.

For the fleshing-out phase we'll cheerfully break the wire on every
release. The point of these primitives is that *when* we declare 1.0,
we have the substrate already.

Everything else (file layout, command set order, persistence strategy)
is deferred until the code wants it.

## Process architecture

There are exactly two kinds of process: **server** and **client**. They
communicate over a Unix domain socket in `$TMUX_TMPDIR/hat-$UID/` (or
`/tmp/hat-$UID/` if unset). **(verified — this mirrors tmux exactly and
matches the FEATURES.md P0 commitment.)**

```
+------------------+            socket          +----------------+
|    hat server    | <----------------------->  |   hat client   |
|                  |                            |                |
|  - owns PTYs     |    framed binary msgs      |  - raw TTY     |
|  - emulator      |                            |  - bytes in/out|
|  - state tree    |                            |  - resize evts |
|  - cmd engine    |                            |                |
+------------------+                            +----------------+
        ^
        | spawned children
        v
   /bin/sh, vim, ...
```

The server is started lazily on first `hat` invocation if not already
running, exactly like tmux. **(inferred — matches `-N` semantics in
tmux(1).)**

A client is *thin*: put the terminal in raw mode, register `SIGWINCH`,
shuttle bytes between the socket and `stdin`/`stdout`, exit on detach.
Almost no logic lives there. **(inferred.)**

### Why one process per role

- We want detach/reattach for free: if clients hold any state, reattach
  is a sync problem. Make clients stateless.
- Multiple simultaneous clients on one session (P0 in FEATURES.md) is
  trivial when the server owns everything and clients are renderers.

### What we deliberately don't do

- **No tmux protocol compatibility.** We own both ends. Inventing our
  own typed protocol is much cheaper than reverse-engineering theirs,
  and tmux's `tmux-protocol.h` is not stable across versions anyway.
- **No control-mode (`tmux -C`) support in v1.** Listed P3 in FEATURES.md.

## Concurrency model

Haskell's strength here. The shape:

- **Server state** lives in a small set of `TVar`s under a top-level
  `Server` record. Sessions, windows, panes, options, key bindings,
  paste buffers — all under STM.
- **Each pane has its own PTY reader thread** that reads bytes off the
  PTY master fd and feeds them into that pane's emulator. The emulator
  update happens inside STM so renderers see consistent grid state.
- **Each pane has its own writer fd** — when the input router decides a
  keystroke goes to a pane, it writes to that PTY directly. No queue
  needed; the OS pipe is the queue.
- **Each client connection has two threads:** a *socket reader* that
  parses incoming messages (keys, resize, command requests) and
  dispatches them onto the command queue, and a *renderer* that subscribes
  to a broadcast TChan of dirty events and re-renders the visible panes
  + status line + overlays into output bytes.
- **One command-queue worker per client.** Tmux has per-client command
  queues (`cmd-queue.c`); we follow suit. Some commands block on
  external state (`run-shell`, `if-shell`) so per-client serialization
  is the natural model.
- **`async` library** for thread lifetimes; `withAsync` everywhere so
  cancellation propagates. **(inferred — standard practice.)**

### Green threads vs. OS threads

When this doc says "thread" it means **green thread** (`forkIO` /
`async`) unless specifically noted. GHC multiplexes them onto a small
pool of capabilities (OS threads), one per RTS capability. We rely on
the IO manager to make `read`/`write` on PTY fds, sockets, and signal
delivery cooperative.

**Bound threads (`forkOS`) — only where needed.** A bound thread is
pinned to one OS thread for its lifetime. Use cases that *might* apply:

- **FFI calls into libraries with thread-local state.** libvterm holds
  all state on the `VTerm *` pointer, not in TLS — so plain green
  threads are fine. **(verified by reading `vterm.h`; the API takes
  the instance pointer everywhere, no `pthread_self()`-like calls.)**
- **Signal handling that must run on the main OS thread.** GHC
  install-once handlers don't care, but if we ever sigwait or call
  `signal()` directly we want the main thread, which is bound by
  default.
- **Calling libraries that require "same thread for init and use"**
  (GUI toolkits, mostly). N/A for HAT.

`safe` FFI calls release the RTS so other green threads keep running;
this is the default we want for `vterm_input_write` because callbacks
back into Haskell would otherwise deadlock on the capability lock.
`unsafe` FFI is faster but blocks all green threads on that capability
for the duration — only for very short C calls.

**Current verdict**: no bound threads needed. Revisit if profiling or
deadlocks suggest otherwise. **(speculative — confirm during the
libvterm FFI spike.)**

### STM granularity

Start coarse: one `TVar` per pane's grid+scrollback, one `TVar` per
session, one `TVar` for the global options map. Refine only if
contention shows up in practice. **(speculative — we may need finer
locking around the grid; the alternative is per-row TVars or a
mutable array under STM.)**

### What lives outside STM

- PTY reader and writer fds (file handles are themselves stateful).
- Socket connection handles.
- The renderer's last-frame cache (per client — for delta encoding).
- Logs.

## Layered module map

This is a logical layering, not necessarily the literal `src/` tree.
Modules at lower layers don't import higher ones.

```
  Hat.Main                                  -- CLI entry, server vs client mode
  +---------------------------------------+
  |  Hat.Server.CommandEngine             |
  |  Hat.Server.Hooks                     |
  |  Hat.Server.Status                    | -- status bar composition
  +---------------------------------------+
  |  Hat.Server.Render                    | -- panes + status -> screen ops
  |  Hat.Server.Input                     | -- keys -> commands
  |  Hat.Server.Layout                    | -- pane geometry
  |  Hat.Server.Format                    | -- #{...} #(...) engine
  +---------------------------------------+
  |  Hat.Model.Session                    |
  |  Hat.Model.Window                     |
  |  Hat.Model.Pane                       |
  |  Hat.Model.Options                    |
  |  Hat.Model.Keymap                     |
  +---------------------------------------+
  |  Hat.Term.Emulator                    | -- VT100/xterm parser + grid
  |  Hat.Term.Grid                        |
  |  Hat.Term.Style                       |
  +---------------------------------------+
  |  Hat.Pty                              | -- forkpty, signals, resize
  |  Hat.Wire                             | -- protocol types + framing
  |  Hat.Socket                           | -- Unix socket open/listen/accept
  |  Hat.Client.Tty                       | -- raw mode, read/write
  +---------------------------------------+
  |  Hat.Config.Lexer / Parser            | -- pure
  |  Hat.Command.Parser                   | -- pure
  |  Hat.Util                             |
```

Notable interface boundaries — these are where flexibility lives:

- **`Hat.Term.Emulator`** exposes only `feedBytes :: Emulator -> ByteString -> STM Emulator` (or similar). The internal state machine is opaque. If we get it wrong, we replace one module. **(speculative — the API may need cursor/grid query methods; grow on demand.)**
- **`Hat.Pty`** exposes `openPty`, `spawnIn`, `resize`, `readPty`, `writePty`, `closePty`. The Unix detail is hidden so the rest of the server doesn't care about `ioctl` numbers.
- **`Hat.Wire`** owns the on-wire format. Server and client both depend on it; nobody else does. Changing the wire = changing this module + a version bump on the socket greeting.
- **`Hat.Server.Format`** is a pure `evaluate :: FormatEnv -> FormatString -> Text`. The cache for `#(shell)` lives one layer up and is wired in by the renderer.

### Mapping back to tmux source for sanity

Just so we can cross-reference when we get stuck:

| HAT module                  | tmux file(s)                                  |
| --------------------------- | --------------------------------------------- |
| `Hat.Main`                  | `tmux.c`, `client.c`                          |
| `Hat.Server.CommandEngine`  | `cmd.c`, `cmd-queue.c`, all `cmd-*.c`         |
| `Hat.Server.Format`         | `format.c`, `format-draw.c`                   |
| `Hat.Server.Render`         | `screen-redraw.c`, `tty-draw.c`               |
| `Hat.Server.Input`          | `input-keys.c`, `key-bindings.c`              |
| `Hat.Server.Layout`         | `layout.c`, `layout-set.c`                    |
| `Hat.Server.Status`         | `status.c`                                    |
| `Hat.Server.Hooks`          | `notify.c`, `alerts.c`                        |
| `Hat.Model.*`               | `session.c`, `window.c`, `options.c`          |
| `Hat.Model.Keymap`          | `key-bindings.c`, `key-string.c`              |
| `Hat.Term.Emulator`         | `input.c`                                     |
| `Hat.Term.Grid`             | `grid.c`, `grid-view.c`, `grid-reader.c`      |
| `Hat.Term.Style`            | `style.c`, `attributes.c`, `colour.c`         |
| `Hat.Pty`                   | `spawn.c`, `osdep-linux.c`                    |
| `Hat.Wire`                  | `tmux-protocol.h`, `proc.c`                   |
| `Hat.Client.Tty`            | `tty.c`, `tty-keys.c`, `tty-term.c`           |
| `Hat.Config.Parser`         | `cfg.c`, `cmd-parse.y`                        |

## Data types (sketched, grow on demand)

Per the rule "types are secondary artifacts" — these are *enough to
start*, not final.

Style: every module that defines a record uses

```haskell
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
```

so field names are short, unprefixed, and only accessible via dot
syntax. No `paneFoo` / `sessionBar` prefixes. `NoFieldSelectors`
suppresses the auto-exported accessor functions, which removes the
Prelude-shadow concerns with names like `id`, and `OverloadedRecordDot`
makes `pane.pty` the only access path.

```haskell
data Server = Server
  { sessions :: TVar (Map SessionId Session)
  , options  :: TVar GlobalOptions
  , keymap   :: TVar Keymap
  , clients  :: TVar (Map ClientId Client)
  , socket   :: Socket
  , logger   :: LogHandle
  }

data Session = Session
  { id       :: SessionId
  , name     :: Text
  , windows  :: TVar (Seq Window)           -- ordered, indexable
  , current  :: TVar WindowIx
  , options  :: TVar SessionOptions
  }

data Window = Window
  { id      :: WindowId
  , name    :: Text
  , layout  :: TVar Layout                  -- tree of panes
  , active  :: TVar PaneId
  }

data Pane = Pane
  { id       :: PaneId
  , pty      :: PtyHandle
  , emulator :: TVar Emulator               -- grid + cursor + scrollback
  , size     :: TVar (Rows, Cols)
  , path     :: TVar FilePath               -- pane_current_path
  }

newtype SessionId = SessionId Int   deriving (Eq, Ord, Show)  -- prints as "$N"
newtype WindowId  = WindowId  Int   deriving (Eq, Ord, Show)  -- prints as "@N"
newtype PaneId    = PaneId    Int   deriving (Eq, Ord, Show)  -- prints as "%N"
```

These are deliberately minimal. We'll add fields as functions demand
them (`focus-events` state, bell flag, active-clients count for the 👀
emoji, etc.) — not preemptively.

**Boolean blindness avoidance** (CLAUDE.md rule): `aggressive-resize`
gets `data ResizePolicy = SizeToSession | SizeToSmallestAttached`, not a
`Bool`. `mode-keys` is `data ModeKeys = Emacs | Vi`.

## Wire protocol shape

A length-prefixed framed stream of typed messages. The encoding is
**CBOR** (RFC 8949) via the `serialise` library — *not* `Data.Binary`,
because the stability-and-compatibility goal above wants unknown-field
tolerance baked into the format, and CBOR maps give us that for free.

Why CBOR over alternatives:

- **vs. `Data.Binary` / `cereal`**: those expose Haskell's structure
  raw on the wire. Adding a field is a breaking change; we'd write
  the version-negotiation machinery by hand.
- **vs. Protobuf**: protobuf has the same compat properties as CBOR
  (field numbers, unknown fields ignored) and is the right answer
  *if* we expect a non-Haskell client (a Rust TUI plugin, a Go
  automation driver). Today both ends are Haskell. Protobuf adds a
  codegen step, a less ergonomic mapping to ADTs (`oneof` is OK but
  not great), and a heavier dep tree, in exchange for cross-language
  capability we don't need. If that need ever appears, the wire shape
  here (envelope `{ version, payload }`) lets us swap encodings
  inside `Hat.Wire.Codec` without touching call sites.
- **vs. Cap'n Proto**: zero-copy reads are overkill at PTY message
  rates; the schema-and-tooling overhead isn't justified.
- **vs. MessagePack**: very similar to CBOR; CBOR has an RFC and
  cleaner Haskell tooling.

`serialise` is well-maintained (used by Cardano, among others) and
generates `Serialise` instances via Generic.

Message space (initial sketch):

```haskell
data ClientToServer
  = Hello { protoVersion :: Word16, term :: Text, env :: [(Text, Text)] }
  | KeyInput ByteString          -- raw bytes from the TTY
  | Resize Rows Cols
  | Command Text                 -- "command line" form, parsed server-side
  | Detach
  | Ping

data ServerToClient
  = Welcome { sessionId :: SessionId, paneId :: PaneId }
  | Draw [DrawOp]                -- screen update for this client
  | SetTitle Text
  | Bell
  | Message Text                 -- toast (display-message)
  | DetachOk
  | Error Text
  | Pong
```

Explicit non-goals:
- No streaming JSON. Binary. Latency matters and structure is fixed.
- No version negotiation beyond a single integer in `Hello`. If client
  and server disagree, the client exits with an error.
- No back-compat. We rev both.

`DrawOp` is the deepest interface decision in this layer and is *not*
fixed yet. Options:

1. **Full-screen pixel-grid diff.** Server computes a diff between last
   frame and current frame; sends only changed cells. Simple semantics,
   moderate bandwidth.
2. **Higher-level ops** (move cursor, set style, write text, scroll
   region). More efficient, more complex to implement and test.
3. **Just send terminal escape sequences.** Server pre-renders to the
   wire format the client's TTY expects, client blits it.

Lean toward **(1)** for the first cut — it's the simplest invariant to
test (frame in, frame out) and the bandwidth cost over a Unix socket
is negligible. **(speculative — revisit when we add real terminals over
SSH-forwarded sockets.)**

## The terminal emulator (strangler-pattern wrap)

This is the part of HAT with the most unknown unknowns. We need
something good enough that vim, less, htop, ncurses apps, fzf, and
modern shells render correctly. Writing it from scratch in Haskell is
months of work to reach parity with what already exists in other
ecosystems. So: **we don't.** We wrap.

### Survey results (June 2026)

**Haskell native — nothing viable.**

- [`hs-term-emulator`](https://hackage.haskell.org/package/hs-term-emulator)
  (bitc) is the only candidate on Hackage. Last release 0.1.0.4 in July
  2021, ~68 GitHub stars, README does not document alternate-screen,
  scroll-region, OSC, or UTF-8-width coverage. The sibling
  `hs-sdl-term-emulator` is self-described as a "proof of concept."
  Status: dormant hobby project.
- [`vty`](https://hackage.haskell.org/package/vty) /
  [`vty-unix`](https://hackage.haskell.org/package/vty-unix) is the
  *client-side* renderer + input parser. Wrong layer.
- `termonad` is a full GTK terminal emulator that wraps GNOME's VTE
  widget. Not an embeddable library.
- `MarkLodato/vt100-parser` is output-only and GitHub-only.

**(verified — searched Hackage and GitHub directly.)** Nothing in the
Haskell ecosystem meets the bar.

**C — `libvterm` is the obvious choice.**

- Pure C99, no GUI, callback-driven. Hand it bytes, it calls back with
  "move cursor", "set cell", "scroll", "set title", etc.
- Battle-tested in Neovim (where it now lives bundled at
  [`src/nvim/vterm`](https://github.com/neovim/neovim/tree/master/src/nvim/vterm))
  and emacs-libvterm.
- Upstream lives at <https://www.leonerd.org.uk/code/libvterm>; the old
  `neovim/libvterm` mirror was archived June 19 2026 because Neovim
  bundled it inline. *Upstream is alive.*
- Covers: VT220 + xterm, alternate screen, scrollback hooks, mouse,
  OSC, mode flags. Effectively the union of what tmux's `input.c`
  handles.
- FFI is straightforward: `c2hs` against the public headers.

**Rust — second choice if libvterm bites us.**

- [`alacritty_terminal`](https://docs.rs/alacritty_terminal/) (crate
  0.26.0) exposes `Grid` and `Term` as a high-level API and bundles
  `vte` internally. The right shape, but pre-1.0 so expect some churn,
  and using it means cbindgen + cargo wired into the Nix build of HAT.
- [`alacritty/vte`](https://github.com/alacritty/vte) is the parser
  state machine only — same scope as writing the parser ourselves,
  doesn't save us anything meaningful.
- `termwiz` (wezterm's) is more comprehensive than alacritty_terminal
  but a much larger surface to bind against.

**Why libvterm first, not alacritty_terminal:**
1. C ABI is the simplest FFI from Haskell. `c2hs` does the work.
2. NixOS packaging is trivial — `libvterm` is already in nixpkgs.
   Rust crates linked into Haskell through cbindgen need a custom Nix
   derivation gluing cargo to cabal.
3. libvterm's API has been stable for a decade. alacritty_terminal is
   pre-1.0.
4. We can switch later. The whole point of the strangler interface is
   that the choice isn't load-bearing.

**(speculative — I haven't actually written the FFI yet. The build-system
claim about NixOS in particular wants verification in M1.)**

### The Haskell interface we hide it behind

The wrap interface is the seam. It's *ours*; the underlying emulator
can be libvterm, a Rust crate, or eventually pure Haskell, without the
rest of HAT noticing.

```haskell
-- Hat.Term.Emulator

newEmulator :: Rows -> Cols -> IO Emulator
resize      :: Emulator -> Rows -> Cols -> IO ()
feed        :: Emulator -> ByteString -> IO [EmulatorEvent]
-- ^ pure-looking from outside; libvterm calls our callbacks under the hood

data EmulatorEvent
  = TitleChanged Text
  | IconChanged Text
  | Bell
  | OscReply ByteString          -- bytes to send back to the PTY (mouse, query)
  | Damage Rect                  -- region of grid that changed since last feed

-- read-only snapshot for renderer
gridAt :: Emulator -> Rect -> IO [[Cell]]
cursor :: Emulator -> IO (Row, Col, CursorStyle)
mode   :: Emulator -> IO ModeFlags  -- alt-screen, mouse-on, etc.
```

It's `IO` at this layer because libvterm holds mutable state behind a
pointer. That's fine — it's a leaf, and we test it as a leaf.
**(inferred — once we know whether libvterm or alacritty_terminal, the
exact signatures will firm up.)**

### Testing the wrap

Per architecture-defaults: don't fake the dependency, test the real
thing. Integration tests feed canned byte streams (captured from vim,
htop, fzf sessions) into the emulator and assert on the resulting grid
snapshot. These tests are short, fast, and detect regressions in *our*
wiring without trying to retest libvterm itself.

### Threading and callback re-entrancy (resolved)

From reading `include/vterm.h` in the Neovim fork:

- `vterm_input_write(vt, bytes, len)` is the single byte-feed entry point.
  Callbacks (parser, state, screen) fire **synchronously during this
  call** — so re-entrancy means "don't call libvterm again from inside a
  callback," which is trivial to arrange.
- Damage callbacks can be **coalesced** with
  `vterm_screen_set_damage_merge`, which we should use to avoid
  per-cell wake-ups for big screen redraws.
- After feeding a batch, the embedder calls
  `vterm_screen_flush_damage` to drain pending damage callbacks.
- Output bytes (mouse replies, query responses) flow either through an
  output callback or a built-in buffer; we'll use the callback so the
  PTY-writer thread can pick them up.

**Concurrency rule for HAT:** each pane owns its libvterm instance and
is touched by exactly one thread at a time. The PTY reader thread reads
bytes, feeds them into libvterm (the callbacks land in a small
thread-local accumulator — an `IORef` in the reader's stack, not STM),
and only after `vterm_input_write` returns does it commit one
`atomically` block to update the pane's `TVar`. Renderers see a
consistent post-batch view; libvterm never observes concurrent access.

**(verified for API shape; speculative for "the IORef accumulator
pattern is the cleanest expression in Haskell" — possibly stable
pointers or a single foreign export `unsafePerformIO` accumulator are
cleaner. Pick during M1's FFI spike.)**

### Nix packaging (resolved)

Nixpkgs has two relevant packages: `libvterm` (older, "apparently
unmaintained") and **`libvterm-neovim`** (the actively-maintained fork
that Neovim itself ships). Use `libvterm-neovim`. The Emacs vterm
module on NixOS uses the same one, so the path is well-trodden.
**(inferred from NixOS Discourse + the nixpkgs convention; verify in M1.)**

### When (if) we replace libvterm with pure Haskell

Not before we have a working HAT. Maybe never. Triggers that would make
us reconsider:
- libvterm proves hard to drive from Haskell (signal interactions,
  callback re-entrancy).
- We want emulator behavior that libvterm doesn't expose (per-cell
  hyperlink metadata, sixel, custom OSCs).
- FFI overhead becomes measurable on large bursts (unlikely; PTY
  bandwidth is modest).

## Command engine and format strings

The command system is, after the emulator, the second-largest piece of
tmux. **(verified — over 70 `cmd-*.c` files.)**

Architecture:

1. **`Hat.Command.Parser`** — pure, using **Megaparsec**. Tokenizes
   tmux's command syntax (semicolons, braces, single/double quotes,
   line continuation, `%if`/`%elif`/`%endif`) into `[ParsedCommand]`.
   The same parser handles config files (`cfg.c` does this in tmux)
   and the command prompt. Megaparsec also drives the format-string
   parser in `Hat.Server.Format` — one parsing toolkit across the
   project.
2. **`Hat.Server.CommandEngine`** — `runCommand :: Server -> ClientId
   -> ParsedCommand -> IO CommandResult`. Each command is a function;
   we register them in a `Map Text CommandImpl`. No giant case
   statement. **(inferred from how tmux registers commands.)**
3. **Command queue**: per-client. A `TQueue ParsedCommand`. A worker
   thread pops, executes, repeats. Commands like `if-shell` push more
   commands onto the front of the queue.
4. **Format strings** — `Hat.Server.Format` is a pure parser + pure
   evaluator over a `FormatEnv` record. The renderer constructs
   `FormatEnv` from current state and calls `evaluate`. `#(shell)`
   interpolation is *not* pure — handle that by pre-expanding shell
   substitutions in the renderer with a per-shell-cmd cache (mirroring
   tmux's `job.c`).

The format-string mini-language is bigger than it looks: `#{?cond,a,b}`,
`#{e|>:a,b}`, `#{=N:str}`, `#{T:format}` for time, `#{s/from/to:str}`
substitution. Start with the subset your status line uses
(FEATURES.md workflow #5: `#I`, `#W`, `#F`, `#{?cond,a,b}`,
`#{e|>:a,b}`, `#{=N:s}`, `#{pane_current_path}`,
`#{window_active_clients}`, `#{host}`, plus date `%V %a %d %b %Y %H:%M`).

## Hooks (resolved)

Tmux's hook model (from `notify.c`):

- Each hook is just an option name (`client-attached`, `pane-died`,
  `session-renamed`, `window-linked`, `pane-mode-changed`, `paste-buffer-changed`,
  paste-buffer-deleted, etc., plus user-named `@my-hook`).
- The option's value is a parsed command list bound by the user with
  `set-hook -g client-attached '...'`.
- Server-side code that detects an event calls a
  `notify_<scope>(name, scope_obj)` function. That helper builds a
  `cmd_find_state` snapshot of "the current client/session/window/pane,"
  looks up the hook option walking pane → window → session → global
  scope, and queues the resulting commands onto the command queue
  *after the current item*.

**Decision: copy this model directly.** A single
`Hat.Server.Hooks.notify :: Server -> HookScope -> HookName -> IO ()`
function does the lookup and enqueue. Call sites in the server emit
explicit `notify` calls at the events tmux defines. No event bus, no
publish/subscribe layer.

Why this and not a more general bus:

- Hooks are sparse (most names have no binding most of the time). The
  lookup cost is one `Map.lookup` per event; cheaper than a broadcast.
- The set of hook names is closed and known. We don't need late binding.
- It matches tmux exactly, so config compatibility (a user pasting
  `set-hook -g client-attached ...` from tmux docs) just works.

`HookScope` is a sum type (`HookGlobal | HookSession SessionId | HookWindow
WindowId | HookPane PaneId | HookClient ClientId`) — boolean-blindness
avoidance per CLAUDE.md.

## Save and restore (resolved as not-our-problem-yet)

Looked at your installed tmux-resurrect (`~/.tmux/plugins/tmux-resurrect/`).
The save side is a shell script bound to `M-s` via `run-shell` that
issues `list-panes -aF` and `list-windows -aF` with format strings to
dump tab-separated state, plus optionally `capture-pane` for scrollback.
The restore side replays via `new-session`, `split-window`,
`select-layout`, `send-keys`, etc.

**Implication for HAT:** if we support the substrate — `run-shell`,
`list-panes -aF`, `list-windows -aF` with the format strings resurrect
uses (`#{session_name}`, `#{window_index}`, `#{window_layout}`,
`#{pane_current_path}`, `#{pane_current_command}`, `#{pane_pid}`,
`#{history_size}`), `capture-pane`, and the construction commands —
tmux-resurrect-the-script may work as-is, or a HAT-resurrect fork is a
few hundred lines of shell.

That's the v1 plan: **don't build save/restore into the server.** Make
sure the substrate is there, point users at the script. A native
`hat save` / `hat restore` is genuinely simpler than the shell pipeline
and is a reasonable post-M8 project, but isn't a v1 architectural
decision.

## Options persistence (resolved)

Tmux stores options in `options.c` as an in-memory tree. There's no
on-disk persistence; options reload from `~/.tmux.conf` on every server
start. **HAT: same.** No SQLite, no JSON dump. The architecture-defaults
"prefer SQL where it does the work" rule doesn't apply — there's no SQL
to do here.

## Clipboard policy (OSC 52, resolved)

OSC 52 is the escape sequence apps use to write (and optionally read)
the system clipboard. It is a known footgun: any program that can write
to your terminal can stomp your clipboard, and `read` access leaks
clipboard contents to whatever process is on the other end.

Modern terminals (kitty, foot, wezterm, alacritty) make it opt-in per
direction, with read often disabled by default.

**Decision:** a server option `clipboard-policy` with values
`Disallow | AllowWrite | AllowReadWrite`. Default `AllowWrite`. The
config command form follows tmux convention: `set -g clipboard-policy
allow-write`. Map to libvterm's OSC handler — when policy denies, drop
the sequence.

Boolean-blindness avoidance applies; the type is

```haskell
data ClipboardPolicy = Disallow | AllowWrite | AllowReadWrite
```

not a `Bool`.

## Logging (resolved)

Architecture-defaults: structured JSON, one event per unit of work, no
logging inside pure functions.

Hackage options surveyed:
- **`katip`** — production-proven (Soostone, years of use), JSON output,
  pluggable scribes (file, stdout, ElasticSearch).
- **`co-log`** / **`co-log-json`** — composable contravariant design;
  elegant but a heavier conceptual surface than HAT needs.
- **`monad-logger-aeson`** — drop-in JSON replacement for
  `monad-logger`; tied to the persistent ecosystem we won't otherwise
  use.

**Committed:** `katip` for JSON output and file scribe, **wrapped** so
the rest of HAT only sees:

```haskell
-- Hat.Log
data LogEvent
  = CommandExecuted   { client :: ClientId, name :: Text, durationMs :: Int }
  | ClientConnected   { client :: ClientId, term :: Text }
  | ClientDetached    { client :: ClientId, reason :: Text }
  | PaneSpawned       { pane :: PaneId, cmd :: Text }
  | PaneExited        { pane :: PaneId, status :: ExitCode }
  | HookFired         { hook :: Text, scope :: Text }
  | ConfigParseError  { file :: FilePath, line :: Int, msg :: Text }
  | EmulatorWarning   { pane :: PaneId, what :: Text }
  -- ... grow on demand

logEvent :: Logger -> LogEvent -> IO ()
```

The wrapper is the seam: katip can be swapped for `co-log` or a
hand-rolled `aeson + TQueue + handle` writer without touching any
caller.

## Configuration loading

- Pure tokenizer + parser produces `[ParsedCommand]`.
- Server-startup runs them through the same command engine clients use.
- `source-file` is just `runCommand` over a freshly parsed file.
- `bind`, `set`, `set -g @foo`, `if-shell` — all reuse engine machinery.

Result: the config language *is* the command language with no extra
work. This mirrors tmux exactly. **(verified.)**

## Testing strategy

Per CLAUDE.md and architecture-defaults: TDD, real dependencies, no
mock seams.

**Pure code — property + unit tests.** All of these go in `test/`,
run by `cabal test`:

- Emulator: QuickCheck. "Feeding ASCII text produces a grid that, when
  read out left-to-right top-to-bottom, equals the input."
  "`\r\n` advances cursor."  "Random valid CSI sequences don't crash."
  Shrinks must be real. **(verified — CLAUDE.md rule.)**
- Format strings: golden tests + properties. "Evaluating `#W` returns
  window name." "`#{?#{==:a,b},yes,no}` short-circuits."
- Config parser: round-trip property — `parse . render = id` on the
  subset that has a `render`. Plus a golden test for your actual
  `~/.tmux.conf`.
- Layout: properties on the tree zipper — splits and swaps preserve
  the set of pane IDs.
- Grid: scrollback and resize preserve content invariants.

**Integration tests — real PTYs, real sockets.** A scripted harness:

- Spawn `hat-server` against a temp socket.
- Spawn `hat client` against it, with a *fake terminal* on the other
  side (just a pair of pipes the test owns).
- Run a scripted shell command (`echo hello`), assert the rendered
  output contains `hello`.
- Detach, reattach, assert state survives.

These live in `test/integration/` and are committed alongside the
features they verify, per CLAUDE.md ("the final tests SHOULD be
scripted and included in the repo").

**No test-only typeclasses.** If something's awkward to test because
it's tangled with I/O, extract pure functions. The emulator core is
the canonical example.

**Upstream tmux's regression suite — borrowed verbatim.** Tmux's
`regress/` directory is ~60 POSIX shell scripts that drive a binary
through the `tmux` CLI and compare output to fixtures or string
literals. Each script begins:

```sh
[ -z "$TEST_TMUX" ] && TEST_TMUX=$(readlink -f ../tmux)
TMUX="$TEST_TMUX -Ltest -f/dev/null"
```

— so swapping the binary is literally a one-variable change. We run
them as `TEST_TMUX=$(realpath path/to/hat) sh regress/<test>.sh`.

What carries over: command-syntax tests, formats (including the
exhaustive `format-strings.sh`), base-index, env propagation, copy
mode (vi + emacs), prompt mechanics, `if-shell`, `run-shell` output,
`capture-pane`, control-mode sanity, cursor and redraw including
resize behavior. Plus the fixture files (`*.result`, `cursor-test.txt`,
`UTF-8-test.txt`, `copy-mode-test.txt`).

What doesn't: the C fuzzers under `fuzz/` (`cmd-parse-fuzzer.c`,
`format-fuzzer.c`, `input-fuzzer.c`, `style-fuzzer.c`) link against
tmux internals. We write equivalents against our own modules —
Megaparsec is well-suited to fuzzing the command and format parsers.

Realistic caveats:
- **CLI byte-fidelity is the bar.** `display-message -p`,
  `list-windows -F`, error messages — must match upstream's exact
  output. This is much stricter than "feature equivalent." Pass-rate
  is a *trajectory*, not a gate.
- **Emulator divergence will surface.** Tests like `cursor-test1.sh`
  assert specific cursor positions after a resize. libvterm and tmux's
  `input.c` agree on most VT220/xterm behavior but not all; when they
  diverge, tmux's test is right by definition (the test was authored
  against tmux's emulator). We allowlist with a note per failure.
- **`TERM=screen` is hardcoded** in the tests. HAT's exported child
  env will need to advertise `screen` (or `tmux`) until we ship our
  own terminfo entry.
- **License hygiene.** Tmux is ISC. We do **not** vendor `regress/`
  into HAT's repo. The runner takes a path to a checked-out tmux
  source (or a Nix flake input pinning a commit) so we stay clearly on
  the "use" side of the line and can track upstream as it adds tests.
- **`regress/Makefile` uses BSD make's `!=`.** POSIX `make` won't
  parse it. We invoke scripts directly via `sh`, ignore the Makefile.

Concrete shape:

```
tools/
  run-upstream-tests.sh        # iterate, record outcomes, summarize
  upstream-xfail.txt           # known-failing tests, one per line, with reason
```

Each script run produces one of four outcomes:

| Outcome             | In `xfail.txt`? | Script result | Suite result |
| ------------------- | --------------- | ------------- | ------------ |
| **PASS**            | no              | 0             | success      |
| **XFAIL** (known)   | yes             | non-zero      | success      |
| **XPASS** (unexpected pass) | yes      | 0             | **failure**  |
| **FAIL**            | no              | non-zero      | failure      |

XPASS is the important one: when a test we'd marked as known-failing
starts passing, CI fails until someone removes its entry from
`xfail.txt`. This keeps the file from rotting into a stale list of
"we tried once and gave up." Every promotion to PASS is recorded as a
real change; every regression to FAIL is caught at the next CI run.

CI runs it. The pass count over time is a real progress signal —
better than any feature checklist, because the tests were written by
people who know tmux better than we ever will.

**(speculative on pass-rate prediction.** Early on we expect <30%
passing — even `new-session-base-index.sh` exercises a deep slice
of the system. The graph going up over months is the design intent,
not a single-PR gate.)

## Open questions / decisions we're deferring

What's left after the investigation:

1. **M1 (FFI spike) must verify three libvterm specifics:**
   (a) the IORef-accumulator pattern for callback events is the
   cleanest expression in Haskell, vs. stable-pointer + foreign-export
   alternatives; (b) `libvterm-neovim` is the actual nixpkgs attribute
   we want and links cleanly into a cabal build; (c) feed and
   `set_size` from different threads do not race (we currently plan
   single-threaded-per-pane access, but the spike confirms it).
2. **Plugin / extension surface.** v1 has no plugin ABI; v2+ might.
   Tmux-resurrect-the-script working via `run-shell` is the v1 answer.
   The shape of any future native plugin system is unknown.
3. **Layout tree representation.** Tmux uses a binary tree of `hsplit`
   / `vsplit` nodes with leaves as panes. We'll do the same, but the
   API to manipulate it (zipper? lens? pure functions over the tree?)
   is undecided and best driven by M4 implementation rather than
   pre-designed.

Resolved during this pass (see sections above): the terminal emulator
strategy, the wire-protocol shape, the concurrency layer, hooks, save
and restore, options persistence, and clipboard policy.

## What flexibility costs us, and where we draw the line

We are *not* going to:

- Build a plugin ABI before there are plugins.
- Add an effect system because someone might want to swap effects
  later. Pure functions + `IO` + records-of-handles is the seam.
- Generalize the wire protocol over multiple transports. Unix socket
  only. If we ever want TCP, we'll add it then.
- Carve out modules for OS portability. Linux + NixOS first.
  Other-Unix support comes when someone runs it.

The flexibility we *are* spending design budget on:

- Emulator behind a narrow interface — we will almost certainly
  rewrite it once.
- Command engine that takes commands as data — adding new commands
  doesn't touch the dispatcher.
- Format-string evaluator pure and total — adding formats doesn't
  touch evaluation, only the env-construction step.
- Wire format owned by one module — we can rev it freely.

---

If this shape feels right, the next step is M0 as a real plan: split
into 1–8 committable steps, failing test first for each. If it feels
wrong, the cheapest place to redirect is the layering map and the
concurrency model — those drive everything else.
