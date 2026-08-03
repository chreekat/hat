# Resource cleanup audit

A sweep of every acquisition/release of a resource with a lifetime — threads,
processes, fds, handles, sockets, foreign memory — checked against the project's
RAII rule (release is *structural*: `bracket`/`finally`/`withAsync`/`link`/a
finalizer, never a hand-called line a later exception can skip) and the
sync-vs-async discipline in `haskell-exceptions.md`.

Bug tracked as `b` id 9.

## What the severities mean

- **(A) real leak/hazard** — an exception path skips a release, an async
  cancellation is swallowed, or a resource outlives its owner unstructurally.
  Drop-everything.
- **(B) works today but fragile** — currently correct, but the release is a
  hand-called line rather than a bracket, so a future edit can regress it
  silently. Should become structural.
- **(C) fine as-is** — either already structural, or a *deliberate*
  non-bracket design that is nonetheless safe (a resource whose lifetime
  legitimately spans the whole process; a thread reaped via a counter/`finally`
  it itself owns). Documented so it is not re-flagged.

## Method

Ripgrep sweep of `src/` and `app/` for `forkIO`, `killThread`, `myThreadId`,
`throwTo`, `createProcess`, `withCreateProcess`, `spawn`, `waitForProcess`,
`terminateProcess`, `openFile`, `fdToHandle`, `hClose`, `closeFd`, `close`,
`socket`/`listen`/`accept`, `newForeignPtr`, `malloc`/`free`, `bracket`,
`finally`, `mask`, `withAsync`, `link`. Each hit was read in context and traced
to how — and on which exit paths — the resource is released. Test-only code
(`test/`, `bench/`, `tools/`) is out of scope and not tabulated.

## Reference: the good patterns to hold others against

These are the structural designs the rest of the codebase should match:

- **`withDaemons`** (`Server.hs:254`) — `foldr (\d k -> withAsync d $ \a -> link
  a >> k) body ds`. Every background daemon (clock, title, reconcile,
  color-scheme, persist) is cancelled when the serve loop returns or throws, and
  `link`ed so an unexpected fault re-raises rather than vanishing. Textbook
  asymmetric supervision.
- **`startPaneReader` / `closePane` / `reapPane`** (`Server.hs:1409`, `1570`,
  `1644`) — the pane reader is `forkIO`ed, but its teardown is two stacked
  `finally`s (`closePane`, then decrement `livePanes`) that run on *every* exit
  (clean EOF, hang-up, exception). The OS reap runs exactly once, from that
  `finally`. This is a `forkIO` that is safe because it owns its own structural
  teardown.
- **`hangupPane`** (`Server.hs:1557`) — kills the reader thread so its blocked
  `readPty` returns and its `finally` runs `closePane`; the release is still
  the reader's `finally`, not a line in the killer.
- **`finallyClearRestoring`** (`Server.hs:263`) — a gate cleared by `finally`,
  never a line a crash can skip.
- **Emulator ForeignPtr finalizer** (`Emulator.hsc`) — `FC.newForeignPtr`
  frees the libghostty terminal and the write_pty/bell wrapper `FunPtr`s
  once the `Emulator` is unreachable. No hand-free anywhere.
- **`listenOn`** (`Socket.hs:56`, `66`) — `bracketOnError` around
  `N.socket`/`N.close`, so a failed `bind`/`listen` never leaks the socket.
- **`serveOn` listen socket** (`Server.hs:177`) — `bracket openListen N.close`.
- **`withColorScheme` monitor** (`Server.hs:524`) — `withCreateProcess` reaps
  the `gsettings monitor` child on every exit.
- **Client `withRawMode`** (`Client/Tty.hs:31`) — `bracket` restores terminal
  attributes no matter what.
- **`Persist.withStore`** (`Persist.hs:183`) — `bracket (open path) close`.

## Findings

| # | Sev | Site | Resource | Current release | Risk | Recommendation |
|---|-----|------|----------|-----------------|------|----------------|
| 1 | C | `Log.hs:54,57` | log file `Handle` + drain thread (`forkIO forever`) | never closed / never killed; both live for the whole process | none in practice — the logger is a process-lifetime singleton, so there is no scope to bracket it to; on exit the RTS reaps the thread and the OS closes the fd | leave as-is. If a `restart-server` self-exec ever needs the log flushed+closed *before* `executeFile`, introduce a `withLogger`/`closeLogger` seam then — see follow-up 1. Not a leak today. |
| 2 | C | `Pty.hsc:169` (`startReaper`) | child-reaper thread (`forkIO`) | thread ends by itself when `getProcessStatus` returns and fills the `MVar` (unconditionally, even on failure) | none — self-terminating, fills its slot exactly once so `waitExit` can never park forever; the comment documents the invariant | leave as-is. Deliberate short-lived thread with a guaranteed terminal put. |
| 3 | C | `Pty.hsc:330` (`closePty`) | pty master `Handle` | `hClose` inside `reapPane`'s single run (itself a `finally`, via `closePane`) | none — reached only from the reader's `finally`; the `catch (_::IOException)` swallows only a *sync* double-close, never an async exception | leave as-is. The hand-called `hClose` is inside a structural `finally`. |
| 4 | C | `Server.hs:1069,1074,1078` (`acceptLoop`) | accepted client `Socket` (`forkIO` per conn) | `handleConn ... finally (N.close conn ...)` | none — the connection socket is closed on every exit of the handler thread | leave as-is. `forkIO` + `finally` release, correct. |
| 5 | C | `Server.hs:1099,1125` (`welcome`) | client registration; render thread | `controlLoop`/`inputLoop ... finally removeClient`; render loop under `withAsync` | none — `removeClient` is structural (`finally`); the render peer is bracketed | leave as-is. |
| 6 | C | `Emulator.hsc` foreign allocs | libghostty terminal, wrapper FunPtrs | ForeignPtr finalizer | none | leave as-is. Model finalizer pattern. |
| 7 | B | `Server.hs:3897,3902,3931` (`startPipe`/`stopPipe`) | pipe-pane subprocess, its stdin `Handle`, and a `pumpPipeOutput` reader thread | `stopPipe`: hand-called `killThread` on the reader, `hClose` on stdin, `terminateProcess`, then a detached `forkIO waitForProcess` reap | works, but every release is a hand-called line, not a bracket. `stopPipe` is invoked from `reapPane` (a `finally`) and from `cmdPipePane`, so the common paths are covered — but nothing *structurally* ties the reader thread / stdin handle to the pane's life; a new caller of `startPipe` that forgets `stopPipe` leaks all three. The detached reap `forkIO` is itself unsupervised (acceptable: fire-and-forget zombie-reaper). | make the pipe a bracketed sub-resource of the pane, or at minimum route *all* teardown through the reader thread's own `finally` (mirror `startPaneReader`). Track as follow-up 2. |
| 8 | B | `Main.hs:189-191` | three `/dev/null` `Handle`s handed to the detached server's `createProcess` | never closed in the parent | negligible — the parent is the short-lived `hat` client wrapper that hands the fds to the child (`UseHandle`) and exits almost immediately, so the OS reclaims them. But it is a hand-open with no matching close | wrap the three opens in `bracket`/`withFile` around the `createProcess`, or close them right after. Self-contained; low value. Track as follow-up 3. |
| 9 | C | `Server.hs:2067` (`showToast`), `4355` (`cmdRunShell`), `View.hs:533` (shell-format cache) | fire-and-forget `forkIO` (a toast-expiry timer; a `readCreateProcessWithExitCode` runner) | none — the thread runs to completion and returns; the subprocess is reaped by `readCreateProcess*` internally | low. These are unsupervised, un-`link`ed background threads not tied to the serve scope, so at shutdown they are abandoned (killed when the process exits) rather than cancelled cleanly. No fd/process leak — `readCreateProcessWithExitCode` brackets its own child; the toast timer holds nothing | acceptable as-is (genuinely detached, hold no resource). If the server ever needs a *clean* drain of in-flight `run-shell`s at shutdown, route them through a supervised nursery. Track as follow-up 4 (nice-to-have). |
| 10 | C | `CopyMode.hs:892-904` (`runPipeCommand`) | `/dev/null` `Handle`, copy-pipe subprocess, detached `forkIO waitForProcess` | `hClose` on stdin after write; child reaped on a detached thread; `/dev/null` handle passed via `UseHandle` and not explicitly closed | low — `close_fds=True`; the `devnull` handle leaks per invocation until the process exits, same shape as #8 but on the server (long-lived) side, so it accumulates one fd per copy-pipe. The subprocess itself is reaped. | close `devnull` after `createProcess` returns (the child has dup'd it), or `bracket` it. This is the one accumulating-fd case worth a real fix — but it is inside a broad `catch (_::SomeException)` and needs care. Track as follow-up 5. |
| 11 | C | `Server.hs:4313-4333` (`keepOpenAcrossExec`/`cleanupInherited`) | listen fd + every pane master fd across a self-exec reload | on a *successful* reload the fds are deliberately kept open (cleared of close-on-exec) and re-adopted; on an *incompatible* payload `cleanupInherited` SIGHUPs each child and `closeFd`s each master + the listen fd | none — this is the version-independent cleanup core the compat rules require; both branches are exhaustive and each `closeFd`/`signalProcess` swallows only sync `IOException` | leave as-is. Intentional cross-exec fd handover; the "release" is either adoption or `cleanupInherited`, both structural at the reload decision point. |
| 12 | C | `Server.hs:504` (`watchColorScheme`) | `gsettings monitor` subprocess | `withCreateProcess` | none — reaped on every exit; the restart loop re-raises `PropagateFault` (async cancel / `ExitCode`) instead of swallowing it, per the exceptions doc | leave as-is. Correct sync/async split. |

## Summary

- **(A) real leak/hazard:** 0.
- **(B) works today but fragile:** 2 — #7 (pipe-pane teardown is hand-wired, not
  bracketed to the pane) and #8 (Main.hs `/dev/null` opens with no close).
- **(C) fine as-is / deliberate-safe:** 10.

No drop-everything leak was found. The codebase is unusually disciplined about
structural cleanup: the daemon supervisor (`withDaemons`), the pane reader's
stacked `finally`s, the ForeignPtr finalizer, and the `bracket`/`bracketOnError`
around every socket and store are all textbook. The residual risk is concentrated
in a few *hand-called* release lines that happen to sit on covered paths today
but are not bound to their resource's scope, so a future edit can regress them
without a compiler or test noticing — exactly the failure mode the RAII rule
exists to prevent.

## Recommended follow-ups (each should become its own `b` bug)

1. **`withLogger` seam.** Give `Hat.Log` a bracketed lifetime (open handle +
   drain thread under `withAsync`, flush-and-close on scope exit). Precondition
   for ever flushing the log cleanly before a `restart-server` self-exec.
   Low-risk but touches `serveOn`'s top-level structure — a refactor, not a
   one-liner.
2. **Bracket the pipe-pane (#7).** Make the `pumpPipeOutput` reader thread and
   the pipe subprocess+stdin a structural sub-resource of the pane (mirror
   `startPaneReader`'s `finally`), so no `startPipe` caller can leak them by
   forgetting `stopPipe`.
3. **Close Main.hs `/dev/null` handles (#8).** `bracket`/`withFile` the three
   opens around the detached `createProcess`. Self-contained, trivial.
4. **Supervised `run-shell`/toast nursery (#9).** Optional: route fire-and-forget
   `forkIO`s through a scope so shutdown can drain them. Nice-to-have; no leak
   today.
5. **Close the copy-pipe `/dev/null` (#10).** The one accumulating-fd site (one
   fd per copy-pipe on the long-lived server). Close `devnull` once the child
   has dup'd it. Needs care around the surrounding broad `catch`.
