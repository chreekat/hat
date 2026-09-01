# Contributor & agent guidance for HAT

Read alongside `ARCHITECTURE.md` (design + rationale) and `FEATURES.md`
(scope). Your global/personal rules still apply; this file only adds what is
specific to HAT.

## Build, test, run

- Everything goes through `cabal` **inside the dev shell**: never call built
  binaries directly (stale artifacts) and never build outside the shell
  (libghostty-vt and its `pkg-config` come from it).
- Enter the shell **once**, not per command: `nix develop -c ...` re-evaluates
  the flake every invocation (~15s on a dirty tree). Snapshot the environment
  and source it instead:

      nix print-dev-env > "$SCRATCH/devenv.sh"   # once per session
      source "$SCRATCH/devenv.sh" && cabal test  # per command

  Re-snapshot if `flake.nix`/`flake.lock` change. (A user shell can use
  direnv or an interactive `nix develop`; the snapshot trick is for
  non-persistent shells like an agent's.)
- `cabal test` runs the unit, property, and integration suites. The
  integration tests drive the real `hat` binary through a pty, so rebuild
  before trusting a repro. Keep the whole suite fast and green — a flaky test
  is a stop-work emergency, never "pre-existing flakiness."
- Golden fixtures: `cabal run gen-fixtures` regenerates emulator goldens. A
  golden diff is a tripwire; regenerate only when the change is intended.
- `just ghcid` (inside the dev shell) is the fast typecheck loop: a
  `-fno-code` multi-repl over every component — tests and benchmarks
  included — that mirrors diagnostics to `errors.err` in the checkout root.
  - While it runs, read compile errors from `errors.err` instead of invoking
    `cabal build`. It typechecks only: running anything (tests, the binary)
    still needs cabal.
  - `errors.err` ending in "Ghcid has stopped." means ghcid is NOT running
    and the file is stale: kill any remnant and relaunch `just ghcid`. Don't
    switch to `cabal build` for feedback — ghcid is fast enough to be worth
    hand-holding through a crash.
  - It can die on first load: an error that keeps the multi-repl session from
    coming up at all (e.g. a cabal-level error) exits ghcid without writing
    `errors.err`. The error is in ghcid's own output, so capture that when
    launching; fix what it names and relaunch.
  - `hat.cabal` and `src/Hat/Term/Emulator.hsc` are `--restart` triggers (the
    session rebuilds from scratch when they change); other files hot-reload
    on save.
  - Each git worktree runs its own ghcid with its own `errors.err`. It is a
    long-running watcher: launch it detached/background, never foreground in
    an agent shell.
  - A ghcid you started is yours to shut down when the job is done — note its
    pid at launch and kill it before finishing (an orphaned multi-repl holds
    the whole project in memory). Never kill a ghcid you did not start: the
    one in the main checkout is the user's.

## Forward/backward compatibility is a first-class concern

HAT is built to keep your running programs alive across upgrades — detach and
reattach, continuous persistence, and in-place `restart-server`. Every one of
those means **data written by one version of the binary is read by another.**
So any serialized format that crosses a version or process boundary must be
built for compatibility *from the moment it is introduced* — even now, pre-1.0,
when we otherwise break formats freely. We break the *payloads* cheaply; what
we never skip is the *substrate* (versioning + tolerance) that lets a break be
safe instead of catastrophic. A format that ships without it can orphan a
running program or corrupt a store on the next upgrade.

The cross-version boundaries that exist today — treat this as the list to
check, and **add a row whenever you introduce a new one**:

| Boundary | Module | Evolution rule | Corpus |
|---|---|---|---|
| Client ↔ server wire | `Hat.Transport.Wire` | Versions exchanged, both speak `min` (`negotiate`); window = every version ≥ floor 4, forever. Append-only CBOR tags; leaves grow under a new dialect level, encoded per-peer. Unknown tag → skip; `Malformed` → fatal. | golden-byte + dialect corpus in `WireSpec` |
| Persistence store | `Hat.Server.Persist` (SQLite) | Additive schema: core columns never change meaning; evolving fields ride a per-row `extra` JSON column; DDL additive only; reads default anything absent and ignore the unknown (never gate on `schema_version`). The live tables always hold the newest tree; `snapshot` history rows carry whole trees as JSON evolved under the same tolerant rule. | `PersistSpec` "schema compatibility" |
| Reload handover | `Hat.Server.Reload` | Frozen envelope (`magic`, `reloadEra`, and a version-independent cleanup core of fds) around an era-tagged payload. The tree rides inside as the store's snapshot JSON, evolving under the store's additive rule; the era gates only the hot core (fds, pids, modes, screens). A build decodes-and-migrates every era `1..X`; a newer/undecodable payload → clean restart, never orphaned processes. | `ReloadSpec` corpus |

Two valid mechanisms, both delivering the same guarantee (a new build reads old
data): **additive schema** — one lenient reader handles old *and* new, both
directions (the wire tags, the store's columns); and **versioned migration** — a
version stamp plus a decoder + forward migration per older version, so era X
reads `1..X` (the reload handover). Prefer additive; reach for migration only
when a change genuinely can't be additive.

When you add or change any such format, the rules are:

1. **Stamp a version/era** at the head, so a reader tells "older version" from
   "corrupt bytes." Mirror `protocolVersion`.
2. **Evolve additively by default.** Append tagged fields or add defaulting
   columns — never renumber, reorder, or repurpose an existing field. Do **not**
   lean on Generic-derived record/constructor layout for anything cross-version:
   that caused the era-4 wire incident. (The reload payload's frozen historical
   shapes are Generic but era-*gated* — only decoded at an exact era match —
   which is why a shape change to the hot core MUST bump `reloadEra`.)
3. **When additive won't do, migrate — never lose state.** Bump the version and
   keep a decoder + forward migration for each older version; a clean restart /
   data loss is the floor, not the goal.
4. **Degrade safely on the unknown.** A newer field a reader doesn't recognize
   is skipped, not fatal. A version it genuinely can't handle fails *safe* —
   never corrupt a store, orphan a process, or silently do the wrong thing.
   Prefer a stable, version-independent core (e.g. the fds/pids to hang up) so
   even an unreadable payload can be cleaned up.
5. **Keep a build-checked version corpus.** Commit one serialized vector per
   version; a test decodes every one into the current shape (armor-style — see
   the `ReloadSpec`/`PersistSpec` corpora). This is what actually *prevents* a
   backward-incompatible change — prose doesn't. Never edit an existing vector;
   append a new one.
6. **The wire upgrades in place: version skew must never break
   `restart-server`.** The handshake negotiates `min(client, server)` and
   accepts every version ≥ the floor (4), in both directions, for all time. The
   command path (hello, `Command`, its replies) is shape-frozen forever, so a
   client at any version can deliver `restart-server` to any older server. Wire
   leaves evolve by tolerant append under a **new dialect level**: new readers
   default what shorter lists omit, the server writes each client's negotiated
   level (`encodeServerMessageAt`), and every historical level's bytes stay
   pinned in the `WireSpec` dialect corpus.

If you can't tell whether a change is compatible, it isn't: add the version, the
migration, and a corpus vector.

## Fail loud, never silently accept

Same spirit as the compatibility rule: the system must never look like it did
something it didn't. A config option or command we don't implement FAILS LOUDLY
(a visible error) — it is never silently stored or ignored.
