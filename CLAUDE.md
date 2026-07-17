# Contributor & agent guidance for HAT

Read alongside `ARCHITECTURE.md` (design + rationale) and `FEATURES.md`
(scope). Your global/personal rules still apply; this file only adds what is
specific to HAT.

## Build, test, run

- Everything goes through `cabal` **inside the dev shell**: `nix develop -c
  cabal build`, `nix develop -c cabal test`, `nix develop -c cabal run hat`.
  Never call built binaries directly (stale artifacts) and never build outside
  `nix develop` (libvterm and its `pkg-config` come from the shell).
- `cabal test` runs the unit, property, and integration suites. The
  integration tests drive the real `hat` binary through a pty, so rebuild
  before trusting a repro. Keep the whole suite fast and green — a flaky test
  is a stop-work emergency, never "pre-existing flakiness."
- Golden fixtures: `cabal run gen-fixtures` regenerates emulator goldens. A
  golden diff is a tripwire; regenerate only when the change is intended.

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

| Boundary | Module | Evolution rule |
|---|---|---|
| Client ↔ server wire | `Hat.Transport.Wire` | Explicit append-only CBOR tags `[tag, …]`, **never** Generic-derived constructor/field numbering. Unknown tag → skip; `Malformed` → fatal. Pinned by golden-byte tests in `WireSpec`. |
| Persistence store | `Hat.Server.Persist` (SQLite) | Core columns never change meaning; evolving fields ride a per-row `extra` JSON column; DDL additive only; reads default anything absent. |
| Reload handover | `Hat.Server.Reload` | Carries a version/era envelope and a stable, version-independent core — enough that a version mismatch can be cleaned up safely rather than orphaning the inherited processes. |

When you add or change any such format, the rule is:

1. **Stamp a version/era.** Put an explicit version at the head so a reader can
   distinguish "older version" from "corrupt bytes." Mirror `protocolVersion`.
2. **Evolve additively.** Append tagged fields or add defaulting columns —
   never renumber, reorder, or repurpose an existing field. Do **not** lean on
   Generic-derived record/constructor layout for anything cross-version: that
   is exactly what caused the era-4 wire incident.
3. **Degrade safely on the unknown.** A newer field a reader doesn't recognize
   is skipped, not fatal. A version it genuinely can't handle fails *safe* — it
   must never corrupt a store, orphan a process, or silently do the wrong
   thing. Prefer a stable, version-independent core (e.g. the fds/pids to hang
   up) so even an unreadable payload can be cleaned up.
4. **Pin it with a golden-byte test**, like `WireSpec`. The bytes are the
   contract — don't "fix" a golden, evolve the format and add a new case.

If you can't tell whether a change is compatible, it isn't: add the envelope
and the golden test.

## Fail loud, never silently accept

Same spirit as the compatibility rule: the system must never look like it did
something it didn't. A config option or command we don't implement FAILS LOUDLY
(a visible error) — it is never silently stored or ignored.
