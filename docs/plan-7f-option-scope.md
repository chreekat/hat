# Implementation Plan: tmux-style option-scope hierarchy (bug 7f)

## Verified current state
- One options table: `ServerState.options :: TVar Options` (`Model.hs:101`). `Session`/`Window` carry no options.
- `Options` is a flat total record (`Model/Options.hs:38-77`): ~35 fields + `user :: Map Text Text` (@-opts) + `explicit :: Set Text` (user-set names, so `applyPalette` won't clobber).
- `cmdSet` (`Server.hs:1898`) parses only `-t` as value-taking; `-g`/`-s`/`-w` fall into a boolean flag list and only `-a` is read. Target + scope flags silently discarded (breaks fail-loud).
- `cmdShow` (`Server.hs:1933`): same, ignores scope/target.
- `setOptionRaw` (`Server.hs:1989`): `Options -> Text -> Text -> Either Text Options` (Either = fail-loud channel, preserve).
- `lookupOption` (`Server.hs:1947`): `Options -> Text -> Maybe Text`.
- **Options are NOT persisted / reloaded / on the wire** (verified: Persist/Reload/Wire have no options refs; ARCHITECTURE.md:733 — reload re-sources ~/.tmux.conf via serverConfig argv replay). => NO version/corpus work needed.
- ARCHITECTURE.md:331-347 + hooks 668-688 already sketch this target shape (Server/Session options TVars, HookScope walking). This plan realizes an already-blessed design.

### 31 read sites classified
Most are session/window-scoped WITH a session/window already in scope (View.hs render path, handleKeys prefix, markActivity monitor-activity, autoRename, copy-mode modeKeys/wordSeparators, base/pane-base-index, main-pane-*, display-time). Genuinely server-global: restoreWhitelist (@restore-commands at reload, `Server.hs:790-792`). Only two WRITE sites: `Server.hs:1910` (cmdSet) and `Server.hs:451` (applyPalette).

## Key decisions
### A — OptionsDelta representation (NEEDS SIGN-OFF)
- A1: per-field `Maybe` twin record. Type-safe/exhaustive; ~35 fields duplicated, every new option touches two records.
- A2 (recommended): `newtype OptionsDelta = OptionsDelta (Map OptionName OptionValue)` with closed `OptionName` + `OptionValue` sums. Keep resolved `Options` flat & unchanged, so all 31 reads keep reading a plain `Options` — just a *resolved* one. Overlay/merge complexity confined to set/resolve path. `setOptionRaw` -> `Either Text (OptionName, OptionValue)`; `resolveOptions :: [OptionsDelta] -> Options` folds most-specific-first over defaults.

### B — Scope type (no boolean blindness)
`data OptionScope = ServerScope | GlobalSession | SessionScope SessionId | GlobalWindow | WindowScope WindowId`
(SessionId/WindowId exist, Model.hs:4-5). Bare `set` -> SessionScope current; `setw`/`set -w` -> WindowScope current.

### C — Option->scope classification
Single source of truth `optionScopeClass :: OptionName -> ScopeClass` drives both fail-loud rejection and the resolve chain. Session: prefix, base-index, status-*, mode-keys, history-limit, display-time, set-titles, default-terminal, focus-events, update-environment, word-separators, automatic-rename-format. Window: automatic-rename, aggressive-resize, monitor-activity, main-pane-w/h, window-status-*, pane-border-*, pane-base-index.

### D — retire `explicit` / applyPalette (M4)
With deltas, "explicitly set" == "present in session delta". Move palette to global-session delta, consult delta-presence, delete `explicit`. Behavioral refactor -> its own step.

## Milestones (TDD-first, each step ~1-8 commits)
- **M1** Overlay types + scope-aware setOption/resolveOptions (pure core, no model change). Riskiest conceptually. Tests: resolve empty->defaults, session>global, window>session>global, field-by-field precedence, fail-loud reject (setw prefix -> Left).
- **M2** Per-session/-window overlay tables in model. Add `options :: TVar OptionsDelta` to Session/Window; retype st.options; init empty deltas in newServerState/createSession/newWindowWithPane. `resolveForSession`/`resolveForWindow :: STM Options` choke points.
- **M3** Thread target/scope into cmdSet/cmdShow; wire -g/-s/-w/-t/-a + setw alias; fail loud on unsupported combos & unknown targets. Riskiest behaviorally (fixes the silent-discard bug). Keep applyHistoryLimit narrowing per scope.
- **M4** Migrate 31 read sites to resolveForSession/Window; thread window ctx into CopyMode sites; retire explicit + move applyPalette to global-session delta.
- **M5** Reload/restart fidelity: `it "a setw survives restart-server"`; verify serverConfig replay reconstructs per-scope deltas; ARCHITECTURE.md note (options resolve through scope chain, still unpersisted). No compat-corpus row added.

## Decisions (resolved)
1. **A2** — typed `Map OptionName OptionValue`; resolved `Options` stays flat.
2. **mode-keys is a window option** (tmux-faithful); thread window ctx into the 4 CopyMode read sites.
3. **Retire `Options.explicit`** — palette writes the global-session delta and consults delta-presence.

## Critical files
Options.hs, Model.hs, Server.hs, Server/View.hs, test/Hat/Server/OptionsSpec.hs
