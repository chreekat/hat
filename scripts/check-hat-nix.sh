#!/usr/bin/env bash
# Regenerate hat.nix from hat.cabal and fail if the committed file is stale.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

warn () { echo >&2 "$@"; }

if ! command -v cabal2nix >/dev/null 2>&1; then
    warn "check-hat-nix: cabal2nix not on PATH; run inside 'nix develop'."
    exit 1
fi

generated=$(cabal2nix ./.)

if ! diff -u hat.nix <(printf '%s\n' "$generated"); then
    warn
    warn "check-hat-nix: hat.nix is out of sync with hat.cabal."
    warn "Regenerate it with:  nix develop -c scripts/gen-hat-nix.sh"
    warn "then 'git add hat.nix' and commit again."
    exit 1
fi
