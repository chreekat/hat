#!/usr/bin/env bash
# Regenerate hat.nix from hat.cabal and fail if the committed file is stale.
# The flake's package.nix callPackages ./hat.nix instead of running
# callCabal2nix at eval time (no IFD); this guard is what keeps that
# committed expression from silently drifting away from hat.cabal.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

if ! command -v cabal2nix >/dev/null 2>&1; then
    echo "check-hat-nix: cabal2nix not on PATH; run inside 'nix develop'." >&2
    exit 1
fi

generated=$(cabal2nix ./.)

if ! diff -u hat.nix <(printf '%s\n' "$generated"); then
    echo >&2
    echo "check-hat-nix: hat.nix is out of sync with hat.cabal." >&2
    echo "Regenerate it with:  nix develop -c scripts/gen-hat-nix.sh" >&2
    echo "then 'git add hat.nix' and commit again." >&2
    exit 1
fi
