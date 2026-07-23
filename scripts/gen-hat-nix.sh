#!/usr/bin/env bash
# Regenerate hat.nix from hat.cabal in place.
#
# Writes via a temp file: `... > hat.nix` truncates hat.nix first, and since
# the flake's devShell reads hat.nix at eval time, that would brick the file
# before cabal2nix ever runs. Generate to a temp path, then move into place.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

if ! command -v cabal2nix >/dev/null 2>&1; then
    echo "gen-hat-nix: cabal2nix not on PATH; run inside 'nix develop'." >&2
    exit 1
fi

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
cabal2nix ./. > "$tmp"
mv "$tmp" hat.nix
trap - EXIT
echo "gen-hat-nix: wrote hat.nix"
