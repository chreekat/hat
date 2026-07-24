#!/usr/bin/env bash
# The one CI script every forge (builds.sr.ht, GitHub, a local shell) runs, so
# the build logic lives here instead of duplicated across forge YAML.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

echo "==> nix develop -c cabal build (zero warnings; hat.cabal is -Werror)"
nix develop -c cabal build

echo "==> nix develop -c cabal test"
nix develop -c cabal test

echo "==> nix build .#hat"
nix build .#hat

echo "==> CI OK"
