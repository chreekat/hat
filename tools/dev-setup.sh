#!/usr/bin/env sh
# Point cabal at the nix-store libvterm for iterating with the system GHC
# (outside `nix develop`). Writes cabal.project.local, which is gitignored.
set -eu

cd "$(dirname "$0")/.."
vterm=$(nix build --no-link --print-out-paths nixpkgs#libvterm-neovim)

cat > cabal.project.local <<EOF
package hat
    extra-include-dirs: $vterm/include
    extra-lib-dirs: $vterm/lib
    ghc-options: -optl-Wl,-rpath,$vterm/lib
EOF
echo "wrote cabal.project.local for $vterm"
