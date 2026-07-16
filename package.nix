# The hat binary with all Haskell libraries statically linked
# (justStaticExecutables); C libraries follow the calling package set.
#
#   pkgs.callPackage ./package.nix { }        — glibc-dynamic, binary-cache
#                                               friendly (the NixOS module
#                                               deployment path)
#   pkgsStatic.callPackage ./package.nix { }  — fully static musl binary
#                                               (see the flake's hat-static)
#
# The source is .gitignore-filtered so a callPackage on a live checkout
# does not copy dist-newstyle into the store.
{ haskell, haskellPackages, libvterm-neovim, nix-gitignore }:

let
  # dontCheck: the test suite locates the hat binary via `cabal list-bin`
  # and drives it through real ptys alongside dev tools (vim, htop, …) —
  # it runs in the dev shell (`cabal test`), not in the build sandbox.
  base = haskell.lib.dontCheck
    (haskellPackages.callCabal2nix "hat"
      (nix-gitignore.gitignoreSource [ ] ./.) {
        vterm = libvterm-neovim;
      });
in
haskell.lib.overrideCabal
  (haskell.lib.justStaticExecutables base)
  (drv: {
    # gen-fixtures is a test-fixture generator; don't ship it.
    postInstall = (drv.postInstall or "") + ''
      rm -f $out/bin/gen-fixtures
    '';
    # Let `nix run <flakeref>` resolve the binary without guessing.
    mainProgram = "hat";
  })
