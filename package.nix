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
{ haskell, haskellPackages, libghostty-vt, nix-gitignore }:

let
  # Checks and benchmarks stay enabled here so the package's test AND
  # benchmark dependencies are part of its build-input closure; the flake
  # devShell derives its GHC from this via shellFor and needs them to run
  # `cabal test` and `cabal bench`. shellFor pulls testHaskellDepends only
  # when checks are enabled and benchmarkHaskellDepends only when benchmarks
  # are — under `active-repositories: :none` a dep absent from the global db
  # is a hard failure, not a Hackage fetch, so the shell must carry both.
  #
  # hat.nix is the committed cabal2nix output (kept in sync by the
  # pre-commit hook in scripts/); callPackage on it avoids the IFD a live
  # callCabal2nix would incur. src is overridden to the gitignore-filtered
  # tree so a callPackage on a live checkout does not copy dist-newstyle.
  withTests = haskell.lib.doBenchmark
    ((haskellPackages.callPackage ./hat.nix {
      libghostty-vt = libghostty-vt;
    }).overrideAttrs (_: {
      src = nix-gitignore.gitignoreSource [ ] ./.;
    }));
  # dontCheck: the test suite locates the hat binary via `cabal list-bin`
  # and drives it through real ptys alongside dev tools (vim, htop, …) —
  # it runs in the dev shell (`cabal test`), not in the build sandbox.
  base = haskell.lib.dontCheck withTests;
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
    # shellFor derives the devShell's GHC from this; checks-enabled so test
    # deps come along.
    passthru = (drv.passthru or { }) // { inherit withTests; };
  })
