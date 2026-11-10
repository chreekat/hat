{
  description = "Hat — a terminal multiplexer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # The Zig toolchain libghostty-vt builds with (nix/libghostty-vt.nix), on a
    # fixed rev so a `nix flake update` cannot swap the compiler under the
    # emulator. Bump it deliberately, with the library.
    nixpkgs-ghostty.url = "github:NixOS/nixpkgs/f4a03c008967841f2a0b3cb7c436fc6fc31671f6";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # Upstream tmux source, pinned so the regress/ corpus the upstream
    # test-suite runs against is deterministic. Bump the rev to re-baseline
    # against a newer tmux; the xfail list is honest only for the pinned rev.
    tmux-src = {
      url = "github:tmux/tmux/31dccb6bc9521b0ea46307974d071ad7f09f0e9b";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-ghostty, tmux-src, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f (import nixpkgs {
            inherit system;
            overlays = [ (final: prev: {
              libghostty-vt = final.callPackage ./nix/libghostty-vt.nix {
                zig = (import nixpkgs-ghostty { inherit system; }).zig_0_17;
              };
            }) ];
          }));
    in
    {
      packages = forAllSystems (pkgs: rec {
        hat = pkgs.callPackage ./package.nix { };
        # Fully static musl binary — portable beyond Nix (scp it anywhere).
        # Builds GHC from source on a cold cache; the ghc910 set is pinned
        # because hat's base bound is 4.20 and pkgsStatic defaults newer.
        hat-static = pkgs.pkgsStatic.callPackage ./package.nix {
          haskellPackages = pkgs.pkgsStatic.haskell.packages.ghc910;
        };
        default = hat;
      });

      devShells = forAllSystems (pkgs: {
        # shellFor gives cabal a GHC whose global db already holds hat's
        # dependency closure, derived from the .cabal file. That is what
        # `active-repositories: :none` requires: cabal fetches nothing, so
        # every dep must already be in the global db. A bare
        # haskellPackages.ghc carries only boot libraries, so vector and the
        # other non-boot deps would be absent.
        default = pkgs.haskellPackages.shellFor {
          # ghc-debug-stub is added to the shell's db (not to package.nix) so a
          # `cabal build -fghc-debug` resolves it while production stays clean.
          packages = p: [
            (pkgs.haskell.lib.addBuildDepend
              (pkgs.callPackage ./package.nix { }).withTests p.ghc-debug-stub)
          ];
          nativeBuildInputs = [ pkgs.pkg-config ];
          # Installs the pre-commit hook that guards hat.nix against drift
          # from hat.cabal (see scripts/check-hat-nix.sh). Idempotent, and
          # worktree-safe via `git rev-parse --git-path`.
          shellHook = ''
            if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
              hookdir=$(git rev-parse --git-path hooks)
              mkdir -p "$hookdir"
              ln -sf "$(git rev-parse --show-toplevel)/scripts/pre-commit" \
                "$hookdir/pre-commit"
            fi
            # Pinned tmux source for the upstream regress corpus, so
            # tools/run-upstream-tests.sh and the hat-upstream test-suite
            # find it without a hardcoded path.
            export HAT_TMUX_SRC=${tmux-src}
            # Programs the integration tests run inside panes, handed to the
            # suite out-of-band instead of on PATH: a bare `pkgs.vim` on the
            # interactive PATH shadows the developer's own vim (bug d9). The
            # suite prepends this to its private PATH (see `testPath`).
            export HAT_TEST_TOOLS=${pkgs.vim}/bin:${pkgs.htop}/bin
          '';
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            # libghostty-vt backs the terminal emulator; the pkg-config setup
            # hook finds its .pc files.
            pkgs.libghostty-vt
            pkgs.haskellPackages.weeder

            # Upstream tmux regress/ scripts need FHS-ish utilities on PATH.
            pkgs.coreutils
            pkgs.diffutils
            pkgs.gawk
            pkgs.gnugrep
            pkgs.gnused
            pkgs.procps

            # System-independent PERF benchmarks: instruction counts.
            pkgs.perf
          ];
        };
      });
    };
}
