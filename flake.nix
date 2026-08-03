{
  description = "HAT — a terminal multiplexer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # libghostty-vt is 0.1.0-unstable with an evolving C ABI (bug 17), so it is
    # pinned to a fixed nixpkgs rev — decoupled from the rolling nixpkgs above —
    # and a `nix flake update` cannot silently swap the emulator's ABI out from
    # under a running deploy. Bump this rev deliberately to move libghostty-vt.
    nixpkgs-ghostty.url = "github:NixOS/nixpkgs/d407951447dcd00442e97087bf374aad70c04cea";
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
            # Take libghostty-vt from the pinned rev; everything else tracks
            # nixpkgs above.
            overlays = [ (final: prev: {
              libghostty-vt =
                (import nixpkgs-ghostty { inherit system; }).libghostty-vt;
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
          packages = p: [ (pkgs.callPackage ./package.nix { }).withTests ];
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
          '';
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            # libghostty-vt backs the terminal emulator (bug 17). Its pkg-config
            # files live in the dev output's share/pkgconfig, which the
            # pkg-config setup hook adds to the path.
            pkgs.libghostty-vt
            pkgs.haskellPackages.weeder

            # Upstream tmux regress/ scripts need FHS-ish utilities on PATH.
            pkgs.coreutils
            pkgs.diffutils
            pkgs.gawk
            pkgs.gnugrep
            pkgs.gnused
            pkgs.procps

            # Programs the integration tests run inside panes; declared here so
            # a clean CI shell has them, not just a NixOS system profile.
            pkgs.vim
            pkgs.htop

            # System-independent PERF benchmarks: instruction counts.
            pkgs.perf
          ];
        };
      });
    };
}
