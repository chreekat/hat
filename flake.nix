{
  description = "HAT — a terminal multiplexer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      # Patch the vendored libvterm: teach it the SGR 2 (faint/dim) attribute
      # it otherwise drops (nix/libvterm-dim.patch), and stop its screen_resize
      # from abort()ing the whole process when an extreme shrink of reflowed
      # content leaves the cursor unresolved (nix/libvterm-resize-clamp.patch).
      # hat is the canonical home for the patches; the NixOS deploy overlay
      # references these same files by path.
      overlay = final: prev: {
        libvterm-neovim = prev.libvterm-neovim.overrideAttrs (o: {
          patches = (o.patches or [ ]) ++ [
            ./nix/libvterm-dim.patch
            ./nix/libvterm-resize-clamp.patch
          ];
        });
      };
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f (import nixpkgs {
            inherit system;
            overlays = [ overlay ];
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
          '';
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            pkgs.libvterm-neovim
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
