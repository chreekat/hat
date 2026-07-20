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
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f nixpkgs.legacyPackages.${system});
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
          buildInputs = [
            pkgs.cabal-install
            pkgs.libvterm-neovim
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
