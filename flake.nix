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
        default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghc
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
