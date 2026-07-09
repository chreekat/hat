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
        hat = pkgs.haskellPackages.callCabal2nix "hat" ./. {
          vterm = pkgs.libvterm-neovim;
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
          ];
        };
      });
    };
}
