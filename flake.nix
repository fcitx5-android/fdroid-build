{
  description = "Dev shell flake for fdroid-build";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.plugin-scaffold = {
    url = "github:fcitx5-android/plugin-scaffold";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, plugin-scaffold, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        plugin-scaffold-exe = plugin-scaffold.packages.${system}.default;
        fdroid-builder = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callPackage ./nix { }) (drv: {
            executableSystemDepends = drv.executableSystemDepends or [ ]
              ++ [ plugin-scaffold-exe pkgs.nvchecker ];
          });
      in with pkgs; {
        devShells.default = (haskell.lib.addBuildTools fdroid-builder [
          haskell-language-server
          cabal2nix
          cabal-install
          plugin-scaffold-exe
          nvchecker
        ]).envFunc { };
        packages.default = haskell.lib.justStaticExecutables fdroid-builder;
      });
}
