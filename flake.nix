{
  description = "Dev shell flake for fdroid-build";
  inputs.fcitx5-android.url = "github:fcitx5-android/fcitx5-android";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.plugin-scaffold = {
    url = "github:fcitx5-android/plugin-scaffold";
    inputs.nixpkgs.follows = "fcitx5-android/nixpkgs";
    inputs.flake-utils.follows = "fcitx5-android/flake-utils";
  };
  outputs = { self, fcitx5-android, flake-utils, plugin-scaffold, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = fcitx5-android.inputs.nixpkgs;
        pkgs = import nixpkgs {
          inherit system;
          config.android_sdk.accept_license = true;
          config.allowUnfree = true;
          overlays = [ fcitx5-android.overlays.default ];
        };
        sdk = pkgs.fcitx5-android.sdk;
        plugin-scaffold-exe = plugin-scaffold.packages.${system}.default;
        fdroid-builder = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callPackage ./nix { }) (drv: {
            buildTools = drv.buildTools or [ ] ++ [ pkgs.makeWrapper ];
            postInstall = with pkgs;
              drv.postInstall or "" + ''
                wrapProgram $out/bin/fdroid-build \
                  --prefix PATH ":" "${
                    lib.makeBinPath [
                      plugin-scaffold-exe
                      nvchecker
                      unzip
                      rsync
                    ]
                  }"
              '';
          });
        fdroid-builder-shell-f = inCI:
          with pkgs;
          (haskell.lib.addBuildTools fdroid-builder ([ cabal-install ]
            ++ (if !inCI then [
              haskell-language-server
              cabal2nix
            ] else
              [ ]))).env;
        ourShell = inCI:
          with pkgs;
          let fdroid-builder-shell = fdroid-builder-shell-f inCI;
          in (sdk.shell.override {
            androidStudio = null;
            generateLocalProperties = false;
          }).overrideAttrs (old: {
            buildInputs = old.buildInputs
              ++ [ plugin-scaffold-exe nvchecker unzip rsync ]
              ++ fdroid-builder-shell.buildInputs;
            nativeBuildInputs = old.nativeBuildInputs
              ++ fdroid-builder-shell.nativeBuildInputs;
          });
      in with pkgs; {
        devShells.default = ourShell false;
        devShells.ci = ourShell true;
        packages.default = haskell.lib.justStaticExecutables fdroid-builder;
      });
}
