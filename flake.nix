{
  description = "FMCt Programming Language";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };

        additionalPckgs = with pkgs; [ nixfmt ];

        additionalHaskellPckgs = with pkgs.haskellPackages; [
          apply-refact
          cabal-fmt
          cabal-install
          fourmolu
          ghcid
          hasktags
          hlint
          zlib
        ];

        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "FMCt";
            root = self;
            withHoogle = true;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
              (additionalHaskellPckgs ++ additionalPckgs);
          };
      in {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
