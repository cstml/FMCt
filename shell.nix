let
  release = import ./release.nix;
  packages = import ./package.nix;
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs;[ghcid # ghcide
                           ghc
                           cabal-install
                           haskellPackages.hlint # linting
                           haskellPackages.fourmolu # reformatting
                           #haskellPackages.hindent # reformatting
                           #haskellPackages.ormolu # reformatting
                           gnumake # makefile
                           rlwrap # to be able to easily re-enter last input when in the repl
                          ];
  packages = pkgs.haskellPackages.callPackage packages{}; 
}

