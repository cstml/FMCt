let
  release = import ./release.nix;
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs;[ghcid # ghcide
                           ghc
                           haskellPackages.hlint # linting
                           haskellPackages.fourmolu # reformatting
                           #haskellPackages.hindent # reformatting
                           #haskellPackages.ormolu # reformatting
                           gnumake # makefile
                           rlwrap # to be able to easily re-enter last input when in the repl
                          ];
}

