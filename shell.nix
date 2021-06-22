let
  release = import ./release.nix;
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs;[ghcid # ghcide
                           haskellPackages.hlint # linting
                           haskellPackages.fourmolu # reformatting
                           #haskellPackages.hindent # reformatting
                           #haskellPackages.ormolu # reformatting
                           gnumake # makefile
                          ];
}

