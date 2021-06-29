{ release  ? import ./release.nix
, sources  ? import ./nix/sources.nix
, pkgs     ? import sources.nixpkgs{}
, packages ? import ./package.nix
}:
pkgs.mkShell {
  buildInputs = with pkgs;[ghcid # ghcide
                           zlib
                           ghc
                           cabal-install
                           haskellPackages.hlint # linting
                           haskellPackages.zlib 
                           #haskellPackages.fourmolu # reformatting
                           #haskellPackages.hindent # reformatting
                           #haskellPackages.ormolu # reformatting
                           gnumake # makefile
                           rlwrap # to be able to easily re-enter last input when in the repl
                          ];
  packages = pkgs.haskellPackages.callPackage packages{}; 
}

