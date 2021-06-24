{ release  ? import ./release.nix
, sources  ? import ./nix/sources.nix
, pkgs     ? import sources.nixpkgs{}
, dotfiles ? import sources.dotfiles{}
}:
let 
  
in
pkgs.mkShell {
  buildInputs = with pkgs;[ghcid # ghcide
                           zlib
                           ghc
                           haskellPackages.hlint # linting
                           haskellPackages.zlib 
                           #haskellPackages.fourmolu # reformatting
                           #haskellPackages.hindent # reformatting
                           #haskellPackages.ormolu # reformatting
                           gnumake # makefile
                           rlwrap # to be able to easily re-enter last input when in the repl
                          ];
}

