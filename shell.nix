{ release  ? import ./release.nix
, sources  ? import ./nix/sources.nix
, pkgs     ? import sources.nixpkgs{}
, packages ? import ./package.nix
, pkgs2    ? import <nixpkgs>{}
}:
let 
  # add ghc packages that should be available to the ghc here
  ghc = (pkgs.haskellPackages.ghcWithPackages (
    hpkgs: with hpkgs;[
      zlib
      scotty
      aeson
      blaze-markup
      aeson
      lens
    ]
  ));
in
pkgs.mkShell rec{
  bi1 = with pkgs;[
    ghcid # ghcide
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
  bi2 = with pkgs2;[
    haskellPackages.haskell-language-server 
  ];
  buildInputs = bi1 ++ bi2; 
}

