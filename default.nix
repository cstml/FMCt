{ release  ? import ./release.nix
, sources  ? import ./nix/sources.nix
, pkgs     ? import sources.nixpkgs{}
, FMCt     ? import ./FMCt.nix
}:
rec{
  package = pkgs.haskellPackages.callPackage FMCt{ };
}

