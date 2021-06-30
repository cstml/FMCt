{ release  ? import ./release.nix
, sources  ? import ./nix/sources.nix
, pkgs     ? import sources.nixpkgs{}
, packages ? import ./package.nix
}:pkgs.haskellPackages.callPackage ./package.nix { }


