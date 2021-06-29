let
  pkgs = import <nixpkgs> { };
in
pkgs.haskellPackages.callPackage ./package.nix { }


