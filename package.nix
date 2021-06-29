{ mkDerivation, base, containers, mtl, parsec, stdenv, text, scotty }:
mkDerivation {
  pname = "FMCt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    mtl
    parsec
    scotty
    text
  ];
  license = stdenv.lib.licenses.bsd3;
}
