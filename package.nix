{ mkDerivation, base, containers, mtl, parsec, stdenv, text }:
mkDerivation {
  pname = "FMCt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers mtl parsec text ];
  license = stdenv.lib.licenses.bsd3;
}
