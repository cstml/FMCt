{ mkDerivation, base, containers, mtl, parsec, QuickCheck, stdenv
, text
}:
mkDerivation {
  pname = "FMCt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl parsec text ];
  executableHaskellDepends = [
    base containers mtl parsec QuickCheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
