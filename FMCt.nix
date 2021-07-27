{ mkDerivation, base, containers, mtl, parsec, QuickCheck, stdenv
, tasty, text, transformers
}:
mkDerivation {
  pname = "FMCt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers mtl parsec text transformers
  ];
  executableHaskellDepends = [
    base containers mtl parsec text transformers
  ];
  testHaskellDepends = [
    base containers mtl parsec QuickCheck tasty text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
