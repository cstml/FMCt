{ mkDerivation, aeson, base, clay, containers, http-types, lens
, lucid, mtl, parsec, scotty, stdenv, text, transformers, wai-extra
}:
mkDerivation {
  pname = "FMCt";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens mtl parsec text transformers
  ];
  executableHaskellDepends = [
    aeson base clay containers http-types lens lucid mtl parsec scotty
    text transformers wai-extra
  ];
  license = stdenv.lib.licenses.bsd3;
}
