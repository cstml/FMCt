{ mkDerivation, aeson, base, clay, containers, http-types, lens
, lucid, parsec, scotty, stdenv, text, wai-extra
}:
mkDerivation {
  pname = "FMCt";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens parsec ];
  executableHaskellDepends = [
    aeson base clay containers http-types lens lucid parsec scotty text
    wai-extra
  ];
  license = stdenv.lib.licenses.bsd3;
}
