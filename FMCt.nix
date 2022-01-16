{ mkDerivation, aeson, base, clay, containers, data-default
, hedgehog, http-types, lens, lucid, mtl, parsec, QuickCheck
, scotty, stdenv, tasty, tasty-expected-failure, tasty-hedgehog
, tasty-hunit, tasty-quickcheck, text, transformers, wai-extra
}:
mkDerivation {
  pname = "FMCt";
  version = "0.6.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default lens parsec transformers
  ];
  executableHaskellDepends = [
    aeson base clay containers data-default http-types lens lucid mtl
    parsec scotty text transformers wai-extra
  ];
  testHaskellDepends = [
    aeson base clay containers data-default hedgehog http-types lens
    lucid mtl parsec QuickCheck scotty tasty tasty-expected-failure
    tasty-hedgehog tasty-hunit tasty-quickcheck text transformers
    wai-extra
  ];
  license = stdenv.lib.licenses.bsd3;
}
