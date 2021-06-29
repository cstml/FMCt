{ mkDerivation
, base
, containers
, mtl
, parsec
, stdenv
, text
, scotty
, base64-bytestring
}:
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
    base64-bytestring
  ];
  license = stdenv.lib.licenses.bsd3;
}
