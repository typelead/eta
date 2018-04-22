{ mkDerivation, base, binary, bytestring, containers, directory
, eta-boot, etlas-cabal, filepath, stdenv
}:
mkDerivation {
  pname = "eta-pkg";
  version = "0.8.0.3";
  src = ../../utils/eta-pkg;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers directory eta-boot etlas-cabal
    filepath
  ];
  description = "A utility for querying and managing the Eta package database";
  license = stdenv.lib.licenses.bsd3;
}
