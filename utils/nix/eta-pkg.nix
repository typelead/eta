{ mkDerivation, base, binary, bytestring, containers, directory
, eta-boot, etlas-cabal, filepath, process, stdenv, terminfo, unix
}:
mkDerivation {
  pname = "eta-pkg";
  version = "0.0.6";
  src = ../../utils/eta-pkg;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers directory eta-boot etlas-cabal
    filepath process terminfo unix
  ];
  description = "A utility for querying and managing the Eta package database";
  license = stdenv.lib.licenses.bsd3;
}
