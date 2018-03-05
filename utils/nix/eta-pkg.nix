{ mkDerivation, base, binary, bytestring, containers, directory
, eta-boot, etlas-cabal, filepath, process, stdenv, terminfo, unix
}:
mkDerivation {
  pname = "eta-pkg";
  # @VERSION_CHANGE@
  # @BUILD_NUMBER@
  # @BUILD_NUMBER_INTERNAL@
  version = "0.7.1.1";
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
