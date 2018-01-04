{ mkDerivation, base, containers, directory, etlas-cabal, shake
, stdenv
}:
mkDerivation {
  pname = "eta-build";
  # @VERSION_CHANGE@
  version = "0.7.0";
  src = ../../shake;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory etlas-cabal shake
  ];
  license = stdenv.lib.licenses.bsd3;
}
