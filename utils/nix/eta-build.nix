{ mkDerivation, base, containers, directory, etlas-cabal, shake
, stdenv
}:
mkDerivation {
  pname = "eta-build";
  # @VERSION_CHANGE@
  version = "0.0.9";
  src = ../../shake;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory etlas-cabal shake
  ];
  license = stdenv.lib.licenses.bsd3;
}
