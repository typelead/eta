{ mkDerivation, base, containers, directory, etlas-cabal, shake
, stdenv
}:
mkDerivation {
  pname = "eta-build";
  version = "0.0.6";
  src = ../../shake;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory etlas-cabal shake
  ];
  license = stdenv.lib.licenses.bsd3;
}
