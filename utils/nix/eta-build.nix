{ mkDerivation, base, Cabal, containers, directory, shake, stdenv
}:
mkDerivation {
  pname = "eta-build";
  version = "0.0.5";
  src = ../../shake;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers directory shake
  ];
  license = stdenv.lib.licenses.bsd3;
}
