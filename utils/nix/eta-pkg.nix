{ mkDerivation, base, binary, bytestring, Cabal, containers
, directory, eta-pkgdb, filepath, process, stdenv, terminfo, unix
}:
mkDerivation {
  pname = "eta-pkg";
  version = "0.0.5";
  src = ../eta-pkg;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring Cabal containers directory eta-pkgdb
    filepath process terminfo unix
  ];
  description = "XXX";
  license = stdenv.lib.licenses.bsd3;
}
