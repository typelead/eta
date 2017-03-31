{ mkDerivation, base, binary, bytestring, directory, filepath
, stdenv
}:
mkDerivation {
  pname = "eta-pkgdb";
  version = "0.0.0.0";
  src = ../eta-pkgdb;
  libraryHaskellDepends = [
    base binary bytestring directory filepath
  ];
  description = "The GHC compiler's view of the GHC package database format";
  license = stdenv.lib.licenses.bsd3;
}
