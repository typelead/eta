{ mkDerivation, base, binary, bytestring, directory, eta-boot-meta
, filepath, stdenv
}:
mkDerivation {
  pname = "eta-boot";
  version = "0.8.0";
  src = ../../libraries/eta-boot;
  libraryHaskellDepends = [
    base binary bytestring directory eta-boot-meta filepath
  ];
  description = "Shared functionality between Eta and its boot libraries";
  license = stdenv.lib.licenses.bsd3;
}
