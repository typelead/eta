{ mkDerivation, base, binary, bytestring, directory, eta-boot-th
, filepath, stdenv
}:
mkDerivation {
  pname = "eta-boot";
  version = "0.0.6";
  src = ../../libraries/eta-boot;
  libraryHaskellDepends = [
    base binary bytestring directory eta-boot-th filepath
  ];
  description = "Shared functionality between Eta and its boot libraries";
  license = stdenv.lib.licenses.bsd3;
}
