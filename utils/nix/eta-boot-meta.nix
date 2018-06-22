{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "eta-boot-meta";
  version = "0.8.0";
  src = ../../libraries/eta-boot-meta;
  libraryHaskellDepends = [ base ];
  description = "Shared functionality between Eta and the @eta-meta@ library";
  license = stdenv.lib.licenses.bsd3;
}
