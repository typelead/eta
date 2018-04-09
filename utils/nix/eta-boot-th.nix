{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "eta-boot-th";
  # @VERSION_CHANGE@
  version = "0.7.2";
  src = ../../libraries/eta-boot-th;
  libraryHaskellDepends = [ base ];
  description = "Shared functionality between Eta and the @template-haskell@ library";
  license = stdenv.lib.licenses.bsd3;
}
