{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "eta-boot-th";
  version = "0.0.6";
  src = ../../libraries/eta-boot-th;
  libraryHaskellDepends = [ base ];
  description = "Shared functionality between Eta and the @template-haskell@ library";
  license = stdenv.lib.licenses.bsd3;
}
