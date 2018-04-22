{ mkDerivation, base, binary, bytestring, deepseq, eta-boot-meta
, stdenv
}:
mkDerivation {
  pname = "eta-repl";
  version = "0.8.0.2";
  src = ../../libraries/eta-repl;
  libraryHaskellDepends = [
    base binary bytestring deepseq eta-boot-meta
  ];
  description = "The library supporting Eta's interactive interpreter";
  license = stdenv.lib.licenses.bsd3;
}
