{ mkDerivation, base, binary, bytestring, deepseq, eta-boot
, eta-boot-meta, eta-repl, pretty, stdenv
}:
mkDerivation {
  pname = "eta-meta";
  version = "0.8.0.3";
  src = ../../libraries/eta-meta;
  libraryHaskellDepends = [
    base binary bytestring deepseq eta-boot eta-boot-meta eta-repl
    pretty
  ];
  description = "Support library for Template Metaprogramming in Eta";
  license = stdenv.lib.licenses.bsd3;
}
