{ mkDerivation, array, base, binary, bytestring, containers, mtl
, stdenv, stringsearch, text, transformers
}:
mkDerivation {
  pname = "codec-jvm";
  version = "0.1";
  src = ../../codec-jvm;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers mtl stringsearch text
    transformers
  ];
  executableHaskellDepends = [ base bytestring text ];
  license = stdenv.lib.licenses.asl20;
}
