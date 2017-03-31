{ mkDerivation, array, base, binary, bytestring, containers, mtl
, stdenv, text
}:
mkDerivation {
  pname = "codec-jvm";
  version = "0.1";
  src = ../../codec-jvm;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers mtl text
  ];
  executableHaskellDepends = [ base bytestring text ];
  license = stdenv.lib.licenses.asl20;
}
