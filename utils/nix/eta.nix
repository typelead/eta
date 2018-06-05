{ mkDerivation, aeson, alex, array, base, bytestring, codec-jvm
, containers, cpphs, deepseq, directory, eta-boot, eta-boot-meta
, exceptions, filepath, happy, haskeline, hpc, mtl, path, path-io
, process, stdenv, text, time, transformers, turtle, unix
, unix-compat, zip
}:
mkDerivation {
  pname = "eta";
  # @VERSION_CHANGE@
  # @BUILD_NUMBER@
  # @BUILD_NUMBER_INTERNAL@
  version = "0.8.0.2";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring codec-jvm containers cpphs deepseq directory
    eta-boot eta-boot-meta exceptions filepath hpc mtl path path-io
    process text time transformers unix unix-compat zip
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    array base bytestring deepseq directory filepath haskeline process
    transformers unix
  ];
  testHaskellDepends = [
    aeson base bytestring directory filepath text turtle
  ];
  license = stdenv.lib.licenses.bsd3;
}
