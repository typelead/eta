{ mkDerivation, aeson, alex, array, base, binary, bytestring
, codec-jvm, containers, cpphs, deepseq, directory, eta-boot
, eta-boot-meta, eta-meta, eta-repl, exceptions, filepath, gitrev
, happy, haskeline, hpc, hpp, mtl, path, path-io, process
, semigroups, stdenv, text, time, transformers, turtle, unix
, unix-compat, zip
}:
mkDerivation {
  pname = "eta";
  version = "0.8.0.3";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring codec-jvm containers cpphs deepseq
    directory eta-boot eta-boot-meta eta-meta eta-repl exceptions
    filepath gitrev hpc hpp mtl path path-io process semigroups text
    time transformers unix unix-compat zip
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    aeson array base bytestring containers deepseq directory eta-boot
    eta-repl filepath haskeline process text time transformers turtle
    unix
  ];
  license = stdenv.lib.licenses.bsd3;
}
