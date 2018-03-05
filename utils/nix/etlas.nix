{ mkDerivation, array, async, base, base16-bytestring, binary
, bytestring, Cabal, containers, cryptohash-sha256, deepseq
, directory, echo, edit-distance, etlas-cabal, filepath
, hackage-security, hashable, HTTP, mtl, network, network-uri
, pretty, process, random, stdenv, stm, tar, time, unix, zlib
}:
mkDerivation {
  pname = "etlas";
  # @VERSION
  version = "1.3.0.0";
  src = ../../etlas/etlas;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal filepath process ];
  libraryHaskellDepends = [
    array async base base16-bytestring binary bytestring containers
    cryptohash-sha256 deepseq directory echo edit-distance etlas-cabal
    filepath hackage-security hashable HTTP mtl network network-uri
    pretty process random stm tar time unix zlib
  ];
  executableHaskellDepends = [ base directory etlas-cabal filepath ];
  homepage = "https://eta-lang.org";
  description = "The package manager for Eta";
  license = stdenv.lib.licenses.bsd3;
}
