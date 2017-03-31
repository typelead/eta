{ mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, extensible-exceptions, filepath, HUnit
, old-time, pretty, process, QuickCheck, regex-posix, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, time, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "1.22.8.0";
  src = ../../epm/Cabal;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    pretty process time unix
  ];
  testHaskellDepends = [
    base bytestring containers directory extensible-exceptions filepath
    HUnit old-time process QuickCheck regex-posix test-framework
    test-framework-hunit test-framework-quickcheck2 unix
  ];
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
