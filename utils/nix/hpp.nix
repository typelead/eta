{ mkDerivation, base, bytestring, bytestring-trie, directory
, fetchgit, filepath, ghc-prim, stdenv, time, transformers
}:
mkDerivation {
  pname = "hpp";
  version = "0.5.2";
  src = fetchgit {
    url = "https://github.com/rahulmutt/hpp.git";
    sha256 = "1i9hk210cvxflhxsdv0pfxjrxayhgvdkiqnc5cm4l5h04ffcb9a3";
    rev = "75d74f53a34c875285a665d8878e5b363edfbea5";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring bytestring-trie directory filepath ghc-prim time
    transformers
  ];
  executableHaskellDepends = [ base directory filepath time ];
  testHaskellDepends = [ base bytestring transformers ];
  homepage = "https://github.com/acowley/hpp";
  description = "A Haskell pre-processor";
  license = stdenv.lib.licenses.bsd3;
}
