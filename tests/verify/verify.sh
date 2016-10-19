#!/usr/bin/env sh

set -e

# (Re)build the Verify.java script and copy the class file here
javac ../../utils/class-verifier/Verify.java
cp ../../utils/class-verifier/Verify.class .

# Clean-up the previous result
rm -rf build/ base/ integer/ ghczmprim/ cern/ ghcvm/ main/

# Compile a simple program and extract the files
mkdir build
ghcvm -fforce-recomp -o build/Out.jar Main.hs && jar xf build/Out.jar

# Do bytecode verification on all the core libraries' class files
java Verify ghczmprim/ghc
java Verify integer/ghc
java Verify integer/ghc/integer
java Verify integer/ghc/integer/biginteger
java Verify integer/ghc/integer/logarithms
java Verify base
java Verify base/control
java Verify base/control/concurrent
java Verify base/control/exception
java Verify base/control/monad
java Verify base/control/monad/st
java Verify base/control/monad/st/lazy
java Verify base/data
java Verify base/data/functor
java Verify base/data/stref
java Verify base/data/type
java Verify base/data/typeable
java Verify base/debug
java Verify base/foreign
java Verify base/foreign/foreignptr
java Verify base/foreign/marshal
java Verify base/ghc
java Verify base/ghc/conc
java Verify base/ghc/event
java Verify base/ghc/fingerprint
java Verify base/ghc/float
java Verify base/ghc/io
java Verify base/ghc/io/encoding
java Verify base/ghc/io/handle
java Verify base/ghc/rts
java Verify base/ghcvm
java Verify base/numeric
java Verify base/system
java Verify base/system/console
java Verify base/system/environment
java Verify base/system/io
java Verify base/system/mem
java Verify base/system/posix
java Verify base/text
java Verify base/text/parsercombinators
java Verify base/text/read
java Verify base/text/show
java Verify base/unsafe

# Make sure a simple "Hello World!" program runs
java -cp build/Out.jar ghcvm.main
