#!/usr/bin/env sh

set -e

# (Re)build the Verify.java script and copy the class file here
echo "Building the Verify script..."
javac ../../utils/class-verifier/Verify.java
cp ../../utils/class-verifier/Verify.class .
echo "Verify.class built successfully."

# Clean-up the previous result
rm -rf build/ base/ integer/ ghczmprim/ cern/ eta/ main/

# Compile a simple program and extract the files
echo "Compiling a simple program..."
echo "=== Eta Compiler Output ==="
mkdir build
eta -fforce-recomp -o build/Out.jar Main.hs && jar xf build/Out.jar
echo "===                     ==="
echo "Compiled succesfully."

# Do bytecode verification on all the core libraries' class files
echo "Verifying the bytecode of compiled program..."
echo "=== Verify Script Output ==="
java Verify ghczmprim/ghc integer/ghc integer/ghc/integer integer/ghc/integer/biginteger integer/ghc/integer/logarithms base base/control base/control/concurrent base/control/exception base/control/monad base/control/monad/st base/control/monad/st/lazy base/data base/data/functor base/data/stref base/data/type base/data/typeable base/debug base/foreign base/foreign/foreignptr base/foreign/marshal base/ghc base/ghc/conc base/ghc/event base/ghc/fingerprint base/ghc/float base/ghc/io base/ghc/io/encoding base/ghc/io/handle base/ghc/rts base/numeric base/system base/system/console base/system/environment base/system/io base/system/mem base/system/posix base/text base/text/parsercombinators base/text/read base/text/show base/unsafe base/java
echo "===                      ==="
echo "Bytecode looking good."

# Make sure a simple "Hello World!" program runs
echo "Running the simple program..."
echo "=== Simple Program Output ==="
java -cp build/Out.jar eta.main
echo "===                       ==="
echo "Done! Everything's looking good."
