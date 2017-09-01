@echo off
:: (Re)build the Verify.java script and copy the class file here
echo Building the Verify script...
javac ../../utils/class-verifier/Verify.java || exit /b
copy ..\..\utils\class-verifier\Verify.class . 
echo Verify.class built successfully.

:: Remove old build artifacts
rmdir build /S /Q

:: Compile a simple program and extract the files
echo Compiling a simple program...
echo === Eta Compiler Output ===
mkdir build
eta -fforce-recomp -o build/Out.jar Main.hs || exit /b
echo ===                     ===
echo Compiled succesfully.

:: Do bytecode verification on all the core libraries' class files
echo Verifying the bytecode of compiled program...
echo === Verify Script Output ===
java Verify build/Out.jar || exit /b
echo ===                      ===
echo Bytecode looking good.

:: Make sure a simple "Hello World!" program runs
echo Running the simple program...
echo === Simple Program Output ===
java -cp build/Out.jar eta.main || exit /b
echo ===                       ===
echo Done! Everything's looking good.
