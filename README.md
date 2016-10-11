# GHCVM - Modern Haskell on the JVM

[![Join the chat at https://gitter.im/rahulmutt/ghcvm](https://badges.gitter.im/rahulmutt/ghcvm.svg)](https://gitter.im/rahulmutt/ghcvm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/rahulmutt/ghcvm.svg?branch=master)](https://travis-ci.org/rahulmutt/ghcvm)

GHCVM compiles Haskell to the JVM while maintaining compatibility with GHC 7.10.3's Haskell.

## Getting Started

If you get stuck at any point in the installation, feel free to join us on Gitter (using the chat badge above).

### Prerequisites

Make sure you have the following tools installed on your system:
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [JDK 1.7](http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html) or [JDK 1.8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
  - make sure `javac` and `java` are on the PATH

### Installation

Clone the repository and run the install script at the root of the repo:
```
$ git clone --recursive https://github.com/rahulmutt/ghcvm
$ cd ghcvm
$ ./install.sh
```
Once the installation is done, you will now have access to the following command-line tools:
- **ghcvm** - The main compiler
- **cabalvm** - The package manager and build tool

If they are not working, ensure that your stack binary path is included in your PATH.

The following packages are available upon installation:
- ghc-prim
- integer
- base

### Updating

GHCVM updates pretty fast and we're incorporating new patches on a daily basis that you might want to get access to.

If you have GHCVM already installed, go to the root of this repository's clone on your system, and run the following commands:
```
$ git pull
$ git submodule update --recursive
$ ./cleaninstall.sh

```
This will do a fresh install, recompiling all the core libraries with the most recent compiler.

If you have existing CabalVM projects, make sure you run ```cabalvm clean``` inside each project before proceeding with your normal development so that CabalVM recognizes the updated libraries.

### Tutorial

Now, go through the following tutorials for understanding how GHCVM works and how you can start running programs.

- [Hello GHCVM!](https://github.com/rahulmutt/ghcvm/wiki/Hello-GHCVM!)

### Examples

- [JavaFX Example](https://github.com/rahulmutt/ghcvm-javafx)

### Libraries
To see the list of currently supported libraries, check out the [ghcvm-hackage](https://github.com/rahulmutt/ghcvm-hackage) repository.

## Contributing

As you can see, this project is a large undertaking. If you would love to run your Haskell programs on the JVM and accelerate this project, consult the [wiki page](https://github.com/rahulmutt/ghcvm/wiki/Contributing).

## License
GHCVM is available under the [BSD 3-Clause License](https://opensource.org/licenses/BSD-3-Clause), see `LICENSE` for more information.

## Gratitude

We would like to specifically thank the following groups/people:
- [GHC HQ](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC) for providing a solid API to access the GHC infrastructure for the first stage of code generation (Haskell -> STG).
- [Alois Cochard](https://github.com/aloiscochard) for his [codec-jvm](https://github.com/aloiscochard/codec-jvm) package that we use for code generation.
- [Christopher Wells](https://github.com/ExcaliburZero) for his JAR packaging [utility](https://github.com/ExcaliburZero/zip-jar-haskell).
- [Brian McKenna](https://github.com/puffnfresh) for his bug fixes in the codegen/runtime and implementation of basic IO facilities.
- [Sibi](https://github.com/psibi) for configuring TravisCI.

Thank you guys!
