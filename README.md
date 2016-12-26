<p align="center">
  <img src="./eta_logo.png" alt="Eta logo" width="20%" />
</p>



<h1 align="center">Eta - Modern Haskell on the JVM</h1>

[![Join the chat at https://gitter.im/typelead/eta](https://badges.gitter.im/typelead/eta.svg)](https://gitter.im/typelead/eta?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/typelead/eta.svg?branch=master)](https://travis-ci.org/typelead/eta)
[![Build status](https://ci.appveyor.com/api/projects/status/walb8x0befptf86j?svg=true)](https://ci.appveyor.com/project/typelead/eta)


Eta is a dialect of Haskell which runs on the JVM and has the following goals:
- accessibility for beginners from imperative languages, especially Java
- compatibility with GHC 7.10.3's Haskell.

## Current Status
**Version:** 0.0.4

Eta is currently hobby-ready, meaning that you can start using it for your hobby
projects, but you may occasionally come across a compiler panic or unimplemented
functions in the standard library. Currently, the following areas are being worked
on:

- Core library support
- Boilerplate generation for Java FFI imports
- Maven support
- Concurrent runtime

We hope to get a first release out by early January 2017.

See [projects](https://github.com/typelead/eta/projects) for a more detailed
overview of the issues that are being prioritized.

Be warned that since we're playing around with the best way to handle Java interop,
there will be breaking changes in the Foreign Function Interface from time to time.

## Getting Started

If you get stuck at any point in the installation, feel free to join
us on [Gitter](https://gitter.im/typelead/eta). There are two ways to do
installation - via Docker or through manual installation.

### Method 1: Normal Installation

#### Prerequisites

Make sure you have the following tools installed on your system:
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [JDK 1.7](http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html) or [JDK 1.8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
  - make sure `javac` and `java` are on the PATH

#### Installation

Clone the repository and run the install script at the root of the repo:
```
$ git clone --recursive https://github.com/typelead/eta
$ cd eta
$ ./install.sh
```

*Note* - If you omit the `--recursive` flag to `git clone`, you will need to
initialize the project's submodules before running `install.sh`:

```
$ cd eta
$ git submodule update --init --recursive
```

Once the installation is done, you will now have access to the following command-line tools:
- **eta** - The main compiler
- **epm** - The package manager and build tool

If they are not working, ensure that your stack binary path is included in your PATH.

The following packages are available upon installation:
- **ghc-prim**
- **integer**
- **base**

#### Updating

Eta updates pretty fast and we're incorporating new patches on a daily basis that you
might want to get access to.

If you have Eta already installed, go to the root of this repository's clone on your
system, and run the following commands:
```
$ git pull
$ git submodule update --recursive
$ ./cleaninstall.sh

```
This will do a fresh install, recompiling all the core libraries with the most recent
compiler.

If you have existing Epm projects, make sure you run ```epm clean``` inside each
project before proceeding with your normal development so that Epm recognizes the
updated libraries. If you are experiencing errors even after that, you can try deleting
`~/.epm` and running `epm update`.

### Method 2: Docker

1. [Install docker for your distribution](https://docs.docker.com/engine/installation/)
2. `docker run -it psibi/eta`

The above command will give you a environment with `eta` and other
related executables. But note that `eta` is a fast moving target
because of it's development pace. So you may have
to [update eta](https://github.com/typelead/eta#updating). You
can check if it's old by navigating to `/eta` and issuing a `git
log` command.

### Documentation

The documentation can be found in the `docs` [directory](./docs/README.md). Be sure to check out the [FAQ](./docs/source/faq.rst) before asking any questions.

### Tutorials & Examples

For tutorials & examples of using Eta, check out:

- [Repository of Eta Examples](https://github.com/typelead/eta-examples)
- [Eta 2048 Game Implementation](https://github.com/rahulmutt/eta-2048)

### Libraries

To see the list of currently supported libraries, check out [Eta Hackage](https://github.com/typelead/eta-hackage).

## License

Eta is available under the [BSD 3-Clause License](https://opensource.org/licenses/BSD-3-Clause), see `LICENSE` for more information.

## Gratitude

We would like to specifically thank the following groups/people:
- [GHC HQ](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC) for providing a solid API to access the GHC infrastructure for the first stage of code generation (Haskell -> STG).
- [Alois Cochard](https://github.com/aloiscochard) for his [codec-jvm](https://github.com/aloiscochard/codec-jvm) package that we use for code generation.
- [Christopher Wells](https://github.com/ExcaliburZero) for his JAR packaging [utility](https://github.com/ExcaliburZero/zip-jar-haskell).
- [Brian McKenna](https://github.com/puffnfresh) for his bug fixes in the codegen/runtime and implementation of basic IO facilities.
- [Sibi](https://github.com/psibi) for helping out with porting packages and setting up Travis.
- [Anton Gushcha](https://github.com/NCrashed) for giving detailed bug reports on the Java FFI.

Thank you guys!
