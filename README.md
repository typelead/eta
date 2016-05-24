# GHCVM - A JVM backend for GHC

[![Join the chat at https://gitter.im/rahulmutt/ghcvm](https://badges.gitter.im/rahulmutt/ghcvm.svg)](https://gitter.im/rahulmutt/ghcvm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This project aims to compile Haskell to the JVM, with the primary goal of seamless compatibility with GHC 7.10.3's Haskell.

## Building
Stack is used to build the code generator and the Shake builder for the RTS. The RTS build is setup so that it allows for C preprocessor directives inside of Java code.

To build everything, simply run the ./build.sh script.
```shell
$ ./build.sh
```

## Running
The ghcvm executable does nothing more than generate a stub .class file for now. Once the code generator is ready, it will do something more exciting.

```shell
$ stack exec -- ghcvm --make Main.hs
```

All the options that are supported by GHC are currently allowed. The options will be filtered in the future.

## Goals 

We aim to meet the following goals:

- Easy interop with Java libraries
- High performance lazy functional language implementation
- Seamless integration with Java IDEs
  - IntelliJ
  - Eclipse
  - Android Studio
- Optimize for specific JVM implementations
  - OpenJDK HotSpot
  - Dalvik VM (for Android compatibility, a lightweight runtime)
- Support hot code reloading 
- Re-use GHC's infrastructure
  - Keep up with their release cycle
  - CLI should match that of ghc's

## Progress

### Completed Items

- Intercepted the STG Code in GHC pipeline
- Updated hs-java to add support for inner classes and did some cosmetic refactorings.
- Wrote very basic runtime system with support for some primitive data types like StgClosure, StackFrame, etc.

### Pending Items
1. Work on the code generator taking the StgCmm*.hs files in the GHC repo as inspiration.
1. Work on runtime system implemented more RTS functions from GHC as required during code generation. 
2. Implement as many of GHC's PrimOps as possible.
3. Change the base library to use the Java FFI instead of C FFI.
4. Compile a single source file to run on the JVM.
and more...

## Contributing

As you can see, this project is a large undertaking. If you would love to run your Haskell programs on the JVM and accelerate this project, get in touch and we'll let you know how you can help out.

## License
GHCVM is available under the [BSD 3-Clause License](https://opensource.org/licenses/BSD-3-Clause), see `LICENSE` for more information.

## Gratitude

We are grateful that the folks at [GHC HQ](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC) have generously open-sourced their state-of-the-art Haskell compiler allowing us to hack on it to implement Haskell on a wide variety of platforms.

We are also grateful for Ilya V. Portnov for his [hs-java](https://hackage.haskell.org/package/hs-java) package that we intend to use heavily for code generation.

Thank you guys!

