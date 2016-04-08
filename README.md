# GHCVM - A JVM backend for GHC

This project aims to compile Haskell to the JVM, with the primary goal of seamless compatibility with GHC's Haskell.

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
- Came up with an efficient design for a JVM Runtime System for Haskell that:
  - Has full support of general tail calls
  - Has a memory-efficient solution to overriding a thunk with its value (at the cost of complicated implementation and a high-level GC layer)
  - Can interoperate with Java seamlessly
- Intercepted the STG Code in GHC pipeline

### Pending Items
1. Write the minimum code generator and run time system implementing the design. 
2. Compile a single source file to run on the JVM.
3. ...

## Contributing

As you can see, this project is a large undertaking. If you would love to run your Haskell programs on the JVM and accelerate this project, get in touch and we'll let you know how you can help out.

## Gratitude

We are grateful that the folks at [GHC HQ](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC) have generously open-sourced their state-of-the-art Haskell compiler allowing us to hack on it to implement Haskell on a wide variety of platforms.

We are also grateful for Ilya V. Portnov for his [hs-java](https://hackage.haskell.org/package/hs-java) package that we intend to use heavily for code generation.

Thank you guys!

