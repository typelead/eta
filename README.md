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
### Simple Program
The equivalent of the program below
```haskell
map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f [] = []

caf :: [Int]
caf = map (\x -> x + 1) [1..1000]

sum :: [Int] -> Int
sum (x:xs) = x + sum xs
sum _ = 0

main :: IO ()
main = do
  print $ sum caf
  return ()
```
has been successfully hand-compiled to work with the GHCVM RTS. The command

```shell
$ ./run.sh
```

is used to run this program.

### GHCVM Compiler
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
  - HotSpot VM
  - Dalvik VM (for Android compatibility, a lightweight runtime)
- Support hot code reloading 
- Re-use GHC's infrastructure
  - Keep a several-month lag with respect to ghc's release cycle
  - CLI should match that of ghc's

## Progress

### Completed Items
- A majority of the single- and multi-threaded RTS and primops have been implemented.
- A sample hand-compiled program is ready. See `sample/mapandsum`.

### Pending Items
1. Work on the code generator.
2. Port the `base` library.

## Contributing

As you can see, this project is a large undertaking. If you would love to run your Haskell programs on the JVM and accelerate this project, join us on Gitter and we'll let you know how you can help out.

## License
GHCVM is available under the [BSD 3-Clause License](https://opensource.org/licenses/BSD-3-Clause), see `LICENSE` for more information.

## Gratitude

We are grateful that the folks at [GHC HQ](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC) have generously open-sourced their state-of-the-art Haskell compiler allowing us to hack on it to implement Haskell on a wide variety of platforms.

We are also grateful for Ilya V. Portnov for his [hs-java](https://hackage.haskell.org/package/hs-java) package that we intend to use heavily for code generation.

Thank you guys!
