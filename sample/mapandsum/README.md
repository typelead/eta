# Sample: Map and Sum

## Overview
This is the first sample program that has been hand-compiled to work with GHCVM's RTS. The basic Haskell program that currently runs on the single-threaded RTS successfully is shown below:
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude     (Monad(..), ($), (.))
import GHC.IO      (IO(..))
import MinimalBase (printInt, Int, enumFromTo, (+), one, zero, ten)

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f [] = []

caf :: [Int]
caf = map (\x -> x + one) $ enumFromTo one ten

sum :: [Int] -> Int
sum (x:xs) = x + sum xs
sum _ = zero

main :: IO ()
main = do
  printInt . sum $ caf
  return ()
```
Since the codegen isn't ready yet, we have no practical way of accessing the `base` library so we have to work with the low-level haskell primitives. [MinimalBase](haskell/src/MinimalBase.hs) takes care of that for us. I only hand-compiled the minimal set of functions/datatypes from `base` that are required to write this program.

# Building & Running
To build & run the sample program, go to the top level of the repository and run the following two commands:
```
$ ./build.sh
$ ./run.sh
```

# Navigation
| Module | Haskell | STG | Cmm | Java |
| ------ | ------- | --- | --- | ---- |
| Main | [Main.hs](haskell/src/Main.hs) | [Main.dump-stg](codes/Main.dump-stg) | [Main.dump-opt-cmm](codes/Main.dump-opt-cmm) | [Main.java](java/src/mapandsum/Main.java) | 
| MinimalBase | [MinimalBase.hs](haskell/src/MinimalBase.hs) | [MinimalBase.dump-stg](codes/MinimalBase.dump-stg) | [MinimalBase.dump-opt-cmm](codes/MinimalBase.dump-opt-cmm) | [MinimalBase.java](java/src/mapandsum/MinimalBase.java) | 

