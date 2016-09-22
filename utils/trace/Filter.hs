module Main where

import Data.Foldable
import Data.List

main = do
  x <- getContents
  let f s = if isInfixOf ">>" s then 1 else (-1)
      g n (s:ss) = let n' = n + f s in (min n' n, s) : g n' ss
      g _ [] = []
  traverse_ (\(n, s) -> putStrLn (replicate n ' ' ++ s)) . g 0 $ lines x
