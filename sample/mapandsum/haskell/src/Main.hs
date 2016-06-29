
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude     (Monad(..), ($), (.))
import GHC.IO      (IO(..))
import MinimalBase (printInt, Int, enumFromTo, (+), one, zero, thousand)

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f [] = []

caf :: [Int]
caf = map (\x -> x + one) $ enumFromTo one thousand

sum :: [Int] -> Int
sum (x:xs) = x + sum xs
sum _ = zero

main :: IO ()
main = do
  printInt . sum $ caf
  return ()
