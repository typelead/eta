
module Main where

data Jojo = Jojo
  { fooBar :: String
  , foobar :: String
  }

main = do
  let
    j = Jojo "hello" "world"
  print $ fooBar j
  print $ foobar j
  
