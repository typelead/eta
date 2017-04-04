
module Main where

newtype NoIO a = NoIO { noio :: IO a }

main = do
  let
    n = NoIO (print "hello")
  noio n
  
