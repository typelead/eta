
module Main where

helloWorld :: IO ()
helloWorld = print "helloWorld"

data HelloWorld = A deriving ( Show )

main = do
  helloWorld
  print A

