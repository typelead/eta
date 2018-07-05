
module Main where

helloWorld :: IO ()
helloWorld = print "helloWorld"

data Hello = HelloWorld deriving ( Show )

main = do
  helloWorld
  print HelloWorld

