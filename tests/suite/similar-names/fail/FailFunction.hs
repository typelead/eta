
module Main where

helloWorld :: IO ()
helloWorld = print "helloWorld"

helloworld :: IO ()
helloworld = print "helloworld"

main = do
  helloWorld
  helloworld

