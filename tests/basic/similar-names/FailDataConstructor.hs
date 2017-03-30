
module Main where

data Hello = HelloWorld
           | Helloworld
           deriving ( Show )

main = do
  print HelloWorld
  print Helloworld

