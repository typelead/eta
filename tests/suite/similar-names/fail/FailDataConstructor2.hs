
module Main where

data Hello = HelloWorld deriving ( Show )
data World = Helloworld deriving ( Show )

main = do
  print HelloWorld
  print Helloworld

