{-# LANGUAGE PatternSynonyms #-}
module Main where

data World = World String

pattern IsWorld = World "World"

isWorld (World "World") = True
isWorld _ = False

printIsWorld IsWorld = putStrLn "Yes."
printIsWorld _ = putStrLn "No."

printIsWorld' (World "World") = putStrLn "Yes."
printIsWorld' _ = putStrLn "No."

main = do
  printIsWorld (World "World")
  printIsWorld' (World "Bar")
