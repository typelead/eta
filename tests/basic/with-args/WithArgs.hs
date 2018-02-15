module Main where

import Control.Concurrent
import System.Environment

main :: IO ()
main = do
  m <- newEmptyMVar
  printArgs
  forkIO $ do
    printArgs
    withArgs ["Hello", "Eta"] printArgs
    printArgs
    putMVar m ()
  forkIO $ do
    printArgs
    withArgs ["Hello", "Haskell"] printArgs
    printArgs
    putMVar m ()
  takeMVar m
  takeMVar m
  printArgs

printArgs :: IO ()
printArgs = getArgs >>= print
