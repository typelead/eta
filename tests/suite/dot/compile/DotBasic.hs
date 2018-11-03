module DotBasic where

import Data.Maybe
import System.Environment
import System.IO

test1 :: [a] -> Bool
test1 = tail.null

test2 :: IO ()
test2 = getArgs.head.head.putChar

test3 :: Int
test3 = (Just (1 :: Int)).(+ (1 :: Int)).(* (2 :: Int)).fromJust
