{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary
import System.Environment
import qualified Data.Map as M

import JVM.Common
import JVM.ClassFile
import JVM.Converter
import JVM.Dump

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      clsFile <- decodeFile clspath
      putStrLn $ showListIx $ M.assocs $ constsPool (clsFile :: Class File)
      cls <- parseClassFile clspath
      dumpClass cls
    _ -> error "Synopsis: dump-class File.class"

