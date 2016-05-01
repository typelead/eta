{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Main where

import Module
import HscTypes
import TyCon
import StgSyn
import DynFlags
import FastString

import JVM.Builder
import JVM.ClassFile

import CodeGen.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Exception.Base

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

runCodeGen :: CgEnv -> CgState -> CodeGen (Caught SomeException NoExceptions) a -> IO [Class Direct]
runCodeGen env state m = generateIO [] $ void (unCG m env state)

generatePackageAndClass :: Module -> (B.ByteString, B.ByteString)
generatePackageAndClass mod = (package, className)
  where
    modString = B.fromStrict
              . fastStringToByteString
              . moduleNameFS
              . moduleName
              $ mod
    mods = split '.' modString
    (parentMods, className') = (init mods, last mods)
    packageString = B.fromStrict
                  . fastZStringToByteString
                  . zEncodeFS
                  . packageKeyFS
                  . modulePackageKey
                  $ mod
    package = B.append "ghcvm/"
            . B.append packageString
            . BC.cons '/'
            . BC.map toLower
            . B.intercalate "/"
            $ parentMods
    className = upperFirst className'
    upperFirst str = case BC.uncons str of
      Nothing -> B.empty
      Just (c, str') -> BC.cons (toUpper c) str'

codeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO [Class Direct]
codeGen hsc_env this_mod data_tycons stg_binds hpc_info =
  runCodeGen initEnv initState $ do
    className <- asks cgClassName
    code $ setClass className
    mapM_ (code . cgTopBinding dflags) stg_binds
  where
    initEnv = CgEnv { cgPackagePrefix = package,
                      cgFileName = B.append className ".class",
                      cgClassName = fullClassName}
    initState = CgState
    (package, className) = generatePackageAndClass this_mod
    dflags = hsc_dflags hsc_env
    fullClassName = B.append package
                  . BC.cons '/'
                  $ className

split   :: Char -> B.ByteString -> [B.ByteString]
split c s =  case BC.dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = BC.break (== c) s'

cgTopBinding :: DynFlags -> StgBinding -> GenerateIO e ()
cgTopBinding dflags stg_bind = return ()
