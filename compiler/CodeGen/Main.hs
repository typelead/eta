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
import JVM.Exceptions

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Exception.Base

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B

data CgEnv = CgEnv {
  cgPackagePrefix :: String,
  cgClassName :: String
  }

data CgState = CgState

newtype CodeGen e a = CG { unCG :: CgEnv -> CgState -> GenerateIO e (CgState, a)}

instance Functor (CodeGen e) where
  fmap = liftM

instance Applicative (CodeGen e) where
  pure = return
  (<*>) = ap

instance Monad (CodeGen e) where
  return x = CG $ \_ s -> return (s, x)
  m >>= f = CG $ \e s -> do
      (s0, x) <- unCG m e s
      unCG (f x) e s0

runCodeGen :: CgEnv -> CgState -> CodeGen (Caught SomeException NoExceptions) a -> IO [Class Direct]
runCodeGen env state m = generateIO [] $ unCG m env state >> return ()

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x:xs) = toUpper x : xs

replace :: Char -> Char -> String -> String
replace x y = map (\c -> if c == x then y else c)

generatePackageAndClass :: Module -> (String, String)
generatePackageAndClass mod = (package, class_)
  where
    mod_str = moduleNameString (moduleName mod)
    mods = split '.' mod_str
    (before_mod, class__) = (init mods, last mods)
    package_str = zString (zEncodeFS (packageKeyFS (modulePackageKey mod)))
    package = "haskell/" ++ package_str ++ "/" ++ (map toLower . concat . intersperse "/" $ before_mod)
    class_ = upperFirst class__

codeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO [Class Direct]
codeGen hsc_env this_mod data_tycons stg_binds hpc_info =
  runCodeGen initEnv initState $ return ()
  where
    initEnv = CgEnv { cgPackagePrefix = package,
                      cgClassName = package ++ "/" ++ class'}
    initState = CgState
    (package, class') = generatePackageAndClass this_mod
    dflags = hsc_dflags hsc_env

mkModuleInit :: Module -> GenerateIO e ()
mkModuleInit _ = return ()

split   :: Char -> String -> [String]
split c s =  case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'
