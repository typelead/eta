{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module GHCVM.CodeGen.Main where

import Module
import HscTypes
import TyCon
import StgSyn
import DynFlags
import FastString
import VarEnv
import Id
import Name
import OccName
import DataCon
import Util (unzipWith)

import GHCVM.Util
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Monad

import JVM.Builder
import JVM.ClassFile

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
    mapM_ (cgTopBinding dflags) stg_binds
  where
    initEnv = CgEnv { cgPackagePrefix = package,
                      cgFileName = B.append className ".class",
                      cgClassName = fullClassName,
                      cgModule = this_mod
                    }
    initState = CgState { cgBindings = emptyVarEnv }
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

cgTopBinding :: DynFlags -> StgBinding -> CodeGen e ()
cgTopBinding dflags (StgNonRec id rhs) = do
  id' <- externaliseId dflags id
  let (info, code) = cgTopRhs dflags NonRecursive id' rhs
  code
  addBinding info

cgTopBinding dflags (StgRec pairs) = do
  let (binders, rhss) = unzip pairs
  binders' <- mapM (externaliseId dflags) binders
  let pairs' = zip binders' rhss
      r = unzipWith (cgTopRhs dflags Recursive) pairs'
      (infos, codes) = unzip r
  addBindings infos
  sequence_ codes

cgTopRhs :: DynFlags -> RecFlag -> Id -> StgRhs -> (CgIdInfo, CodeGen e ())
cgTopRhs dflags _ binder (StgRhsCon _ con args) =
  cgTopRhsCon dflags binder con args

cgTopRhs dflags recflag binder
   (StgRhsClosure _ binderInfo freeVars updateFlag _ args body) =
  -- fvs should be empty
  cgTopRhsClosure dflags recflag binder binderInfo updateFlag args body


cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen e ())
cgTopRhsCon dflags id con args = (cgIdInfo, genCode)
  where cgIdInfo = mkCgIdInfo id lambdaFormInfo
        lambdaFormInfo = mkConLFInfo con
        genCode = do
          _ <- getModule
          -- initialize and create new object
          return ()

cgTopRhsClosure :: DynFlags
                -> RecFlag              -- member of a recursive group?
                -> Id
                -> StgBinderInfo
                -> UpdateFlag
                -> [Id]                 -- Args
                -> StgExpr
                -> (CgIdInfo, CodeGen e ())
cgTopRhsClosure dflags recflag id binderInfo updateFlag args body
  = (cgIdInfo, genCode dflags lambdaFormInfo)
  where cgIdInfo = mkCgIdInfo id lambdaFormInfo
        lambdaFormInfo = mkClosureLFInfo dflags id TopLevel [] updateFlag args
        genCode dflags _
          | StgApp f [] <- body, null args, isNonRec recflag
          = code . emitClosure id $ IndStatic f
        genCode dflags lf = do
          let name = idName id
          mod <- getModule
          return ()
         -- A new inner class must be generated

-- Names are externalized so that we don't have to thread Modules through the entire program
externaliseId :: DynFlags -> Id -> CodeGen e Id
externaliseId dflags id
  | isInternalName name = do
      mod <- asks cgModule
      return . setIdName id
             $ externalise mod
  | otherwise           = return id
  where
    externalise mod = mkExternalName uniq mod new_occ loc
    name    = idName id
    uniq    = nameUnique name
    new_occ = mkLocalOcc uniq $ nameOccName name
    loc     = nameSrcSpan name
