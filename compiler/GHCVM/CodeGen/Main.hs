module GHCVM.CodeGen.Main where

import Module
import HscTypes
import Type
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
import GHCVM.Primitive
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Rts

import Codec.JVM

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad

import Data.Text (Text)

codeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO [ClassFile]
codeGen hscEnv thisMod dataTyCons stgBinds _hpcInfo =
  runCodeGen initEnv initState $ do
      mapM_ (cgTopBinding dflags) stgBinds
      let cgTyCon tyCon = do
            let dataCons = tyConDataCons tyCon
            unless (null dataCons) $ do
              typeClass <- newTypeClosure (  nameTypeText
                                           . tyConName
                                           $ tyCon) stgConstr
              mapM_ (cgDataCon typeClass) (tyConDataCons tyCon)
      mapM_ cgTyCon dataTyCons
  where
    initEnv = CgEnv { cgQClassName = fullClassName,
                      cgModule = thisMod,
                      cgDynFlags = dflags }
    initState = CgState { cgBindings = emptyVarEnv,
                          cgMethodDefs = [],
                          cgFieldDefs = [],
                          cgClassInitCode = [],
                          cgCompiledClosures = [],
                          cgClassName = fullClassName,
                          cgSuperClassName = Nothing }
    -- TODO: Remove the second part of this pair?
    (fullClassName, _) = generatePackageAndClass thisMod
    dflags = hsc_dflags hscEnv

cgTopBinding :: DynFlags -> StgBinding -> CodeGen ()
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

cgTopRhs :: DynFlags -> RecFlag -> Id -> StgRhs -> (CgIdInfo, CodeGen ())
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
            -> (CgIdInfo, CodeGen ())
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
                -> (CgIdInfo, CodeGen ())
cgTopRhsClosure dflags recflag id binderInfo updateFlag args body
  = (cgIdInfo, genCode dflags lambdaFormInfo)
  where cgIdInfo = mkCgIdInfo id lambdaFormInfo
        lambdaFormInfo = mkClosureLFInfo dflags id TopLevel [] updateFlag args
        genCode dflags _
          | StgApp f [] <- body, null args, isNonRec recflag
          = undefined -- TODO: code . emitClosure id $ IndStatic f
        genCode dflags lf = do
          let name = idName id
          mod <- getModule
          return ()
         -- A new inner class must be generated

-- Names are externalized so that we don't have to thread Modules through the entire program
externaliseId :: DynFlags -> Id -> CodeGen Id
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

cgDataCon :: Text -> DataCon -> CodeGen ()
cgDataCon typeClass dataCon = do
  newExportedClosure (nameText . dataConName $ dataCon) typeClass $ do
    -- TODO: Implement
    return ()
  return ()
  where argReps :: [(JPrimRep, UnaryType)]
        argReps = [(typeJPrimRep repTy, repTy) |
                   ty     <- dataConRepArgTys dataCon,
                   repTy  <- flattenRepType (repType ty)]
