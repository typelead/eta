module GHCVM.DsForeign where

import TcRnMonad
import TypeRep
import CoreSyn
import CoreUtils
import MkCore
import DsMonad
import HsSyn
import DataCon
import CoreUnfold
import Id
import Var
import MkId
import Literal
import Module
import Name
import Type
import TyCon
import Coercion
import TcEnv
import TcType
import DataCon

import HscTypes
import ForeignCall
import TysWiredIn
import TysPrim
import PrelNames
import BasicTypes
import SrcLoc
import Outputable
import FastString
import DynFlags
import Platform
import Config
import OrdList
import Pair
import Util
import Hooks

import Data.Maybe
import Data.List

type Binding = (Id, CoreExpr)

dsForeigns :: [LForeignDecl Id] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns p = return (NoStubs, nilOL)
dsForeigns fdecls = do
  fives <- mapM doLDecl fdecls
  -- TODO: Finish
  return (undefined, undefined)
  where doLDecl (L loc decl) = putSrcSpanDs loc (doDecl decl)
        doDecl (ForeignImport id _ co spec) = do
          (bs, h, c) <- dsFImport (unLoc id) co spec
          return (h, c, [], bs)
        doDecl fi = pprPanic "doDecl: Not implemented" (ppr fi)

dsFImport :: Id -> Coercion -> ForeignImport -> DsM ([Binding], SDoc, SDoc)
dsFImport id co (CImport cconv safety mHeader spec _) = do
  (ids, h, c) <- dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader
  return (ids, h, c)

dsCImport :: Id -> Coercion -> CImportSpec -> CCallConv -> Safety
  -> Maybe Header -> DsM ([Binding], SDoc, SDoc)
dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id _ _ _ _ _ = pprPanic "doCImport: Not implemented" (ppr id)

dsPrimCall :: Id -> Coercion -> ForeignCall -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsPrimCall funId co fcall = do
  args <- mapM newSysLocalDs argTypes
  ccallUniq <- newUnique
  dflags <- getDynFlags
  let callApp = mkFCall dflags ccallUniq fcall (map Var args) ioResType
      rhs = mkLams tvs (mkLams args callApp)
      rhs' = Cast rhs co
  return ([(funId, rhs')], empty, empty)
  where ty                    = pFst $ coercionKind co
        (tvs, funTy)          = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy

dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header
  -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsFCall funId co fcall mDeclHeader = do
  dflags <- getDynFlags
  liftIO . putStrLn $ showSDoc dflags (ppr ty)
  args <- mapM newSysLocalDs argTypes
  (valArgs, argWrappers) <- mapAndUnzipM unboxArg (map Var args)
  let workArgIds = [v | Var v <- valArgs]
  (ccallResultType, resWrapper) <- boxResult ioResType
  ccallUniq <- newUnique
  workUniq <- newUnique
  -- Build worker
  let workerType = mkForAllTys tvs $
                   mkFunTys (map idType workArgIds) ccallResultType
      ccallApp   = mkFCall dflags ccallUniq fcall valArgs ccallResultType
      workRhs    = mkLams tvs (mkLams workArgIds ccallApp)
      workId     = mkSysLocal (fsLit "$wccall") workUniq workerType

  -- Build wrapper
  let workApp        = mkApps (mkVarApps (Var workId) tvs) valArgs
      wrapperBody    = foldr ($) (resWrapper workApp) argWrappers
      wrapRhs        = mkLams (tvs ++ args) wrapperBody
      wrapRhs'       = Cast wrapRhs co
      funIdWithInline = funId
                       `setIdUnfolding`
                       mkInlineUnfolding (Just (length args)) wrapRhs'
  return ([(workId, workRhs), (funIdWithInline, wrapRhs')], empty, empty)

  where ty = pFst $ coercionKind co
        (tvs, funTy) = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy

unboxArg :: CoreExpr -> DsM (CoreExpr, CoreExpr -> CoreExpr)
unboxArg arg
  | isPrimitiveType argType
  = return (arg, id)
  | Just (co, _) <- topNormaliseNewType_maybe argType
  = unboxArg $ mkCast arg co
  | Just tc <- tyConAppTyCon_maybe argType
  , tc `hasKey` boolTyConKey = do
      dflags <- getDynFlags
      primArg <- newSysLocalDs intPrimTy
      return ( Var primArg
             , \body -> Case (mkWildCase arg argType intPrimTy
                              [ (DataAlt falseDataCon, [], Lit (MachInt 0))
                              , (DataAlt trueDataCon,  [], Lit (MachInt 1)) ])
                              primArg (exprType body)
                              [(DEFAULT,[],body)] )
  | isProductType && dataConArity == 1 = do
      caseBinder <- newSysLocalDs argType
      primArg <- newSysLocalDs dataConArgTy1
      return ( Var primArg,
               \body -> Case arg caseBinder (exprType body)
                             [(DataAlt dataCon, [primArg], body)] )
  -- TODO: Handle ByteArray# and MutableByteArray#
  | otherwise = do
      l <- getSrcSpanDs
      pprPanic "unboxArg: " (ppr l <+> ppr argType)
  where argType                          = exprType arg
        maybeProductType                 = splitDataProductType_maybe argType
        isProductType                    = isJust maybeProductType
        Just (_,_,dataCon,dataConArgTys) = maybeProductType
        dataConArity                     = dataConSourceArity dataCon
        (dataConArgTy1 : _)              = dataConArgTys

mkFCall = undefined
boxResult :: Type -> DsM (Type, CoreExpr -> CoreExpr)
boxResult resultType
  | Just (ioTyCon, ioResType) <- tcSplitIOType_maybe resultType
  = do res <- resultWrapper ioResType
       let extraResultTypes =
             case res of
               (Just ty, _)
                 | isUnboxedTupleType ty
                 -> let Just ls = tyConAppArgs_maybe ty
                    in tail ls
               _ -> []
           returnResult state anss
             = mkCoreConApps (tupleCon UnboxedTuple
                                       (2 + length extraResultTypes))
                             (map Type ( realWorldStatePrimTy
                                       : ioResType
                                       : extraResultTypes ))
       (ccallResultType, alt) <- mkAlt returnResult res
       stateId <- newSysLocalDs realWorldStatePrimTy
       let ioDataCon = head (tyConDataCons ioTyCon)
           toIOCon = dataConWrapId ioDataCon
           wrap call = mkApps (Var toIOCon)
                              [ Type ioResType
                              , Lam stateId $
                                mkWildCase (App call (Var stateId))
                                           ccallResultType
                                           (coreAltType alt)
                                           [alt] ]
       return (realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)

boxResult resultType = do
  res <- resultWrapper resultType
  (ccallResultType, alt) <- mkAlt returnResult res
  let wrap call = mkWildCase (App call (Var realWorldPrimId))
                             ccallResultType
                             (coreAltType alt)
                             [alt]
  return (realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)
  where returnResult _ [ans] = ans
        returnResult _ _ = panic "returnResult: expected single result"

mkAlt :: (Expr Var -> [Expr Var] -> Expr Var)
      -> (Maybe Type, Expr Var -> Expr Var)
      -> DsM (Type, (AltCon, [Id], Expr Var))
mkAlt returnResult (Nothing, wrapResult) = do
  stateId <- newSysLocalDs realWorldStatePrimTy
  let rhs = returnResult (Var stateId) [wrapResult (panic "boxResult")]
      ccallResultType = mkTyConApp unboxedSingletonTyCon [realWorldStatePrimTy]
      alt = (DataAlt unboxedSingletonDataCon, [stateId], rhs)
  return (ccallResultType, alt)
mkAlt returnResult (Just primResType, wrapResult)
  | isUnboxedTupleType primResType = do
      let Just ls = tyConAppArgs_maybe primResType
          arity = 1 + length ls
      argIds@(resultId:as) <- mapM newSysLocalDs ls
      stateId <- newSysLocalDs realWorldStatePrimTy
      let rhs = returnResult (Var stateId) (wrapResult (Var resultId) : map Var as)
          ccallResultType = mkTyConApp (tupleTyCon UnboxedTuple arity)
                                       (realWorldStatePrimTy : ls)
          alt = ( DataAlt (tupleCon UnboxedTuple arity)
                , stateId : argIds
                , rhs )
      return (ccallResultType, alt)
  | otherwise = do
      resultId <- newSysLocalDs primResType
      stateId <- newSysLocalDs realWorldStatePrimTy
      let rhs = returnResult (Var stateId) [wrapResult (Var resultId)]
          ccallResultType = mkTyConApp unboxedPairTyCon
                                       [realWorldStatePrimTy, primResType]
          alt = ( DataAlt unboxedPairDataCon
                , [stateId, resultId]
                , rhs )
      return (ccallResultType, alt)

resultWrapper :: Type -> DsM (Maybe Type, CoreExpr -> CoreExpr)
resultWrapper resultType
  | isPrimitiveType resultType
  = return (Just resultType, id)
  | Just (tc, _) <- maybeTcApp, tc `hasKey` boolTyConKey
  = return ( Just intPrimTy
           , \e -> mkWildCase e intPrimTy boolTy
                   [ (DEFAULT, [], Var trueDataConId)
                   , (LitAlt (MachInt 0), [], Var falseDataConId) ] )
  | Just (co, repType) <- topNormaliseNewType_maybe resultType
  = do (maybeType, wrapper) <- resultWrapper repType
       return (maybeType, \e -> mkCast (wrapper e) (mkSymCo co))
  | Just (tyVar, rest) <- splitForAllTy_maybe resultType
  = do (maybeType, wrapper) <- resultWrapper rest
       return (maybeType, \e -> Lam tyVar (wrapper e))
  | Just (tyCon, tyConArgTys, dataCon, dataConArgTys)
    <- splitDataProductType_maybe resultType,
    dataConSourceArity dataCon == 1 = do
      let (unwrapperResType : _) = dataConArgTys
          narrowWrapper = id --TODO: maybeNarrow tyCon
      (maybeType, wrapper) <- resultWrapper unwrapperResType
      return ( maybeType
             , \e ->
                 mkApps (Var (dataConWrapId dataCon))
                        (map Type tyConArgTys ++ [wrapper (narrowWrapper e)]) )
  | otherwise
  = pprPanic "resultWrapper" (ppr resultType)
  where maybeTcApp = splitTyConApp_maybe resultType
