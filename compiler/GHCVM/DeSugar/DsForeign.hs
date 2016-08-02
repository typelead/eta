module GHCVM.DeSugar.DsForeign where

import GHCVM.BasicTypes.VarSet
import GHCVM.TypeCheck.TcRnMonad
import GHCVM.Types.TypeRep
import GHCVM.Core.CoreSyn
import GHCVM.Core.CoreUtils
import GHCVM.Core.MkCore
import GHCVM.DeSugar.DsMonad
import GHCVM.HsSyn.HsSyn
import GHCVM.Core.CoreUnfold
import GHCVM.BasicTypes.Id
import GHCVM.BasicTypes.Var
import GHCVM.BasicTypes.MkId
import GHCVM.BasicTypes.Literal
import GHCVM.BasicTypes.Module
import GHCVM.BasicTypes.Name
import GHCVM.Types.Type
import GHCVM.Types.TyCon
import GHCVM.Types.Coercion
import GHCVM.TypeCheck.TcEnv
import GHCVM.TypeCheck.TcType (tcSplitForAllTys, tcSplitFunTys, tcSplitTyConApp_maybe)
import GHCVM.BasicTypes.DataCon

import GHCVM.Main.HscTypes
import GHCVM.Prelude.ForeignCall
import GHCVM.Prelude.TysWiredIn
import GHCVM.Prelude.TysPrim
import GHCVM.Prelude.PrelNames
import GHCVM.BasicTypes.BasicTypes
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.Outputable
import GHCVM.Utils.FastString
import GHCVM.Main.DynFlags
import GHCVM.Utils.Platform
import GHCVM.Utils.OrdList
import GHCVM.Utils.Pair
import GHCVM.Utils.Util
import GHCVM.Main.Hooks

import Data.Maybe
import Data.List

import GHCVM.Primitive

type Binding = (Id, CoreExpr)

dsForeigns :: [LForeignDecl Id] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns [] = return (NoStubs, nilOL)
dsForeigns fdecls = do
  fives <- mapM doLDecl fdecls
  let (hs, cs, idss, bindss) = unzip4 fives
      feIds = concat idss
      -- TODO: Foreign Exports: feInitCode =
  return (ForeignStubs empty empty, foldr (appOL . toOL) nilOL bindss)
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
  liftIO . putStrLn $ showSDoc dflags $ ptext (sLit "dsFCall:") <+> ppr ty <+> ppr tvs <+> ppr argTypes <+> ppr ioResType
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

mkFCall :: DynFlags -> Unique -> ForeignCall -> [CoreExpr] -> Type -> CoreExpr
mkFCall dflags unique fcall valArgs resType
  = mkApps (mkVarApps (Var fcallId) tyVars) valArgs
  where argTypes = map exprType valArgs
        bodyType = mkFunTys argTypes resType
        tyVars = varSetElems $ tyVarsOfType bodyType
        ty = mkForAllTys tyVars bodyType
        fcallId = mkFCallId dflags unique fcall ty

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
                                       : extraResultTypes )
                              ++ (state : anss))
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
  | Just (javaTyCon, javaTagType, javaResType) <- tcSplitJavaType_maybe resultType
  = do res <- resultWrapper javaResType
       let extraResultTypes =
             case res of
               (Just ty, _)
                 | isUnboxedTupleType ty
                 -> let Just ls = tyConAppArgs_maybe ty
                    in tail ls
               _ -> []
           objectType = mkJObjectTy javaTagType
           returnResult state object anss
             = mkCoreConApps (tupleCon UnboxedTuple
                                       (3 + length extraResultTypes))
                             (map Type ( realWorldStatePrimTy
                                       : objectType
                                       : javaResType
                                       : extraResultTypes )
                              ++ (state : object : anss))
       (ccallResultType, alt) <- mkAltJava objectType returnResult res
       stateId <- newSysLocalDs realWorldStatePrimTy
       thisId <- newSysLocalDs objectType
       let javaDataCon = head (tyConDataCons javaTyCon)
           toJavaCon = dataConWrapId javaDataCon
           wrap call = mkApps (Var toJavaCon)
                              [ Type javaTagType
                              , Type javaResType
                              , Lam stateId . Lam thisId $
                                mkWildCase (App (App call (Var stateId))
                                                (Var thisId))
                                           ccallResultType
                                           (coreAltType alt)
                                           [alt] ]
       return ( mkFunTys [realWorldStatePrimTy, objectType] ccallResultType
              , wrap )
  | otherwise
  = do res <- resultWrapper resultType
       (ccallResultType, alt) <- mkAlt returnResult res
       let wrap call = mkWildCase (App call (Var realWorldPrimId))
                                  ccallResultType
                                  (coreAltType alt)
                                  [alt]
       return (realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)
       where returnResult _ [ans] = ans
             returnResult _ _ = panic "returnResult: expected single result"

mkAltJava :: Type -> (Expr Var -> Expr Var -> [Expr Var] -> Expr Var)
      -> (Maybe Type, Expr Var -> Expr Var)
      -> DsM (Type, (AltCon, [Id], Expr Var))
mkAltJava objectType returnResult (Nothing, wrapResult) = do
  stateId <- newSysLocalDs realWorldStatePrimTy
  objectId <- newSysLocalDs objectType
  let rhs = returnResult (Var stateId) (Var objectId)
                         [wrapResult (panic "boxResult")]
      ccallResultType = mkTyConApp unboxedPairTyCon [ realWorldStatePrimTy
                                                    , objectType ]
      alt = (DataAlt unboxedPairDataCon, [stateId, objectId], rhs)
  return (ccallResultType, alt)
mkAltJava objectType returnResult (Just primResType, wrapResult)
  | isUnboxedTupleType primResType = do
      let Just ls = tyConAppArgs_maybe primResType
          arity = 2 + length ls
      argIds@(resultId:as) <- mapM newSysLocalDs ls
      stateId <- newSysLocalDs realWorldStatePrimTy
      objectId <- newSysLocalDs objectType
      let rhs = returnResult (Var stateId)
                             (Var objectId)
                             (wrapResult (Var resultId) : map Var as)
          ccallResultType = mkTyConApp (tupleTyCon UnboxedTuple arity)
                                       (realWorldStatePrimTy : objectType : ls)
          alt = ( DataAlt (tupleCon UnboxedTuple arity)
                , stateId : objectId : argIds
                , rhs )
      return (ccallResultType, alt)
  | otherwise = do
      resultId <- newSysLocalDs primResType
      stateId <- newSysLocalDs realWorldStatePrimTy
      objectId <- newSysLocalDs objectType
      let rhs = returnResult (Var stateId) (Var objectId)
                             [wrapResult (Var resultId)]
          ccallResultType = mkTyConApp (tupleTyCon UnboxedTuple 3)
                                       [ realWorldStatePrimTy
                                       , objectType
                                       , primResType ]
          alt = ( DataAlt (tupleCon UnboxedTuple 3)
                , [stateId, objectId, resultId]
                , rhs )
      return (ccallResultType, alt)

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
  | Just (tc, _) <- maybeTcApp, tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)
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
       return (maybeType, Lam tyVar . wrapper)
  | Just (tyCon, tyConArgTys, dataCon, dataConArgTys)
    <- splitDataProductType_maybe resultType,
    dataConSourceArity dataCon == 1
  = do let (unwrapperResType : _) = dataConArgTys
           narrowWrapper = id --TODO: maybeNarrow tyCon
       (maybeType, wrapper) <- resultWrapper unwrapperResType
       return ( maybeType
              , \e ->
                  mkApps (Var (dataConWrapId dataCon))
                         (map Type tyConArgTys ++ [wrapper (narrowWrapper e)]))
  | otherwise
  = pprPanic "resultWrapper" (ppr resultType)
  where maybeTcApp = splitTyConApp_maybe resultType
