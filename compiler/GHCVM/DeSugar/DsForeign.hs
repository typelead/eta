{-# LANGUAGE OverloadedStrings #-}
module GHCVM.DeSugar.DsForeign where

import GHCVM.Core.CoreSyn
import GHCVM.Core.CoreUtils
import GHCVM.Core.MkCore
import GHCVM.DeSugar.DsMonad
import GHCVM.HsSyn.HsSyn
import GHCVM.Core.CoreUnfold
import GHCVM.BasicTypes.VarEnv
import GHCVM.BasicTypes.VarSet
import GHCVM.BasicTypes.Id
import GHCVM.BasicTypes.Var
import GHCVM.BasicTypes.MkId
import GHCVM.BasicTypes.Literal
import GHCVM.BasicTypes.Module
import GHCVM.BasicTypes.Name
import GHCVM.BasicTypes.DataCon
import GHCVM.Types.Type
import GHCVM.Types.TyCon
import GHCVM.Types.TypeRep
import GHCVM.Types.Coercion
import GHCVM.TypeCheck.TcRnMonad
import GHCVM.TypeCheck.TcEnv
import GHCVM.TypeCheck.TcType

import GHCVM.Main.HscTypes
import GHCVM.Prelude.ForeignCall
import GHCVM.Prelude.TysWiredIn
import GHCVM.Prelude.TysPrim
import GHCVM.Prelude.PrelNames
import GHCVM.Prelude.PrelInfo ( primOpId )
import GHCVM.Prelude.PrimOp
import GHCVM.BasicTypes.BasicTypes
import GHCVM.BasicTypes.SrcLoc
import GHCVM.Utils.Outputable hiding ((<>))
import GHCVM.Utils.FastString
import GHCVM.Main.DynFlags
import GHCVM.Utils.Platform
import GHCVM.Utils.MonadUtils
import GHCVM.Utils.OrdList
import GHCVM.Utils.Pair
import GHCVM.Utils.Util
import GHCVM.Main.Hooks
import GHCVM.CodeGen.ArgRep (repFieldType_maybe)
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Name

import Data.Maybe
import Data.Monoid((<>))
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import GHCVM.Debug
import Codec.JVM

data Bound = SuperBound | ExtendsBound
  deriving (Eq, Show)
type Binding = (Id, CoreExpr)
type ExtendsInfo = VarEnv (Id, Type, Bound)

invertBound :: Bound -> Bound
invertBound SuperBound = ExtendsBound
invertBound ExtendsBound = SuperBound

dsForeigns :: [LForeignDecl Id] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns [] = return (NoStubs, nilOL)
dsForeigns fdecls = do
  ieDecls <- mapM doLDecl fdecls
  let (methods', bindss) = unzip ieDecls
      methods = catMaybes methods'
  return (appendDefs NoStubs methods, foldr (appOL . toOL) nilOL bindss)
  where doLDecl (L loc decl) = putSrcSpanDs loc (doDecl decl)
        doDecl (ForeignImport id _ co spec) = do
          bs <- dsFImport (unLoc id) co spec
          return (Nothing, bs)
        doDecl (ForeignExport (L _ id) _ co
                              (CExport (L _ (CExportStatic extName cconv)) _)) = do
            method <- dsFExport id co extName cconv False
            return (Just method, [])
        doDecl fi = pprPanic "doDecl: Not implemented" (ppr fi)

dsFImport :: Id -> Coercion -> ForeignImport -> DsM [Binding]
dsFImport id co (CImport cconv safety mHeader spec _) =
  dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

dsCImport :: Id -> Coercion -> CImportSpec -> CCallConv -> Safety
  -> Maybe Header -> DsM [Binding]
dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id _ _ _ _ _ = pprPanic "doCImport: Not implemented" (ppr id)

dsPrimCall :: Id -> Coercion -> ForeignCall -> DsM [(Id, Expr TyVar)]
dsPrimCall funId co fcall = do
  args <- mapM newSysLocalDs argTypes
  ccallUniq <- newUnique
  dflags <- getDynFlags
  let callApp = mkFCall dflags ccallUniq fcall (map Var args) ioResType
      rhs = mkLams tvs (mkLams args callApp)
      rhs' = Cast rhs co
  return [(funId, rhs')]
  where ty                    = pFst $ coercionKind co
        (tvs, funTy)          = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy

dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header -> DsM [(Id, Expr TyVar)]
dsFCall funId co fcall mDeclHeader = do
  dflags <- getDynFlags
  (thetaArgs, extendsInfo) <- extendsMap thetaType
  args <- mapM newSysLocalDs argTypes
  (valArgs, argWrappers) <- mapAndUnzipM (unboxArg extendsInfo) (map Var args)
  let workArgIds = [v | Var v <- valArgs]
  (ccallResultType, resWrapper) <- boxResult extendsInfo ioResType
  ccallUniq <- newUnique
  workUniq <- newUnique
  -- Build worker
  let workerType = mkForAllTys tvs $
                   mkFunTys (map idType workArgIds) ccallResultType
      ccallApp   = mkFCall dflags ccallUniq (fcall' extendsInfo) valArgs ccallResultType
      workRhs    = mkLams tvs (mkLams workArgIds ccallApp)
      workId     = mkSysLocal (fsLit "$wccall") workUniq workerType

  -- Build wrapper
  let workApp        = mkApps (mkVarApps (Var workId) tvs) valArgs
      wrapperBody    = foldr ($) (resWrapper workApp) argWrappers
      wrapRhs        = mkLams (tvs ++ thetaArgs ++ args) wrapperBody
      wrapRhs'       = Cast wrapRhs co
      funIdWithInline = funId
                       `setIdUnfolding`
                       mkInlineUnfolding (Just (length args)) wrapRhs'
  return [(workId, workRhs), (funIdWithInline, wrapRhs')]

  where ty = pFst $ coercionKind co
        (tvs, thetaFunTy) = tcSplitForAllTys ty
        (thetaType, funTy) = tcSplitPhiTy thetaFunTy
        (argTypes, ioResType) = tcSplitFunTys funTy
        fcall' extendsInfo
          | Just (_, javaTagType, _) <- tcSplitJavaType_maybe ioResType
          , CCall (CCallSpec (StaticTarget label mPkgKey isFun) JavaCallConv safety) <- fcall
          = let morphTarget f = CCall (CCallSpec (StaticTarget
                                                  (mkFastString ("@java " ++
                                                                 (f $ unpackFS label)))
                                                  mPkgKey isFun) JavaCallConv safety)
            in case getTyVar_maybe javaTagType of
              Just var ->
                case lookupVarEnv extendsInfo var of
                 Just (ident, ty, _) ->
                   morphTarget id
                   -- (\label ->
                   --    let parts = words label
                   --        start = init parts
                   --        change = last parts
                   --    in unwords $ start ++ [T.unpack (tagTypeToText ty) ++ "." ++ change])
                 Nothing -> morphTarget ("@static " ++)
              _ -> morphTarget id
          | otherwise = fcall

extendsMap :: ThetaType -> DsM ([Id], ExtendsInfo)
extendsMap thetaType = do
  (ids, keyVals) <- flip mapAndUnzipM thetaType $ \thetaTy -> do
    dictId <- newSysLocalDs thetaTy
    let (var', tagTy') = tcSplitExtendsType thetaTy
        (var, tagTy, bound)
          | isTyVarTy var' = (var', tagTy', ExtendsBound)
          | otherwise = (tagTy', var', SuperBound)
    return (dictId, ( getTyVar "extendsMap: Not type variable!" var
                    , (dictId, tagTy, bound)))
  return (ids, mkVarEnv keyVals)

unboxArg :: ExtendsInfo -> CoreExpr -> DsM (CoreExpr, CoreExpr -> CoreExpr)
unboxArg vs arg
  | isPrimitiveType argType
  = return (arg, id)
  | Just (co, _) <- topNormaliseNewType_maybe argType
  = unboxArg vs $ mkCast arg co
  | Just tc <- tyConAppTyCon_maybe argType
  , tc `hasKey` boolTyConKey = do
      dflags <- getDynFlags
      primArg <- newSysLocalDs jboolPrimTy
      return ( Var primArg
             , \body ->
                 App (Var (primOpId Int2JBoolOp))
                     (Case (mkWildCase arg argType intPrimTy
                            [ (DataAlt falseDataCon, [], Lit (MachInt 0))
                            , (DataAlt trueDataCon,  [], Lit (MachInt 1)) ])
                           primArg (exprType body)
                           [(DEFAULT,[],body)] ))
  | isProductType && dataConArity == 1 = do
      caseBinder <- newSysLocalDs argType
      primArg <- newSysLocalDs dataConArgTy1
      return ( Var primArg,
               \body -> Case arg caseBinder (exprType body)
                             [(DataAlt dataCon, [primArg], body)] )
  | Just v <- getTyVar_maybe argType
  , Just (dictId, tagType, bound) <- lookupVarEnv vs v = do
      castId <- getClassCastId bound
      let typeArgs = if bound == ExtendsBound
            then [argType, tagType]
            else [tagType, argType]
      unboxArg vs $ mkApps (mkTyApps (Var castId) typeArgs) [Var dictId, arg]
  | otherwise = do
      l <- getSrcSpanDs
      pprPanic "unboxArg: " (ppr l <+> ppr argType)
  where argType                          = exprType arg
        maybeProductType                 = splitDataProductType_maybe argType
        isProductType                    = isJust maybeProductType
        Just (_,_,dataCon,dataConArgTys) = maybeProductType
        dataConArity                     = dataConSourceArity dataCon
        (dataConArgTy1 : _)              = dataConArgTys

getClassCastId :: Bound -> DsM Id
getClassCastId bound
  | bound == ExtendsBound = dsLookupGlobalId supercastName
  | otherwise = dsLookupGlobalId classcastName

mkFCall :: DynFlags -> Unique -> ForeignCall -> [CoreExpr] -> Type -> CoreExpr
mkFCall dflags unique fcall valArgs resType
  = mkApps (mkVarApps (Var fcallId) tyVars) valArgs
  where argTypes = map exprType valArgs
        bodyType = mkFunTys argTypes resType
        tyVars = varSetElems $ tyVarsOfType bodyType
        ty = mkForAllTys tyVars bodyType
        fcallId = mkFCallId dflags unique fcall ty

boxResult :: ExtendsInfo -> Type -> DsM (Type, CoreExpr -> CoreExpr)
boxResult extendsInfo resultType
  | Just (ioTyCon, ioResType) <- tcSplitIOType_maybe resultType
  = do res <- resultWrapper extendsInfo ioResType
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
  = do dflags <- getDynFlags
       res <- resultWrapper extendsInfo javaResType
       let extraResultTypes =
             case res of
               (Just ty, _)
                 | isUnboxedTupleType ty
                 -> let Just ls = tyConAppArgs_maybe ty
                    in tail ls
               _ -> []
           objectType = mkObjectPrimTy javaTagType
           returnResult object anss
             = mkCoreConApps (tupleCon UnboxedTuple
                                       (2 + length extraResultTypes))
                             (map Type ( objectType
                                       : javaResType
                                       : extraResultTypes )
                              ++ (object : anss))
       (javaResultType, alt) <- mkAltJava objectType returnResult res
       thisId <- newSysLocalDs objectType
       let javaDataCon = head (tyConDataCons javaTyCon)
           toJavaCon = dataConWrapId javaDataCon
           wrap call = mkApps (Var toJavaCon)
                              [ Type javaTagType
                              , Type javaResType
                              , Lam thisId $
                                mkWildCase (App call (Var thisId))
                                           javaResultType
                                           (coreAltType alt)
                                           [alt] ]
       return ( objectType `mkFunTy` javaResultType
              , wrap )
  | otherwise
  = do res <- resultWrapper extendsInfo resultType
       (ccallResultType, alt) <- mkAlt returnResult res
       let wrap call = mkWildCase (App call (Var realWorldPrimId))
                                  ccallResultType
                                  (coreAltType alt)
                                  [alt]
       return (realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)
       where returnResult _ [ans] = ans
             returnResult _ _ = panic "returnResult: expected single result"

mkAltJava :: Type -> (Expr Var -> [Expr Var] -> Expr Var)
      -> (Maybe Type, Expr Var -> Expr Var)
      -> DsM (Type, (AltCon, [Id], Expr Var))
mkAltJava objectType returnResult (Nothing, wrapResult) = do
  objectId <- newSysLocalDs objectType
  let rhs = returnResult (Var objectId) [wrapResult (panic "boxResult")]
      javaResultType = mkTyConApp unboxedSingletonTyCon [ objectType ]
      alt = (DataAlt unboxedSingletonDataCon, [objectId], rhs)
  return (javaResultType, alt)
mkAltJava objectType returnResult (Just primResType, wrapResult)
  | isUnboxedTupleType primResType = do
      let Just ls = tyConAppArgs_maybe primResType
          arity = 1 + length ls
      argIds@(resultId:as) <- mapM newSysLocalDs ls
      objectId <- newSysLocalDs objectType
      let rhs = returnResult (Var objectId)
                             (wrapResult (Var resultId) : map Var as)
          javaResultType = mkTyConApp (tupleTyCon UnboxedTuple arity)
                                      (objectType : ls)
          alt = ( DataAlt (tupleCon UnboxedTuple arity)
                , objectId : argIds
                , rhs )
      return (javaResultType, alt)
  | otherwise = do
      resultId <- newSysLocalDs primResType
      objectId <- newSysLocalDs objectType
      let rhs = returnResult (Var objectId) [wrapResult (Var resultId)]
          alt = ( DataAlt unboxedPairDataCon
                , [objectId, resultId]
                , rhs )
          javaResultType = mkTyConApp unboxedPairTyCon [ objectType , primResType ]
      return (javaResultType, alt)

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

resultWrapper :: ExtendsInfo -> Type -> DsM (Maybe Type, CoreExpr -> CoreExpr)
resultWrapper extendsInfo resultType
  | isPrimitiveType resultType
  = return (Just resultType, id)
  | Just (tc, _) <- maybeTcApp, tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)
  | Just (tc, _) <- maybeTcApp, tc `hasKey` boolTyConKey
  = return ( Just jboolPrimTy
           , \e -> mkWildCase e jboolPrimTy boolTy
                   [ (DEFAULT, [], Var trueDataConId)
                   , (LitAlt (MachInt 0), [], Var falseDataConId) ] )
  | Just (co, repType) <- topNormaliseNewType_maybe resultType
  = do (maybeType, wrapper) <- resultWrapper extendsInfo repType
       return (maybeType, \e -> mkCast (wrapper e) (mkSymCo co))
  | Just (tyVar, rest) <- splitForAllTy_maybe resultType
  = do (maybeType, wrapper) <- resultWrapper extendsInfo rest
       return (maybeType, Lam tyVar . wrapper)
  | Just (tyCon, tyConArgTys, dataCon, dataConArgTys)
    <- splitDataProductType_maybe resultType,
    dataConSourceArity dataCon == 1
  = do let (unwrapperResType : _) = dataConArgTys
           narrowWrapper = maybeNarrow tyCon
       (maybeType, wrapper) <- resultWrapper extendsInfo unwrapperResType
       return ( maybeType
              , \e ->
                  mkApps (Var (dataConWrapId dataCon))
                         (map Type tyConArgTys ++ [wrapper (narrowWrapper e)]))
  | Just var <- getTyVar_maybe resultType
  , Just (dictId, tagType, bound) <- lookupVarEnv extendsInfo var
  = do (objType, wrapper) <- resultWrapper extendsInfo tagType
       castId <- getClassCastId (invertBound bound)
       let typeArgs = map Type $
             if bound == ExtendsBound
             then [resultType, tagType]
             else [tagType, resultType]
       return ( objType
              , \e ->
                  mkApps (Var castId) (typeArgs ++ [Var dictId, wrapper e]))
  | otherwise
  = pprPanic "resultWrapper" (ppr resultType)
  where maybeTcApp = splitTyConApp_maybe resultType

maybeNarrow :: TyCon -> (CoreExpr -> CoreExpr)
maybeNarrow tycon
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkPrimOpId Narrow16WordOp)) e
  | otherwise                     = id

dsFExport :: Id                 -- The exported Id
          -> Coercion           -- Coercion between the Haskell type callable
                                -- from C, and its representation type
          -> CLabelString       -- The name to export to C land
          -> CCallConv
          -> Bool               -- True => foreign export dynamic
                                --         so invoke IO action that's hanging off
                                --         the first argument's stable pointer
          -> DsM (Text, MethodDef)

dsFExport fnId co externalName cconv isDyn = do
  mod <- fmap ds_mod getGblEnv
  dflags <- getDynFlags
  let resClass = typeDataConClass dflags resType
      boxedArgs =
        if length argFts > 5
        then error $ "Foreign exports with number of arguments > 5 are currently not "
                  ++ "supported."
        else foldl' (\code (i, argPrimFt, argType) ->
                        let argClass = typeDataConClass dflags argType
                            argClassFt = obj argClass
                        in code
                        <> (if argPrimFt == jbool
                            then gload argPrimFt i <> ifeq falseClosure trueClosure
                            else    new argClassFt
                                 <> dup argClassFt
                                 <> gload argPrimFt i
                                 <> invokespecial (mkMethodRef argClass "<init>" [argPrimFt] void)))
                    mempty
                    (zip3 [1..] argFts argTypes)
      numApplied = length argTypes + 1
      apClass = apUpdName numApplied
      apFt = obj apClass
  return ( rawClassSpec
         , mkMethodDef className [Public] methodName argFts resFt $
             invokestatic (mkMethodRef rtsGroup "lock" [] (ret capabilityType))
          <> gload classFt 0
          -- TODO: Implement runJava :: Java a -> Java a that catches exceptions as well
          <> new apFt
          <> dup apFt
          <> getstatic (mkFieldRef (moduleJavaClass mod) (closure (idNameText dflags fnId))
                                   closureType)
          <> boxedArgs
          <> invokespecial (mkMethodRef apClass "<init>" (replicate numApplied closureType) void)
          -- TODO: Support java args > 5
          <> invokestatic (mkMethodRef rtsGroup "evalJava"
                                       [capabilityType, jobject, closureType]
                                       (ret hsResultType))
          <> (if voidResult
              then dup hsResultType
              else mempty)
          <> hsResultCap
          <> invokestatic (mkMethodRef rtsGroup "unlock" [capabilityType] void)
          -- TODO: add a call to checkSchedStatus
          <> (if voidResult
             then vreturn
             else (hsResultValue <> unboxResult resType resClass rawResFt)))
  where ty = pSnd $ coercionKind co
        (tvs, thetaFunTy) = tcSplitForAllTys ty
        (thetaType, funTy) = tcSplitPhiTy thetaFunTy
        (argTypes, ioResType) = tcSplitFunTys funTy
        classFt = obj className
        argFts = map getPrimFt argTypes
        rawResFt = fromJust resFt
        resFt = if voidResult
                then void
                else repFieldType_maybe $ getPrimTyOf resType
        voidResult
          | UnaryRep repResTy <- repType resType
          , isUnitTy repResTy
          = True
          | otherwise = False
        methodName = fastStringText externalName
        (rawClassSpec, className, resType) =
          case tcSplitJavaType_maybe ioResType of
            Just (javaTyCon, javaTagType, javaResType) ->
              ((either (error $ "The tag type should be annotated with a CLASS annotation.")
               (maybe (error $ "No type variables for the Java foreign export!") id)
               $ rawTagTypeToText javaTagType)
              , tagTypeToText javaTagType
              , javaResType)
            _ -> error $ "Result type of 'foreign export java' declaration must be in the "
                      ++ "Java monad."

typeDataConClass :: DynFlags -> Type -> Text
typeDataConClass dflags = dataConClass dflags . head . tyConDataCons . tyConAppTyCon

dataConWrapper :: Type -> Code
dataConWrapper = undefined

unboxResult :: Type -> Text -> FieldType -> Code
unboxResult ty resClass resPrimFt
  | isBoolTy ty = getTagMethod mempty
               <> iconst jbool (fromIntegral 1)
               <> isub
               <> greturn resPrimFt
  | otherwise = gconv closureType resClassFt
             <> getfield (mkFieldRef resClass (constrField 1) resPrimFt)
             <> greturn resPrimFt
  where resClassFt = obj resClass

getPrimFt :: Type -> FieldType
getPrimFt = fromJust . repFieldType_maybe . getPrimTyOf

getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | UnaryRep repTy <- repType ty =
      if isBoolTy repTy
      then jboolPrimTy
      else case splitDataProductType_maybe repTy of
        Just (_, _, _, [primTy]) -> primTy
        _ -> pprPanic "DsForeign.getPrimTyOf" $ ppr ty
