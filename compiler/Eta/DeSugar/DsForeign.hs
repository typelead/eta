{-
(c) Rahul Muttineni 2016-2017
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


Desugaring foreign declarations.
-}

{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Eta.DeSugar.DsForeign where

import Eta.Core.CoreSyn
import Eta.Core.CoreUtils
import Eta.Core.MkCore
import Eta.DeSugar.DsMonad
import Eta.HsSyn.HsSyn
import Eta.Core.CoreUnfold
import Eta.BasicTypes.Module
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.Id

import Eta.BasicTypes.MkId
import Eta.BasicTypes.Literal


import Eta.BasicTypes.DataCon
import Eta.Types.Type
import Eta.Types.TyCon
import Eta.Types.FamInstEnv

import Eta.Types.Coercion
import Eta.TypeCheck.TcRnMonad

import Eta.TypeCheck.TcType

import Eta.Main.HscTypes
import Eta.Prelude.ForeignCall
import Eta.Prelude.TysWiredIn
import Eta.Prelude.TysPrim
import Eta.Prelude.PrelNames
import Eta.Prelude.PrelInfo ( primOpId )
import Eta.Prelude.PrimOp
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.Name
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Outputable hiding ((<>))
import Eta.Utils.FastString
import Eta.Main.DynFlags

import Eta.Utils.OrdList
import Eta.Utils.Pair
import Eta.Utils.Util

import Eta.CodeGen.ArgRep ( repFieldType_maybe, primRepFieldType,
                            srepFieldTypes, primRepFieldType_maybe )
import Eta.CodeGen.Rts
import Eta.CodeGen.Name

import Data.Maybe
import Data.Char(toUpper)
import Data.Monoid((<>))
import Data.List
import Data.Text (Text)
import qualified Data.Text as T


import Codec.JVM
import Codec.JVM.Attr hiding (ExtendsBound, SuperBound)
import qualified Codec.JVM.Attr as A

type Binding = (Id, CoreExpr)
type ClassExport = (Text, MethodDef, Maybe FieldDef)
type ExtendsInfo = VarEnv (Id, Type, BoundTag)

data BoundTag = SuperBound | ExtendsBound
  deriving (Eq, Show)

instance Outputable BoundTag where
  ppr x = text (show x)


invertBound :: BoundTag -> BoundTag
invertBound SuperBound = ExtendsBound
invertBound ExtendsBound = SuperBound

dsForeigns :: [LForeignDecl Id] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns [] = return (NoStubs, nilOL)
dsForeigns fdecls = do
  ieDecls <- mapM doLDecl fdecls
  let (methods', bindss) = unzip ieDecls
      methods = concat methods'
  return (appendDefs NoStubs methods, foldr (appOL . toOL) nilOL bindss)
  where doLDecl (L loc decl) = putSrcSpanDs loc (doDecl decl)
        doDecl (ForeignImport id _ co spec) = do
          (bs, methodDefs) <- dsFImport (unLoc id) co spec
          return (methodDefs, bs)
        doDecl (ForeignExport (L _ id) _ co
                              (CExport (L _ (CExportStatic extName _)) _)) = do
            mod    <- getModule
            envs   <- dsGetFamInstEnvs
            inheritsFamTyCon <- dsLookupTyCon inheritsFamTyConName
            method <- dsFExport (Right id) inheritsFamTyCon envs co extName Nothing mod
            return ([method], [])

dsFImport :: Id -> Coercion -> ForeignImport -> DsM ([Binding], [ClassExport])
dsFImport id co (CImport cconv safety mHeader spec _) =
  dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

dsCImport :: Id -> Coercion -> CImportSpec -> CCallConv -> Safety
  -> Maybe Header -> DsM ([Binding], [ClassExport])
dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id co (CWrapper target isAbstract) _ _ _
  = dsFWrapper id co target isAbstract
dsCImport id _ _ _ _ _ = pprPanic "doCImport: Not implemented" (ppr id)

dsPrimCall :: Id -> Coercion -> ForeignCall -> DsM ([Binding], [ClassExport])
dsPrimCall funId co fcall = do
  args <- mapM newSysLocalDs argTypes
  ccallUniq <- newUnique
  dflags <- getDynFlags
  let callApp = mkFCall dflags ccallUniq fcall (map Var args) ioResType
      rhs = mkLams tvs (mkLams args callApp)
      rhs' = Cast rhs co
  return ([(funId, rhs')], [])
  where ty                    = pFst $ coercionKind co
        (tvs, funTy)          = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy

dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header -> DsM ([Binding], [ClassExport])
dsFCall funId co fcall _
  | null thetaType && null argTypes
  , Just funPtrType <- splitFunPtrType ioResType
  , CCall (CCallSpec (StaticTarget label mPkgKey _) callConv safety) <- fcall = do
    dflags    <- getDynFlags
    ccallUniq <- newUnique
    let fcall  = CCall (CCallSpec (StaticTarget (mkFastString label') mPkgKey True) callConv safety)
        label' = serializeTarget False False True (unpackFS label) argFts resRep
        resRep = maybe VoidRep stypePrimRep resPrimType
        argFts = srepFieldTypes $ map unboxType argTypes
        (argTypes, ioResType) = tcSplitFunTys (dropForAlls funPtrType)
        resType
          | Just (tc, _) <- splitTyConApp_maybe ioResType
          , tc `hasKey` unitTyConKey = Nothing
          | Just (_, resType') <- tcSplitIOType_maybe ioResType = Just resType'
          | otherwise = Just ioResType
        resPrimType = fmap unboxType resType
        createFunPtrApp = mkFCall dflags ccallUniq fcall [Var realWorldPrimId] int64PrimTy
    funPtrAddrId <- newSysLocalDs int64PrimTy
    funPtrTyCon <- dsLookupTyCon funPtrTyConName
    let funPtrDataCon = head $ tyConDataCons funPtrTyCon
        funPtrWrapId  = dataConWrapId funPtrDataCon
        rhs = Case createFunPtrApp funPtrAddrId
                   (mkTyConApp funPtrTyCon [funPtrType])
                   [(DEFAULT, [], funPtrApp)]
        funPtrApp = mkCoreApps (Var funPtrWrapId) [Type funPtrType, Var funPtrAddrId]
        {- We give a NOINLINE so that the FunPtr doesn't get added to
           the funPtrMap twice. While getting added twice is thread-safe,
           it creates unnecessary duplication. -}
        refIdWithNoInline = funId `setInlinePragma` neverInlinePragma
    return ([(refIdWithNoInline, rhs)], [])
  | otherwise = do
    dflags <- getDynFlags
    (thetaArgs, extendsInfo) <- extendsMap thetaType
    args <- mapM newSysLocalDs argTypes
    (argPrimTypes, valArgs, argWrappers) <- mapAndUnzip3M (unboxArg extendsInfo) (map Var args)
    (resPrimType, ccallResultType, resWrapper) <- boxResult extendsInfo ioResType
    let workArgIds = [v | Var v <- valArgs]
        fcall' = genJavaFCall fcall extendsInfo argPrimTypes (Right resPrimType) ioResType
    ccallUniq <- newUnique
    workUniq  <- newUnique
    -- Build worker
    let workerType = mkForAllTys tvs $ mkFunTys (map idType workArgIds) ccallResultType
        ccallApp   = mkFCall dflags ccallUniq fcall' valArgs ccallResultType
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
    return ([(workId, workRhs), (funIdWithInline, wrapRhs')], [])

  where ty = pFst $ coercionKind co
        (tvs, thetaFunTy) = tcSplitForAllTys ty
        (thetaType, funTy) = tcSplitPhiTy thetaFunTy
        (argTypes, ioResType) = tcSplitFunTys funTy
        splitFunTyCon resType
          | Just (tc, [tyArg]) <- tcSplitTyConApp_maybe resType
          , tyConUnique tc == funPtrTyConKey
          = Just tyArg
          | otherwise = Nothing
        splitFunPtrType resType
          | arg@(Just _) <- splitFunTyCon resType = arg
          | Just (_, resTy) <- tcSplitIOType_maybe resType
          = splitFunPtrType resTy
          | otherwise = Nothing

genJavaFCall :: ForeignCall -> ExtendsInfo -> [Type] -> Either PrimRep (Maybe Type) -> Type -> ForeignCall
genJavaFCall (CCall (CCallSpec (StaticTarget label mPkgKey isFun) JavaCallConv safety))
             extendsInfo argTypes eResType ioResType
  = CCall (CCallSpec (StaticTarget (mkFastString label') mPkgKey isFun) JavaCallConv safety)
  where label' = serializeTarget hasObj hasSubclass (not (isJust mObj))
                                 (unpackFS label) argFts resRep
        argFts' = srepFieldTypes argTypes
        argFts = maybe argFts' (: argFts') mObj
        resRep = either id (maybe VoidRep stypePrimRep) eResType
        (hasObj, hasSubclass, mObj) = case tcSplitJavaType_maybe ioResType of
          Just (_, tagType, _)
            | (hasSubclass, Just clsName) <- getArgClass extendsInfo tagType
            -> (True, hasSubclass, Just $ obj clsName)
            | otherwise -> (True, False, Nothing)
          _ -> (False, False, Nothing)
genJavaFCall _ _ _ _ _ = error $ "genJavaFCall: bad genJavaFCall"

getArgClass :: ExtendsInfo -> Type -> (Bool, Maybe Text)
getArgClass extendsInfo ty
  | Just var <- getTyVar_maybe ty
  = case lookupVarEnv extendsInfo var of
      Just (_, tagType, _) ->
        let (_, res) = getArgClass extendsInfo tagType
        in (True, res)
      _ -> (False, Nothing)
  | otherwise = (False, Just $ stagTypeToText ty)

getArgType :: ExtendsInfo -> Type -> Type
getArgType extendsInfo ty
  | Just var <- getTyVar_maybe ty
  , Just (_, tagType, _) <- lookupVarEnv extendsInfo var
  = getArgType extendsInfo tagType
  | otherwise = ty

serializeTarget :: Bool -> Bool -> Bool -> String -> [FieldType] -> PrimRep -> String
serializeTarget hasObj hasSubclass qObj label' argFts resRep =
  show hasObj ++ "|" ++ show isStatic ++ "|" ++ result

  where (label, isStatic') = maybe (label', False) (, True) $ stripPrefix "@static" label'
        isStatic = isStatic' || (hasObj && qObj)

        result = case words label of
          ["@new"]              -> genNewTarget
          ["@field",label1]     -> genFieldTarget label1
          ["@interface",label1] -> genMethodTarget True  False label1
          ["@super",label1]     -> genMethodTarget False True label1
          [label1]              -> genMethodTarget False False label1
          _                     -> pprPanic "labelToTarget: bad label: " (ppr label')

        argFts' dropArg = if dropArg then drop 1 argFts else argFts

        genNewTarget = "0,"
                    ++ show clsName ++ ","
                    ++ show methodDesc
          where clsName = getObjectClass resRep
                methodDesc = mkMethodDesc' (argFts' False) void

        genFieldTarget label = "1,"
                            ++ show clsName ++ ","
                            ++ show fieldName ++ ","
                            ++ show fieldDesc ++ ","
                            ++ instr
          where (clsName, fieldName) =
                  if isStatic
                  then labelToMethod label
                  else ( getFtClass (let args = argFts' False
                                     in if length args > 0
                                        then head args
                                        else primRepFieldType resRep)
                       , T.pack label)
                fieldDesc = mkFieldDesc' fieldFt
                (instr, fieldFt) =
                  if isVoidRep resRep then
                    ( "0" -- putInstr
                    , if isStatic
                      then expectHead "serializeTarget: static field" (argFts' hasObj)
                      else expectHead "serializeTarget: instance field" (argFts' True) )
                  else
                    ( "1" -- getInstr
                    , primRepFieldType resRep )

        genMethodTarget isInterface isSuper label =
             "2,"
          ++ show isInterface ++ ","
          ++ show isSuper ++ ","
          ++ show hasSubclass ++ ","
          ++ show clsName ++ ","
          ++ show methodName ++ ","
          ++ show methodDesc
          where (clsName, methodName) =
                  if isStatic
                  then labelToMethod label
                  else (getFtClass (expectHead "serializeTarget: non-static method"
                                    (argFts' False)), T.pack label)
                resFt = primRepFieldType_maybe resRep
                methodDesc = mkMethodDesc' (argFts' (not isStatic)) resFt

extendsMap :: ThetaType -> DsM ([Id], ExtendsInfo)
extendsMap thetaType = do
  (ids, keyVals) <- flip mapAndUnzipM (zip [1 :: Int ..] thetaType) $ \(_, thetaTy) -> do
    dictId <- newSysLocalDs thetaTy
    let (var', tagTy') = tcSplitExtendsType thetaTy
        (var, tagTy, bound)
          | isTyVarTy var' = (var', tagTy', ExtendsBound)
          | otherwise = (tagTy', var', SuperBound)
    return (dictId, ( getTyVar "extendsMap: Not type variable!" var
                    , (dictId, tagTy, bound)))
  return (ids, mkVarEnv keyVals)

extendsMapWithVar :: ThetaType -> ExtendsInfo
extendsMapWithVar thetaType = mkVarEnv keyVals
  where keyVals = map extendsMapEntry thetaType
        extendsMapEntry thetaTy = (var, (var, tagTy, bound))
          where (varTy', tagTy') = tcSplitExtendsType thetaTy
                (varTy, tagTy, bound)
                  | isTyVarTy varTy' = (varTy', tagTy', ExtendsBound)
                  | otherwise = (tagTy', varTy', SuperBound)
                var = getTyVar "extendsMap: Not type variable!" varTy

unboxType :: Type -> Type
unboxType ty
  | isPrimitiveType ty = ty
  | Just (_, ty') <- topNormaliseNewType_maybe ty
  = unboxType ty'
  | Just tc <- tyConAppTyCon_maybe ty
  , tc `hasKey` boolTyConKey
  = jboolPrimTy
  | Just (tc, [ty']) <- splitTyConApp_maybe ty
  , tc `hasKey` maybeTyConKey
  = unboxType ty'
  | Just (_,_,dataCon,dataConArgTys) <- splitDataProductType_maybe ty
  , dataConSourceArity dataCon == 1
  , (dataConArgTy1 : _) <- dataConArgTys
  = unboxType dataConArgTy1
  | otherwise = ty

unboxArg :: ExtendsInfo -> CoreExpr -> DsM (Type, CoreExpr, CoreExpr -> CoreExpr)
unboxArg vs arg
  | isPrimitiveType argType
  = return (argType, arg, id)
  | Just (co, _) <- topNormaliseNewType_maybe argType
  = unboxArg vs $ mkCast arg co
  | Just tc <- tyConAppTyCon_maybe argType
  , tc `hasKey` boolTyConKey = do
      _ <- getDynFlags
      primArg <- newSysLocalDs jboolPrimTy
      -- TODO: Is this correct?
      return ( jboolPrimTy
             , Var primArg
             , \body -> Case
               (mkCoreApp (Var (primOpId Int2JBoolOp))
                          (mkWildCase arg argType intPrimTy
                           [ (DataAlt falseDataCon, [], Lit (MachInt 0))
                           , (DataAlt trueDataCon,  [], Lit (MachInt 1)) ]))
               primArg (exprType body)
               [(DEFAULT, [], body)] )
  | Just (tc, [_]) <- splitTyConApp_maybe argType
  , tc `hasKey` listTyConKey
  -- TODO: Support list types other than Char
  = do toJStringId <- dsLookupGlobalId toJStringName
       unboxArg vs $ mkCoreApp (Var toJStringId) arg
  | Just (tc, [ty]) <- splitTyConApp_maybe argType
  , tc `hasKey` maybeTyConKey
  = do innerArg <- newSysLocalDs ty
       (primTy, primArg, bodyWrapper) <- unboxArg vs (Var innerArg)
       return ( primTy
              , primArg
              , \body -> mkWildCase arg argType (exprType body)
                          [ (DataAlt nothingDataCon, [],
                             mkCoreLet (NonRec (getIdFromTrivialExpr primArg)
                                               (mkCoreApps (Var unsafeCoerceId)
                                                           [ Type (mkObjectPrimTy jstringTy) -- TODO: Hack, change this? -RM
                                                           , Type primTy
                                                           , Lit nullRefLit ]))
                                       body),
                            (DataAlt justDataCon, [innerArg], bodyWrapper body)])
  | isProductType && dataConArity == 1 = do
      primArg <- newSysLocalDs dataConArgTy1
      return ( dataConArgTy1
             , Var primArg
             , \body -> mkWildCase arg argType (exprType body)
                             [(DataAlt dataCon, [primArg], body)] )
  | Just v <- getTyVar_maybe argType
  , Just (dictId, tagType, bound) <- lookupVarEnv vs v = do
      castId <- getClassCastId bound
      let typeArgs = whenExtends bound [argType, tagType]
      unboxArg vs $ mkApps (mkTyApps (Var castId) typeArgs) [Var dictId, arg]
  | otherwise = return (argType, arg, id)  --TODO: What are the implications of allowing everything here?  - NickSeagull
  where argType                          = exprType arg
        maybeProductType                 = splitDataProductType_maybe argType
        isProductType                    = isJust maybeProductType
        Just (_,_,dataCon,dataConArgTys) = maybeProductType
        dataConArity                     = dataConSourceArity dataCon
        (dataConArgTy1 : _)              = dataConArgTys

getClassCastId :: BoundTag -> DsM Id
getClassCastId bound
  | bound == ExtendsBound = dsLookupGlobalId superCastName
  | otherwise = dsLookupGlobalId unsafeCastName

mkFCall :: DynFlags -> Unique -> ForeignCall -> [CoreExpr] -> Type -> CoreExpr
mkFCall dflags unique fcall valArgs resType
  = mkApps (mkVarApps (Var fcallId) tyVars) valArgs
  where argTypes = map exprType valArgs
        bodyType = mkFunTys argTypes resType
        tyVars  = tyVarsOfTypeList bodyType
        ty = mkForAllTys tyVars bodyType
        fcallId = mkFCallId dflags unique fcall ty

boxResult :: ExtendsInfo -> Type -> DsM (Maybe Type, Type, CoreExpr -> CoreExpr)
boxResult extendsInfo resultType
  | Just (ioTyCon, ioResType) <- tcSplitIOType_maybe resultType
  = do res@(mResType, _) <- resultWrapper extendsInfo ioResType
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
       return (mResType, realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)
  | Just (javaTyCon, javaTagType, javaResType) <- tcSplitJavaType_maybe resultType
  = do _ <- getDynFlags
       res@(mResType, _) <- resultWrapper extendsInfo javaResType
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
       return (mResType, objectType `mkFunTy` javaResultType, wrap)
  | otherwise
  = do res@(mResType, _) <- resultWrapper extendsInfo resultType
       (ccallResultType, alt) <- mkAlt returnResult res
       let wrap call = mkWildCase (App call (Var realWorldPrimId))
                                  ccallResultType
                                  (coreAltType alt)
                                  [alt]
       return (mResType, realWorldStatePrimTy `mkFunTy` ccallResultType, wrap)
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
           , \e -> mkWildCase (mkCoreApp (Var (primOpId JBool2IntOp)) e)
                   intPrimTy boolTy
                   [ (DEFAULT, [], Var trueDataConId)
                   , (LitAlt (MachInt 0), [], Var falseDataConId) ] )
  | Just (tc, [_]) <- maybeTcApp
  , tc `hasKey` listTyConKey
  = do (maybeType, wrapper) <- resultWrapper extendsInfo $
                                 mkTyConApp jstringTyCon []
       fromJStringId <- dsLookupGlobalId fromJStringName
       return (maybeType, \e -> mkCoreApp (Var fromJStringId) (wrapper e))
  | Just (tc, [ty]) <- maybeTcApp
  , tc `hasKey` maybeTyConKey
  = do (maybeType, wrapper) <- resultWrapper extendsInfo ty
       let objType = head . snd
                   . expectJust "resultWrapper: splitTyConApp"
                   . splitTyConApp_maybe
                   $ expectJust "resultWrapper: maybeType: Nothing" maybeType
       return (maybeType, \e ->
                mkWildCase (mkCoreApps (Var (primOpId IsNullObjectOp))
                                       [Type objType, e])
                           intPrimTy resultType
                           [ (DEFAULT, [], mkConApp justDataCon [ Type ty
                                                                , wrapper e])
                           , (LitAlt (MachInt 1), [], mkConApp nothingDataCon [Type ty])])
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
       let typeArgs = map Type $ whenExtends bound [resultType, tagType]
       return ( objType
              , \e ->
                  mkApps (Var castId) (typeArgs ++ [Var dictId, wrapper e]))
  | otherwise = return (Just resultType, id)
  where maybeTcApp = splitTyConApp_maybe resultType  -- TODO: What are the implications of allowing everything here?  - NickSeagull

maybeNarrow :: TyCon -> (CoreExpr -> CoreExpr)
maybeNarrow tycon
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkPrimOpId Narrow16WordOp)) e
  | otherwise                     = id

dsFExport :: Either Int Id      -- The exported Id
                                -- OR field
          -> TyCon
          -> FamInstEnvs
          -> Coercion           -- Coercion between the Haskell type callable
                                -- from C, and its representation type
          -> CLabelString       -- The name to export to C land
          -> Maybe Text         -- rawClassSpec & className
          -> Module
          -> DsM ClassExport

dsFExport closureId inheritsFamTyCon famInstEnvs co externalName classSpec mod = do
  dflags <- getDynFlags
  let resClass = typeDataConClass dflags extendsInfo resType
      (mFieldDef, loadClosureRef) =
        either
          (\i ->
              let fieldName = constrField i
              in ( Just $ mkFieldDef [Public] fieldName closureType
                , gload classFt 0
               <> getfield (mkFieldRef className fieldName closureType) ))
          (\fnId ->
              ( Nothing
              , invokestatic (mkMethodRef modClass
                                          (closure (idNameText dflags fnId))
                                          []
                                          (ret closureType))))
          closureId
      varBoundsArgs =
        mconcat $ replicate (length tyVarBounds)
                $ invokestatic (mkMethodRef trivialExtendsClass trivialExtendsMethod
                                [] (ret closureType))
      boxedArgs =
        if length argFts > 5
        then error $ "Foreign exports with number of arguments > 5 are currently not "
                  ++ "supported."
        else fst $ foldl' (\(code, stackIndex) (argPrimFt, argType) ->
                           let argClass = typeDataConClass dflags extendsInfo argType
                               argClassFt = obj argClass
                           in (code
                           <> (if argPrimFt == jbool
                               then gload argPrimFt stackIndex <> ifeq falseClosure trueClosure
                               else new argClassFt
                                 <> dup argClassFt
                                 <> gload argPrimFt stackIndex
                                 <> invokespecial
                                     (mkMethodRef argClass "<init>" [argPrimFt] void))
                               , stackIndex + fieldSize argPrimFt))
                           (mempty, localVariableStart)
                           (zip argFts argTypes)
      methodCode
        | Just superClassName <- genSuperAccessor
        , let go _ [] = mempty
              go n (ft:fts) = gload ft (fromIntegral n) <> go (n + fieldSize ft) fts
        =    gload classFt 0
          <> go 1 argFts
          <> invokespecial (mkMethodRef superClassName (T.drop 1 methodName) argFts resFt)
         <> maybe vreturn greturn resFt
        | otherwise =
             loadThis
          <> new ap2Ft
          <> dup ap2Ft
          <> invokestatic (mkMethodRef runClass runClosure
                                       [] (ret closureType))
          <> new apFt
          <> dup apFt
          <> loadClosureRef
          <> varBoundsArgs
          <> boxedArgs
          <> invokespecial (mkMethodRef apClass "<init>" (replicate numApplied closureType) void)
          <> invokespecial (mkMethodRef ap2Class "<init>" [ closureType
                                                          , closureType] void)
          -- TODO: Support java args > 5
          <> invokestatic (mkMethodRef rtsGroup evalMethod evalArgFts (ret closureType))
          <> (if voidResult
              then pop closureType <> vreturn
              else unboxResult resType resClass rawResFt)

  return ( rawClassSpec
         , addAttrsToMethodDef mAttrs $
           mkMethodDef className accessFlags methodName argFts resFt methodCode
         , mFieldDef )
  where ty = pSnd $ coercionKind co
        modClass = moduleJavaClass mod
        trivialExtendsClass = "eta/runtime/stg/Closures"
        trivialExtendsMethod = "getTrivialExtendsInstance"
        (tyVars, thetaFunTy) = tcSplitForAllTys ty
        (tyVarBounds, funTy) = tcSplitPhiTy thetaFunTy
        (argTypes, ioResType) = tcSplitFunTys funTy
        classFt  = obj className
        extendsInfo = extendsMapWithVar tyVarBounds
        loadThis
          | isJust staticMethodClass =
            if runClosure == "runJava"
            then aconst_null jobject
            else mempty
          | otherwise = gload classFt 0
        (accessFlags, localVariableStart)
          | isJust staticMethodClass = ([Public, Static], 0)
          | otherwise = ([Public], 1)
        voidResult
          | UnaryRep repResTy <- repType resType
          , isUnitTy repResTy
          = True
          | otherwise = False
        (evalArgFts, evalMethod, runClass) =
          case runClosure of
            "runJava" ->
              ([jobject, closureType], "evalJava", "base/java/TopHandler")
            _                 ->
              ([closureType], "evalIO", "base/ghc/TopHandler")
        (staticMethodClass, methodName, genSuperAccessor)
          | Just camn <- stripPrefix "@static " methodString
          , let (className, methodName) = labelToMethod camn
          = (Just className, methodName, Nothing)
          | Just mn <- stripPrefix "@super " methodString
          = (Nothing, T.pack ('$':mn), Just retrieveSuperClass)
          | otherwise = (Nothing, T.pack methodString, Nothing)
          where methodString = unpackFS externalName

        retrieveSuperClass
          | (_, rest) <- T.break (== ' ') rawClassSpec
          , let superClassPrefix = T.drop 9 rest
          , (superClass,_) <- T.break (== ' ') superClassPrefix
          = superClass
          | otherwise = pprPanic "Unable to find super class: " (ppr (T.unpack rawClassSpec))

        (rawClassSpec, className) = maybe (rawClassSpec', className')
                                          (\spec ->
                                            let (className:_) = T.words spec
                                            in (spec, className))
                                          classSpec
        (rawClassSpec', className', resType, runClosure, isTagTypeVar)
          | Just (_, resType) <- tcSplitIOType_maybe ioResType
          = (className, className, resType, "runIO", False)
          | Just (_, javaTagType', javaResType) <- tcSplitJavaType_maybe ioResType
          , javaTagType <- getArgType extendsInfo javaTagType'
          = (rawClassSpecFromInherits className famInstEnvs inheritsFamTyCon javaTagType
            ,tagTypeToText javaTagType
            ,javaResType
            ,"runJava", isTyVarTy javaTagType)
          | otherwise = (className, className, ioResType, "runNonIO", False)
          where className
                  | Just cls <- staticMethodClass = cls
                  | otherwise = modClass

        numApplied = length tyVarBounds + length argTypes + 1
        apClass = apUpdName numApplied
        apFt = obj apClass
        ap2Class = apUpdName 2
        ap2Ft = obj ap2Class
        argFts = map (getArgFt extendsInfo) argTypes
        resFt
          | voidResult = Nothing
          | otherwise  = Just $ getArgFt extendsInfo resType
        rawResFt = fromJust resFt

        methodParams = map (uncurry genMethodParam) $ zip argTypes argFts
        methodResult = fmap (genMethodParam resType) resFt
        tyVarDecls = genTyVarDecls extendsInfo
        mAttrs
          | (null tyVars ||
            -- Don't generate signature if the only tyVar is Java a r in
            -- a static method signature
            ((length tyVars == 1) && isJust staticMethodClass && isTagTypeVar))
            && allParameterLess = []
          | otherwise = [ASignature
                        (MethodSig
                          (MethodSignature tyVarDecls methodParams methodResult []))]
          where allParameterLess = all checkParameterLess (resType:argTypes)
                checkParameterLess ty
                  | Just (_, []) <- splitTyConApp_maybe ty = True
                  | otherwise = False

-- This will evaluate Inherits X and traverse the resulting type-level list and
-- construct a class spec:
-- X extends Y implements Z ...
rawClassSpecFromInherits :: Text -> FamInstEnvs -> TyCon -> Type -> Text
rawClassSpecFromInherits className famInstEnvs inheritsFamTyCon classType
  | isReflCo normalizeCo
  = rawClassSpec
  | (base:ext:impls) <- map tagTypeToText (classType : extractListElems inheritsList)
  = T.concat $ [base, " extends ", ext] ++ map (T.append " implements ") impls
  | otherwise = missingInheritsError
  where (normalizeCo, inheritsList) =
          normaliseTcApp famInstEnvs Nominal inheritsFamTyCon [classType]

        extractListElems ty
          | Just (_, [_kind, elem, recList]) <- splitTyConApp_maybe ty
          = elem : extractListElems recList
          | otherwise = []

        rawClassSpec =
          either missingAnnotationError (maybe className id)
          $ rawTagTypeToText classType

        missingInheritsError =
          pprPanic "The Inherits list for the following type must contain at least one element: " (ppr classType <+> ppr normalizeCo <+> ppr inheritsList)

        missingAnnotationError =
              pprPanic "The following type should be annotated with a CLASS annotation: "
                (ppr classType)

-- | Convert an Eta type variable to a Java Generic parameter
-- | a <: Object --> A
-- | this <: Object --> This
sigTyVarText :: Id -> Text
sigTyVarText ident = T.pack identString
  where (c:identString') = occNameString . nameOccName . idName $ ident
        identString = toUpper c : identString'

genTyVarDecls :: ExtendsInfo -> TypeVariableDeclarations TypeVariable
genTyVarDecls extendsInfo = foldVarEnv accumTyVarDecls [] extendsInfo
  where
      accumTyVarDecls (ident, typeBound, bound) acc
        | SimpleTypeParameter boundSignature <- genTypeParam typeBound
        = TypeVariableDeclaration (sigTyVarText ident) [A.ExtendsBound boundSignature]
        : acc
        | otherwise = pprPanic "accumTyVarDecls: Bad argument" (ppr (ident, typeBound, bound))

typeClassText :: Type -> Text
typeClassText = tagTypeToText

genTypeParam :: Type -> TypeParameter TypeVariable
genTypeParam ty
  | Just tyVar <- getTyVar_maybe ty
  = SimpleTypeParameter (VariableReferenceParameter (sigTyVarText tyVar))
  | Just (_, tyArgs) <- splitTyConApp_maybe ty
  = SimpleTypeParameter
    (GenericReferenceParameter
      (IClassName (typeClassText ty))
      (map genTypeParam tyArgs)
      [])
  | otherwise = SimpleTypeParameter
    $ pprPanic "genTypeParam: Not a valid type parameter" (ppr ty)

genMethodParam :: Type -> FieldType -> Parameter TypeVariable
genMethodParam argType argFt
  | isObjectFt argFt = ReferenceParameter $ genClassMethodParam argType argFt
  | otherwise        = PrimitiveParameter $ baseType argFt

fieldTypeToParam :: FieldType -> [TypeParameter TypeVariable] -> ReferenceParameter TypeVariable
fieldTypeToParam ft params = case ft of
  ObjectType c  -> GenericReferenceParameter c params []
  ArrayType ft' -> ArrayReferenceParameter (go ft')
  _             -> panic "fieldTypeToParam: Not object type"
  where go ft = case ft of
          ObjectType c  -> ReferenceParameter $ GenericReferenceParameter c [] []
          ArrayType ft' -> ReferenceParameter $ ArrayReferenceParameter (go ft')
          _             -> PrimitiveParameter $ baseType ft

genClassMethodParam :: Type -> FieldType -> ReferenceParameter TypeVariable
genClassMethodParam argType argFt
  | Just tyVar <- getTyVar_maybe argType
  = VariableReferenceParameter $ sigTyVarText tyVar
  | Just (_, tyArgs)      <- splitTyConApp_maybe argType
  , isObjectFt argFt
  = fieldTypeToParam argFt (map genTypeParam tyArgs)
  | otherwise = pprPanic "genClassMethodParam: Not a valid argument."
                  (ppr argType <+> ppr (show argFt))

getArgFt :: ExtendsInfo -> Type -> FieldType
getArgFt extendsInfo ty
  | Just var <- getTyVar_maybe ty
  = case lookupVarEnv extendsInfo var of
      Just (_, tagType, _) -> getArgFt extendsInfo tagType
      _ -> pprPanic "getArgFt: unconstrained type variable" (ppr ty)
  | otherwise = getPrimFt ty

typeDataConClass :: DynFlags -> ExtendsInfo -> Type -> Text
typeDataConClass dflags extendsInfo =
  dataConClass dflags . head . tyConDataCons . tyConAppTyCon . reduceType extendsInfo

reduceType :: ExtendsInfo -> Type -> Type
reduceType extendsInfo ty
  | Just var <- getTyVar_maybe ty
  = if | Just (_, tagType, _) <- lookupVarEnv extendsInfo var ->
         reduceType extendsInfo tagType
       | otherwise -> pprPanic "reduceType: unconstrained type variable" (ppr ty)
  | otherwise = ty

unboxResult :: Type -> Text -> FieldType -> Code
unboxResult ty resClass resPrimFt
  | isBoolTy ty = getTagMethod mempty
               <> iconst jbool 1
               <> isub
               <> greturn resPrimFt
  | otherwise = gconv closureType resClassFt
             <> getfield (mkFieldRef resClass (constrField 1) resPrimFt)
             <> greturn resPrimFt
  where resClassFt = obj resClass


getPrimFt :: Type -> FieldType
getPrimFt ty = maybe (fromJust $ repFieldType_maybe ty) id (repFieldType_maybe =<< getPrimTyOf ty)

getPrimTyOf :: Type -> Maybe UnaryType
getPrimTyOf ty
  | UnaryRep repTy <- repType ty =
      if isBoolTy repTy
      then Just jboolPrimTy
      else case splitDataProductType_maybe repTy of
        Just (_, _, _, [primTy]) -> Just primTy
        _ -> Nothing -- pprPanic "DsForeign.getPrimTyOf" $ ppr ty
getPrimTyOf _ = error $ "getPrimTyOf: bad getPrimTyOf"

dsFWrapper :: Id -> Coercion -> CLabelString -> Bool -> DsM ([Binding], [ClassExport])
dsFWrapper id co0 target isAbstract = do
  -- TODO: We effectively assume that the coercion is Refl.
  mod    <- getModule
  dflags <- getDynFlags
  envs   <- dsGetFamInstEnvs
  inheritsFamTyCon <- dsLookupTyCon inheritsFamTyConName
  (thetaArgs, extendsInfo) <- extendsMap thetaType
  args <- mapM newSysLocalDs argTypes
  (realArgs, mCastBinders) <- mapAndUnzipM (genericCast extendsInfo) (map Var args)
  (resPrimType, resWrapper) <- resultWrapper extendsInfo resType
  classExports' <- mapM (\(i, arg, methodName) ->
                          let argType = exprType arg
                              argCo = mkReflCo Representational argType
                          in dsFExport (Left i) inheritsFamTyCon envs argCo
                               methodName (Just classSpec) mod)
                        $ zip3 [1..] realArgs methodNames
  javaCallUniq <- newUnique
  let  castBinders = catMaybes mCastBinders
       binding     = mkCoreLams (tvs ++ thetaArgs ++ args) $ foldr ($)
                       (resWrapper javaCallApp) castBinders
       fcall       = CCall (CCallSpec (StaticTarget (fsLit "@new") Nothing False)
                            JavaCallConv PlayRisky)
       fcall'      = genJavaFCall fcall extendsInfo argTypes (Left (ObjectRep genClassName))
                                  resType
       javaCallApp = mkFCall dflags javaCallUniq fcall' realArgs (fromJust resPrimType)
       _ = id
                      `setIdUnfolding`
                        mkInlineUnfolding (Just (length args)) (mkCast binding co0)
       classExports =
         ( classSpec
         , mkMethodDef className [Public] "<init>" (replicate (length args) closureType) void
           ( gload genClassFt 0
          <> invokespecial (mkMethodRef superClassName "<init>" [] void)
          <> mconcat
             (map (\i -> gload genClassFt 0
                      <> gload closureType i
                      <> putfield (mkFieldRef genClassName (constrField i) closureType))
                  [1..numMethods])
          <> vreturn)
         , Nothing )
         : classExports'

  return ([(id, binding)], classExports)
  where ty                  = pFst $ coercionKind co0
        (tvs, thetaFunTy)   = tcSplitForAllTys ty
        (thetaType, funTy)  = tcSplitPhiTy thetaFunTy
        (argTypes, resType) = tcSplitFunTys funTy
        className           = tagTypeToText resType
        superClassName      = if isAbstract then className else jobjectC
        genClassFt          = obj genClassName
        genClassName        = T.concat ["eta/", className, "$Eta"]
        classSpec           = T.append genClassName $
                                 T.append (if isAbstract
                                           then " extends "
                                           else " implements ") className
        methodNames         = map mkFastString $ split ',' $ unpackFS target
        numMethods          = length argTypes

castResult :: ExtendsInfo -> Type -> Type -> DsM (Maybe [CoreExpr])
castResult extendsInfo javaTagType resultType
  | Just var <- getTyVar_maybe resultType
  , Just (dictId, tagType, bound) <- lookupVarEnv extendsInfo var
  = do castId <- getClassCastId bound
       let  rawTypeArgs = map Type [resultType, tagType]
            typeArgs = whenExtends bound rawTypeArgs
       return . Just $ rawTypeArgs ++ [ Type javaTagType
                                      , mkCoreApps (Var castId) (typeArgs ++ [Var dictId]) ]
  | otherwise = return Nothing

genericCast :: ExtendsInfo -> CoreExpr -> DsM (CoreExpr, Maybe (CoreExpr -> CoreExpr))
genericCast extendsInfo methodExpr = do
  (bindArgs, callArgs, castExprs) <- mapAndUnzip3M (castArg extendsInfo) argTypes
  mResultCastExpr <- castResult extendsInfo javaTagType resultType
  let casts = catMaybes castExprs
  if length casts > 0 || isJust mResultCastExpr
  then do
    fmapJavaId <- dsLookupGlobalId fmapJavaName
    let castedMethodBinding = mkCoreLams bindArgs . foldr ($) origMethodApp $ casts
        castedMethodType = exprType castedMethodBinding
        origMethodCall = mkCoreApps methodExpr callArgs
        origMethodApp = maybe id (\resultCast -> \e ->
                                   mkCoreApps (Var fmapJavaId) (resultCast ++ [e]))
                                 mResultCastExpr
                               $ origMethodCall
    methodBinder <- newSysLocalDs castedMethodType
    return ( Var methodBinder, Just $ bindNonRec methodBinder castedMethodBinding )
  else return (methodExpr, Nothing)
  where methodType = exprType methodExpr
        (argTypes, javaResType) = tcSplitFunTys methodType
        Just (_, javaTagType, resultType) = tcSplitJavaType_maybe javaResType

castArg :: ExtendsInfo -> Type -> DsM (Var, CoreExpr, Maybe (CoreExpr -> CoreExpr))
castArg extendsInfo argType
  | Just v <- getTyVar_maybe argType
  , Just (dictId, tagType, bound) <- lookupVarEnv extendsInfo v
  = do castId <- getClassCastId (invertBound bound)
       let typeArgs = whenExtends bound [argType, tagType]
       origArg <- newSysLocalDs tagType
       castedArg <- newSysLocalDs argType
       return ( origArg
              , Var castedArg
              , Just $ \body -> Case (mkCoreApps (Var castId)
                                                 ( map Type typeArgs
                                                ++ [Var dictId, Var origArg]))
                                castedArg
                                (exprType body)
                                [(DEFAULT,[],body)] )
  | otherwise = do
      arg <- newSysLocalDs argType
      return (arg, Var arg, Nothing)

whenExtends :: BoundTag -> [a] -> [a]
whenExtends bound xs = if bound == ExtendsBound then xs else reverse xs
