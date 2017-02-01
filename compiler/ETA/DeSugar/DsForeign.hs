{-
(c) Rahul Muttineni 2016-2017
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


Desugaring foreign declarations.
-}

{-# LANGUAGE OverloadedStrings #-}
module ETA.DeSugar.DsForeign where

import ETA.Core.CoreSyn
import ETA.Core.CoreUtils
import ETA.Core.MkCore
import ETA.DeSugar.DsMonad
import ETA.HsSyn.HsSyn
import ETA.Core.CoreUnfold
import ETA.BasicTypes.VarEnv
import ETA.BasicTypes.VarSet
import ETA.BasicTypes.Id
import ETA.BasicTypes.Var
import ETA.BasicTypes.MkId
import ETA.BasicTypes.Literal
import ETA.BasicTypes.Module
import ETA.BasicTypes.Name
import ETA.BasicTypes.DataCon
import ETA.Types.Type
import ETA.Types.TyCon
import ETA.Types.TypeRep
import ETA.Types.Coercion
import ETA.TypeCheck.TcRnMonad
import ETA.TypeCheck.TcEnv
import ETA.TypeCheck.TcType

import ETA.Main.HscTypes
import ETA.Prelude.ForeignCall
import ETA.Prelude.TysWiredIn
import ETA.Prelude.TysPrim
import ETA.Prelude.PrelNames
import ETA.Prelude.PrelInfo ( primOpId )
import ETA.Prelude.PrimOp
import ETA.BasicTypes.BasicTypes
import ETA.BasicTypes.SrcLoc
import ETA.Utils.Outputable hiding ((<>))
import ETA.Utils.FastString
import ETA.Main.DynFlags
import ETA.Utils.Platform
import ETA.Utils.MonadUtils
import ETA.Utils.Maybes (expectJust)
import ETA.Utils.OrdList
import ETA.Utils.Pair
import ETA.Utils.Util
import ETA.Main.Hooks
import ETA.CodeGen.ArgRep ( repFieldTypes, repFieldType_maybe, primRepFieldType
                            , primRepFieldType_maybe )
import ETA.CodeGen.Rts
import ETA.CodeGen.Name

import Data.Maybe
import Data.Monoid((<>), mconcat)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import ETA.Debug
import Codec.JVM

type Binding = (Id, CoreExpr)
type ClassExport = (Text, MethodDef, Maybe FieldDef)
data Bound = SuperBound | ExtendsBound
  deriving (Eq, Show)
type ExtendsInfo = VarEnv (Id, Type, Bound)

invertBound :: Bound -> Bound
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
                              (CExport (L _ (CExportStatic extName cconv)) _)) = do
            method <- dsFExport (Right id) co extName Nothing
            return ([method], [])
        doDecl fi = pprPanic "doDecl: Not implemented" (ppr fi)

dsFImport :: Id -> Coercion -> ForeignImport -> DsM ([Binding], [ClassExport])
dsFImport id co (CImport cconv safety mHeader spec _) =
  dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

dsCImport :: Id -> Coercion -> CImportSpec -> CCallConv -> Safety
  -> Maybe Header -> DsM ([Binding], [ClassExport])
dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id co (CWrapper target isAbstract) cconv safety mHeader
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
dsFCall funId co fcall mDeclHeader = do
  dflags <- getDynFlags
  (thetaArgs, extendsInfo) <- extendsMap thetaType
  args <- mapM newSysLocalDs argTypes
  (argPrimTypes, valArgs, argWrappers) <- mapAndUnzip3M (unboxArg extendsInfo) (map Var args)
  (resPrimType, ccallResultType, resWrapper) <- boxResult extendsInfo ioResType
  let workArgIds = [v | Var v <- valArgs]
      fcall' = genJavaFCall fcall extendsInfo argPrimTypes (Right resPrimType) ioResType
  ccallUniq <- newUnique
  workUniq <- newUnique
  -- Build worker
  let workerType = mkForAllTys tvs $
                   mkFunTys (map idType workArgIds) ccallResultType
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

genJavaFCall :: ForeignCall -> ExtendsInfo -> [Type] -> Either PrimRep (Maybe Type) -> Type -> ForeignCall
genJavaFCall (CCall (CCallSpec (StaticTarget label mPkgKey isFun) JavaCallConv safety))
             extendsInfo argTypes eResType ioResType
  = CCall (CCallSpec (StaticTarget (mkFastString label') mPkgKey isFun) JavaCallConv safety)
  where label' = serializeTarget hasObj hasSubclass (not (isJust mObj))
                                 (unpackFS label) argFts resRep
        argFts' = repFieldTypes argTypes
        argFts = maybe argFts' (: argFts') mObj
        resRep = either id (maybe VoidRep typePrimRep) eResType
        (hasObj, hasSubclass, mObj) = case tcSplitJavaType_maybe ioResType of
          Just (_, tagType, _)
            | (hasSubclass, Just clsName) <- getArgClass extendsInfo tagType
            -> (True, hasSubclass, Just $ obj clsName)
            | otherwise -> (True, False, Nothing)
          _ -> (False, False, Nothing)

getArgClass :: ExtendsInfo -> Type -> (Bool, Maybe Text)
getArgClass extendsInfo ty
  | Just var <- getTyVar_maybe ty
  = case lookupVarEnv extendsInfo var of
      Just (_, tagType, _) ->
        let (_, res) = getArgClass extendsInfo tagType
        in (True, res)
      _ -> (False, Nothing)
  | otherwise = (False, Just $ tagTypeToText ty)

serializeTarget :: Bool -> Bool -> Bool -> String -> [FieldType] -> PrimRep -> String
serializeTarget hasObj hasSubclass qObj label' argFts resRep =
  show hasObj ++ "|" ++ show isStatic ++ "|" ++ result

  where (label, isStatic') = maybe (label', False) (, True) $ stripPrefix "@static" label'
        isStatic = isStatic' || (hasObj && qObj)

        result = case words label of
          ["@new"]              -> genNewTarget
          ["@field",label1]     -> genFieldTarget label1
          ["@interface",label1] -> genMethodTarget True label1
          [label1]              -> genMethodTarget False label1
          _                     -> pprPanic "labelToTarget: bad label: " (ppr label')

        argFts' dropArg = if dropArg then drop 1 argFts else argFts

        genNewTarget = show 0 ++ ","
                    ++ show clsName ++ ","
                    ++ show methodDesc
          where clsName = getObjectClass resRep
                methodDesc = mkMethodDesc' (argFts' False) void

        genFieldTarget label = show 1 ++ ","
                            ++ show clsName ++ ","
                            ++ show fieldName ++ ","
                            ++ show fieldDesc ++ ","
                            ++ show instr
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
                    ( 0 -- putInstr
                    , if isStatic
                      then expectHead "serializeTarget: static field" (argFts' hasObj)
                      else expectHead "serializeTarget: instance field" (argFts' True) )
                  else
                    ( 1 -- getInstr
                    , primRepFieldType resRep )

        genMethodTarget isInterface label = show 2 ++ ","
                                         ++ show isInterface ++ ","
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
  (ids, keyVals) <- flip mapAndUnzipM (zip [1..] thetaType) $ \(i, thetaTy) -> do
    dictId <- newSysLocalDs thetaTy
    let (var', tagTy') = tcSplitExtendsType thetaTy
        (var, tagTy, bound)
          | isTyVarTy var' = (var', tagTy', ExtendsBound)
          | otherwise = (tagTy', var', SuperBound)
    return (dictId, ( getTyVar "extendsMap: Not type variable!" var
                    , (dictId, tagTy, bound)))
  return (ids, mkVarEnv keyVals)

unboxArg :: ExtendsInfo -> CoreExpr -> DsM (Type, CoreExpr, CoreExpr -> CoreExpr)
unboxArg vs arg
  | isPrimitiveType argType
  = return (argType, arg, id)
  | Just (co, _) <- topNormaliseNewType_maybe argType
  = unboxArg vs $ mkCast arg co
  | Just tc <- tyConAppTyCon_maybe argType
  , tc `hasKey` boolTyConKey = do
      dflags <- getDynFlags
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
  | Just (tc, [ty]) <- splitTyConApp_maybe argType
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
                                                           [ Type addrPrimTy
                                                           , Type primTy
                                                           , Lit nullAddrLit ]))
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
  | bound == ExtendsBound = dsLookupGlobalId superCastName
  | otherwise = dsLookupGlobalId unsafeCastName

mkFCall :: DynFlags -> Unique -> ForeignCall -> [CoreExpr] -> Type -> CoreExpr
mkFCall dflags unique fcall valArgs resType
  = mkApps (mkVarApps (Var fcallId) tyVars) valArgs
  where argTypes = map exprType valArgs
        bodyType = mkFunTys argTypes resType
        tyVars = varSetElems $ tyVarsOfType bodyType
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
  = do dflags <- getDynFlags
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
           , \e -> mkWildCase e jboolPrimTy boolTy
                   [ (DEFAULT, [], Var trueDataConId)
                   , (LitAlt (MachInt 0), [], Var falseDataConId) ] )
  | Just (tc, [ty]) <- maybeTcApp
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

dsFExport :: Either Int Id      -- The exported Id
                                -- OR field
          -> Coercion           -- Coercion between the Haskell type callable
                                -- from C, and its representation type
          -> CLabelString       -- The name to export to C land
          -> Maybe Text         -- rawClassSpec & className
          -> DsM ClassExport

dsFExport closureId co externalName classSpec = do
  mod <- fmap ds_mod getGblEnv
  dflags <- getDynFlags
  let resClass = typeDataConClass dflags resType
      (mFieldDef, loadClosureRef) =
        either
          (\i ->
              let fieldName = constrField i
              in ( Just $ mkFieldDef [Public] fieldName closureType
                , gload classFt 0
               <> getfield (mkFieldRef className fieldName closureType) ))
          (\fnId ->
              ( Nothing
              , getstatic (mkFieldRef (moduleJavaClass mod)
                                      (closure (idNameText dflags fnId))
                                      closureType)))
          closureId
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
                            else new argClassFt
                              <> dup argClassFt
                              <> gload argPrimFt i
                              <> invokespecial
                                   (mkMethodRef argClass "<init>" [argPrimFt] void)))
                    mempty
                    (zip3 [1..] argFts argTypes)
      numApplied = length argTypes + 1
      apClass = apUpdName numApplied
      apFt = obj apClass
      ap2Class = apUpdName 2
      ap2Ft = obj ap2Class
  return ( rawClassSpec
         , mkMethodDef className [Public] methodName argFts resFt $
             invokestatic (mkMethodRef rtsGroup "lock" [] (ret capabilityType))
          <> gload classFt 0
          <> new ap2Ft
          <> dup ap2Ft
          <> getstatic (mkFieldRef "base/java/TopHandler" "runJava_closure"
                                   closureType)
          <> new apFt
          <> dup apFt
          <> loadClosureRef
          <> boxedArgs
          <> invokespecial (mkMethodRef apClass "<init>" (replicate numApplied closureType) void)
          <> invokespecial (mkMethodRef ap2Class "<init>" [ closureType
                                                          , closureType] void)
          -- TODO: Support java args > 5
          <> invokestatic (mkMethodRef rtsGroup "evalJava"
                                       [capabilityType, jobject, closureType]
                                       (ret hsResultType))
          <> (if voidResult
              then mempty
              else dup hsResultType)
          <> hsResultCap
          <> invokestatic (mkMethodRef rtsGroup "unlock" [capabilityType] void)
          -- TODO: add a call to checkSchedStatus
          <> (if voidResult
             then vreturn
             else ( hsResultValue
                 <> unboxResult resType resClass rawResFt))
         , mFieldDef )
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
        (rawClassSpec, className) = maybe (rawClassSpec', className')
                                          (\spec ->
                                            let (className:_) = T.words spec
                                            in (spec, className))
                                          classSpec
        (rawClassSpec', className', resType) =
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

dsFWrapper :: Id -> Coercion -> CLabelString -> Bool -> DsM ([Binding], [ClassExport])
dsFWrapper id co0 target isAbstract = do
  -- TODO: We effectively assume that the coercion is Refl.
  dflags <- getDynFlags
  (thetaArgs, extendsInfo) <- extendsMap thetaType
  args <- mapM newSysLocalDs argTypes
  (realArgs, mCastBinders) <- mapAndUnzipM (genericCast extendsInfo) (map Var args)
  (resPrimType, resWrapper) <- resultWrapper extendsInfo resType
  classExports' <- mapM (\(i, arg, methodName) ->
                          let argType = exprType arg
                              argCo = mkReflCo Representational argType
                          in dsFExport (Left i) argCo methodName $ Just classSpec)
                  $ zip3 [1..] realArgs methodNames
  javaCallUniq <- newUnique
  let  castBinders = catMaybes mCastBinders
       binding     = mkCoreLams (tvs ++ thetaArgs ++ args) $ foldr ($)
                       (resWrapper javaCallApp) castBinders
       fcall       = CCall (CCallSpec (StaticTarget (fsLit "@new") Nothing True)
                            JavaCallConv PlayRisky)
       fcall'      = genJavaFCall fcall extendsInfo argTypes (Left (ObjectRep genClassName))
                                  resType
       javaCallApp = mkFCall dflags javaCallUniq fcall' realArgs (fromJust resPrimType)
       idWithInline = id
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

whenExtends :: Bound -> [a] -> [a]
whenExtends bound xs = if bound == ExtendsBound then xs else reverse xs
