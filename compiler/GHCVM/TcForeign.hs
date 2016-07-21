module GHCVM.TcForeign where

import TysWiredIn (unitTyCon)
import ErrUtils
import DynFlags
import DataCon
import Unique
import PrelNames
import Type
import TypeRep
import Coercion
import TcType (tcSplitForAllTys, tcSplitFunTys, tcSplitTyConApp_maybe)
import SrcLoc
import ForeignCall
import FamInst
import FamInstEnv
import TyCon
import Name
import GHCVM.Debug
import Id
import RdrName
import HsSyn
import TcForeign (isForeignImport, isForeignExport)
import TcRnMonad
import TcHsType
import TcExpr
import TcEnv
import Bag

import GHCVM.Primitive

-- TODO: Temporary hack
javaCallConv :: CCallConv
javaCallConv = JavaScriptCallConv

tcForeignImports :: [LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
tcForeignImports decls = do
  (ids, decls, gres) <- mapAndUnzip3M tcFImport $ filter isForeignImport decls
  return (ids, decls, unionManyBags gres)

tcFImport :: LForeignDecl Name -> TcM (Id, LForeignDecl Id, Bag GlobalRdrElt)
tcFImport (L declLoc fi@(ForeignImport (L nameLoc name) hsType _ impDecl))
  = setSrcSpan declLoc . addErrCtxt (foreignDeclCtxt fi) $ do
      sigType <- tcHsSigType (ForSigCtxt name) hsType
      (normCo, normSigType, gres) <- normaliseFfiType sigType
      let (_, ty)             = tcSplitForAllTys normSigType
          (argTypes, resType) = tcSplitFunTys ty
          id                  = mkLocalId name sigType
      impDecl' <- tcCheckFIType argTypes resType impDecl
      let fiDecl = ForeignImport (L nameLoc id) undefined
                                 (mkSymCo normCo) impDecl'
      return (id, L declLoc fiDecl, gres)
tcFImport d = pprPanic "tcFImport" (ppr d)

normaliseFfiType :: Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType ty = do
  famEnvs <- tcGetFamInstEnvs
  normaliseFfiType' famEnvs ty

normaliseFfiType' :: FamInstEnvs -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType' env ty0 = go initRecTc ty0
  where go :: RecTcChecker -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
        go recNts ty
          | Just ty' <- coreView ty = go recNts ty'
        go recNts ty@(TyConApp tc tys)
          -- TODO: Address funPtrs and the Java monad
          | tcKey `elem` [ioTyConKey]
          = childrenOnly
          | isNewTyCon tc
          , Just recNts' <- checkRecTc recNts tc
          = do
              rdrEnv <- getGlobalRdrEnv
              case checkNewtypeFFI rdrEnv tc of
                Nothing -> nothing
                Just gre -> do
                  (co', ty', gres) <- go recNts' ntRhs
                  return (mkTransCo ntCo co', ty', gre `consBag` gres)
          | isFamilyTyCon tc
          , (co, nty) <- normaliseTcApp env Representational tc tys
          , not (isReflCo co)
          = do (co', ty', gres) <- go recNts nty
               return (mkTransCo co co', ty', gres)
          | otherwise
          = nothing
          where tcKey = getUnique tc
                childrenOnly = do
                  xs <- mapM (go recNts) tys
                  let (cos, tys', gres) = unzip3 xs
                      cos' = zipWith3 downgradeRole (tyConRoles tc)
                                      (repeat Representational) cos
                  return ( mkTyConAppCo Representational tc cos'
                         , mkTyConApp tc tys'
                         , unionManyBags gres )
                ntCo = mkUnbranchedAxInstCo Representational (newTyConCo tc)
                                            tys
                ntRhs = newTyConInstRhs tc tys
                nothing = return (Refl Representational ty, ty, emptyBag)
        go recNts (FunTy ty1 ty2) = do
          (coi1, nty1, gres1) <- go recNts ty1
          (coi2, nty2, gres2) <- go recNts ty2
          return (mkFunCo Representational coi1 coi2, mkFunTy nty1 nty2,
                  gres1 `unionBags` gres2)
        go recNts (ForAllTy tyVar ty) = do
          (coi, nty, gres) <- go recNts ty
          return (mkForAllCo tyVar coi, ForAllTy tyVar nty, gres)
        go _ ty@(TyVarTy {}) = return (Refl Representational ty, ty, emptyBag)
        go _ ty@(LitTy {})   = return (Refl Representational ty, ty, emptyBag)
        go _ ty@(AppTy {})   = return (Refl Representational ty, ty, emptyBag)

checkNewtypeFFI :: GlobalRdrEnv -> TyCon -> Maybe GlobalRdrElt
checkNewtypeFFI rdrEnv tc
  | Just con <- tyConSingleDataCon_maybe tc
  , [gre] <- lookupGRE_Name rdrEnv (dataConName con)
  = Just gre
  | otherwise
  = Nothing

foreignDeclCtxt :: ForeignDecl Name -> SDoc
foreignDeclCtxt fo
  = hang (str "When checking declaration:")
       2 (ppr fo)

tcCheckFIType :: [Type] -> Type -> ForeignImport -> TcM ForeignImport
-- TODO: Handle CLabel and CWrapper
tcCheckFIType argTypes resType idecl@(CImport (L lc cconv) (L ls safety) mh
                                              (CFunction target) src)
  -- TODO: Handle dynamic targets
  | cconv == PrimCallConv = do
      dflags <- getDynFlags
      checkTc (xopt Opt_GHCForeignImportPrim dflags)
              (text "Use GHCForeignImportPrim to allow `foreign import prim'.")
      -- TODO: Validate the code generation mode
      -- TODO: Validate the target string
      checkJavaTarget target
      checkTc (playSafe safety)
              (text $ "The safe/unsafe annotation should not be used with "
                   ++ "`foreign import prim'.")
      checkForeignArgs (isFFIPrimArgumentTy dflags) argTypes
      checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) resType
      return idecl
  -- TODO: Because of the rigidity of the GHC API, we have to reuse the
  --       existing calling convention data types.
  | cconv == javaCallConv = do
      -- TODO: Validate the code generation mode
      -- TODO: Validate the target string
      dflags <- getDynFlags
      checkJavaTarget target
      checkForeignArgs (isFFIArgumentTy dflags safety) argTypes
      checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) resType
      return idecl
  -- TODO: Support the other C-based conventions
  | otherwise = pprPanic "Unsupported calling convention." (ppr idecl)

check :: Validity -> (MsgDoc -> MsgDoc) -> TcM ()
check IsValid _             = return ()
check (NotValid doc) err_fn = addErrTc (err_fn doc)

checkForeignArgs :: (Type -> Validity) -> [Type] -> TcM ()
checkForeignArgs pred tys = mapM_ go tys
  where
    go ty = check (pred ty) (illegalForeignTyErr argument)

illegalForeignTyErr :: SDoc -> SDoc -> SDoc
illegalForeignTyErr argOrRes extra
  = hang msg 2 extra
  where
    msg = hsep [ str "Unacceptable", argOrRes
               , str "type in foreign declaration:"]

isFFIPrimArgumentTy :: DynFlags -> Type -> Validity
isFFIPrimArgumentTy dflags ty
  | isAnyTy ty = IsValid
  | otherwise  = checkRepTyCon (legalFIPrimArgTyCon dflags) ty empty

isFFIPrimResultTy :: DynFlags -> Type -> Validity
isFFIPrimResultTy dflags ty
   = checkRepTyCon (legalFIPrimResultTyCon dflags) ty empty

checkRepTyCon :: (TyCon -> Type -> Bool) -> Type -> SDoc -> Validity
checkRepTyCon checkTc ty extra
  = case splitTyConApp_maybe ty of
      Just (tc, tys)
        | isNewTyCon tc  -> NotValid $ hang msg 2 (mkNtReason tc tys $$ ntFix)
        | checkTc tc ty  -> IsValid
        | otherwise      -> NotValid $ msg $$ extra
      Nothing ->
        NotValid $ quotes (ppr ty) <+> str "is not a data type" $$ extra
  where
    msg = quotes (ppr ty) <+> str "cannot be marshalled in a foreign call"
    mkNtReason tc tys
      | null tys  = str "because its data construtor is not in scope"
      | otherwise = str "because the data construtor for"
                    <+> quotes (ppr tc) <+> str "is not in scope"
    ntFix = str $ "Possible fix: import the data constructor to bring it"
               ++ " into scope"

legalFIPrimArgTyCon :: DynFlags -> TyCon -> Type -> Bool
legalFIPrimArgTyCon dflags tc ty
  | xopt Opt_UnliftedFFITypes dflags
    && isUnLiftedTyCon tc
    && not (isUnboxedTupleTyCon tc)
  = True
  | otherwise
  = False

legalFIPrimResultTyCon :: DynFlags -> TyCon -> Type -> Bool
legalFIPrimResultTyCon dflags tc ty
  | xopt Opt_UnliftedFFITypes dflags
    && isUnLiftedTyCon tc
    && (  isUnboxedTupleTyCon tc
       || not (isVoidJRep (typeJPrimRep ty)) )
  = True
  | otherwise
  = False

checkForeignRes :: Bool -> Bool -> (Type -> Validity) -> Type -> TcM ()
checkForeignRes nonIOResultOk checkSafe predResType ty
  | Just (_, resType) <- tcSplitIOType_maybe ty
  = check (predResType resType) (illegalForeignTyErr result)

  -- Case for non-IO result type with FFI Import
  | not nonIOResultOk = addErrTc
                      . illegalForeignTyErr result
                      $ str "IO result type expected"
  | otherwise = do
      dflags <- getDynFlags
      case predResType ty of
        -- Handle normal typecheck fail, we want to handle this first and
        -- only report safe haskell errors if the normal type check is OK.
        NotValid msg -> addErrTc $ illegalForeignTyErr result msg
        -- handle safe infer fail
        _ | checkSafe && safeInferOn dflags -> recordUnsafeInfer
        -- handle safe language typecheck fail
        _ | checkSafe && safeLanguageOn dflags ->
            addErrTc $ illegalForeignTyErr result safeHsErr
        -- sucess! non-IO return is fine
        _ -> return ()
  where safeHsErr = str $ "Safe Haskell is on, all FFI imports must be in the"
                       ++ " IO monad"

-- TODO: Take into account the Java monad
tcSplitIOType_maybe :: Type -> Maybe (TyCon, Type)
tcSplitIOType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (ioTyCon, [ioResType])
         | ioTyCon `hasKey` ioTyConKey ->
            Just (ioTyCon, ioResType)
        _ ->
            Nothing

isFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity
isFFIArgumentTy dflags safety ty
  = checkRepTyCon (legalOutgoingTyCon dflags safety) ty empty

isFFIImportResultTy :: DynFlags -> Type -> Validity
isFFIImportResultTy dflags ty
  = checkRepTyCon (legalFIResultTyCon dflags) ty empty

argument, result :: SDoc
argument = text "argument"
result   = text "result"

checkSafe :: Bool
checkSafe = True

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Type -> Bool
legalOutgoingTyCon dflags _ tc ty = marshalableTyCon dflags tc ty

legalFIResultTyCon :: DynFlags -> TyCon -> Type -> Bool
legalFIResultTyCon dflags tc ty
  | tc == unitTyCon         = True
  | otherwise               = marshalableTyCon dflags tc ty

marshalableTyCon :: DynFlags -> TyCon -> Type -> Bool
marshalableTyCon dflags tc ty
  |  (xopt Opt_UnliftedFFITypes dflags
      && isUnLiftedTyCon tc
      && not (isUnboxedTupleTyCon tc)
      && not (isVoidJRep (typeJPrimRep ty)))
  = True
  | otherwise
  = boxedMarshalableTyCon tc

boxedMarshalableTyCon :: TyCon -> Bool
boxedMarshalableTyCon tc
   -- TODO: Add more GHCVM-specific tycons here
   | getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
                         , int32TyConKey, int64TyConKey
                         , wordTyConKey, word8TyConKey, word16TyConKey
                         , word32TyConKey, word64TyConKey
                         , floatTyConKey, doubleTyConKey
                         , ptrTyConKey, funPtrTyConKey
                         , charTyConKey , stablePtrTyConKey
                         , boolTyConKey ]
  = True
  | otherwise = False

nonIOok :: Bool
nonIOok = True

checkJavaTarget :: CCallTarget -> TcM ()
checkJavaTarget (StaticTarget str _ _) = do
  -- TODO: Validate the name
  return ()

isAnyTy :: Type -> Bool
isAnyTy = isTc anyTyConKey

isTc :: Unique -> Type -> Bool
isTc uniq ty = case tcSplitTyConApp_maybe ty of
  Just (tc, _) -> uniq == getUnique tc
  Nothing      -> False
