module ETA.TypeCheck.TcForeign
  ( tcForeignImports
  , tcForeignExports
  -- Low-level exports for hooks
  , isForeignImport, isForeignExport
  , tcFImport --, tcFExport
--  , tcForeignImports'
  , tcCheckFIType, checkJavaTarget, checkForeignArgs, checkForeignRes
  , normaliseFfiType
  , nonIOok, mustBeIO
  , checkSafe, noCheckSafe
 -- , tcForeignExports'
 -- , tcCheckFEType
  ) where

import ETA.BasicTypes.DataCon
import ETA.BasicTypes.Unique
import ETA.BasicTypes.SrcLoc
import ETA.BasicTypes.Name
import ETA.BasicTypes.VarSet
import ETA.BasicTypes.Id
import ETA.BasicTypes.RdrName
import ETA.TypeCheck.FamInst
import ETA.TypeCheck.TcRnMonad
import ETA.TypeCheck.TcHsType
import ETA.TypeCheck.TcExpr
import ETA.TypeCheck.TcEnv
import ETA.TypeCheck.TcType
import ETA.Prelude.TysWiredIn (unitTyCon)
import ETA.Prelude.PrelNames
import ETA.Prelude.ForeignCall
import ETA.Main.Hooks
import ETA.Main.ErrUtils
import ETA.Main.DynFlags
import ETA.Types.FamInstEnv
import ETA.Types.Type
import ETA.Types.TypeRep
import ETA.Types.Coercion
import ETA.Types.TyCon
import ETA.Debug
import ETA.HsSyn.HsSyn
import ETA.Utils.Bag
import ETA.Utils.Outputable
import ETA.Utils.FastString
import ETA.Utils.Maybes

import Data.Maybe(fromMaybe)

-- Defines a binding
isForeignImport :: LForeignDecl name -> Bool
isForeignImport (L _ ForeignImport {}) = True
isForeignImport _                             = False

-- Exports a binding
isForeignExport :: LForeignDecl name -> Bool
isForeignExport (L _ ForeignExport {}) = True
isForeignExport _                             = False

tcForeignImports :: [LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
tcForeignImports decls
  = getHooked tcForeignImportsHook tcForeignImports' >>= ($ decls)

tcForeignImports' :: [LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
tcForeignImports' decls = do
  (ids, decls, gres) <- mapAndUnzip3M tcFImport $ filter isForeignImport decls
  return (ids, decls, unionManyBags gres)

printDebug h s = do
  dflags <- getDynFlags
  liftIO . putStrLn . showSDoc dflags $ (ptext $ sLit h) <+> s

tcFImport :: LForeignDecl Name -> TcM (Id, LForeignDecl Id, Bag GlobalRdrElt)
tcFImport (L declLoc fi@(ForeignImport (L nameLoc name) hsType _ impDecl))
  = setSrcSpan declLoc . addErrCtxt (foreignDeclCtxt fi) $ do
      sigType <- tcHsSigType (ForSigCtxt name) hsType
      --printDebug "tcFImport: sigType" $ ppr sigType
      (normCo, normSigType, gres) <- normaliseFfiType sigType
      --printDebug "tcFImport: normSigType" $ ppr normSigType
      let (_, ty)             = tcSplitForAllTys normSigType
          (theta, ty')        = tcSplitPhiTy ty
          (argTypes, resType) = tcSplitFunTys ty'
          id                  = mkLocalId name sigType
      traceTc "tcFIImport" $ ppr theta <+> ppr argTypes <+> ppr resType
      --printDebug "tcFImport: normSigType" $ ppr argTypes <+> ppr resType
      impDecl' <- tcCheckFIType theta argTypes resType impDecl
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
          -- TODO: Address funPtrs
          | tcKey == ioTyConKey
          = childrenOnly False
          | tcKey == javaTyConKey
          = childrenOnly True
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
                childrenOnly isJava = do
                  xs <- mapM (go recNts) tys
                  let (cos, tys', gres) = unzip3 xs
                      cos' = zipWith3 downgradeRole (tyConRoles tc)
                                      ((if isJava then [Nominal] else [])
                                       ++ repeat Representational) cos
                      co' = mkTyConAppCo Representational tc cos'
                  return ( co'
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

tcCheckFIType :: ThetaType -> [Type] -> Type -> ForeignImport -> TcM ForeignImport
tcCheckFIType thetaType argTypes resType idecl@(CImport (L lc cconv) (L ls safety) mh
                                               targetSpec src)
  | CFunction target <- targetSpec
  = case cconv of
      PrimCallConv -> do
        dflags <- getDynFlags
        checkTc (xopt Opt_GHCForeignImportPrim dflags)
                (text "Use GHCForeignImportPrim to allow `foreign import prim'.")
        -- TODO: Validate the target string
        checkJavaTarget target
        checkTc (playSafe safety)
                (text $ "The safe/unsafe annotation should not be used with "
                    ++ "`foreign import prim'.")
        checkForeignArgs (isFFIPrimArgumentTy dflags) argTypes
        checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) resType
        return idecl
      JavaCallConv -> do
        -- TODO: Validate the target string for @new, @field
        -- TODO: Validate ThetaType
        dflags <- getDynFlags
        checkJavaTarget target
        let javaClassVars = extendsVars thetaType
        checkForeignArgs (isFFIArgumentTy dflags safety javaClassVars) argTypes
        checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) resType
        return idecl
      _ -> pprPanic "tcCheckFIType: Unsupported calling convention." (ppr idecl)
  | CWrapper target isAbstract <- targetSpec
  , JavaCallConv <- cconv
  = do
      -- TODO: Validate target
      dflags <- getDynFlags
      let javaClassVars = extendsVars thetaType
      -- TODO: Typecheck foreign wrappers properly
      -- checkForeignArgs (isFFIArgumentTy dflags safety javaClassVars) argTypes
      -- checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) resType
      return idecl
  | otherwise = pprPanic "tcCheckFIType: Unsupported calling convention." (ppr idecl)

tcCheckFIType _ _ _ idecl = pprPanic "tcCheckFIType: Unsupported calling convention." (ppr idecl)

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

checkForeignRes :: Bool -> Bool -> (Type -> Validity) -> Type -> TcM ()
checkForeignRes nonIOResultOk checkSafe predResType ty
  | Just (_, resType) <- tcSplitIOType_maybe ty
  = do
      traceTc "checkForeignRes[IO]" $ ppr resType
      check (predResType resType) (illegalForeignTyErr result)
  | Just (_, tagType, resType) <- tcSplitJavaType_maybe ty
  = do
      traceTc "checkForeignRes[Java]" $ ppr tagType <+> ppr resType
      check (predResType resType) (illegalForeignTyErr result)
  -- Case for non-IO result type with FFI Import
  | not nonIOResultOk = addErrTc
                      . illegalForeignTyErr result
                      $ str "IO result type expected"
  | otherwise = do
      traceTc "checkForeignRes[Other]" $ ppr ty
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

argument, result :: SDoc
argument = text "argument"
result   = text "result"

checkSafe, noCheckSafe :: Bool
checkSafe = True
noCheckSafe = False

nonIOok, mustBeIO :: Bool
nonIOok = True
mustBeIO = False

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

tcForeignExports :: [LForeignDecl Name]
                 -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
tcForeignExports decls =
  getHooked tcForeignExportsHook tcForeignExports' >>= ($ decls)

tcForeignExports' :: [LForeignDecl Name]
                 -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
tcForeignExports' decls = foldlM combine (emptyLHsBinds, [], emptyBag)
                                 (filter isForeignExport decls)
  where combine (binds, fs, gres1) (L loc fe) = do
          (b, f, gres2) <- setSrcSpan loc (tcFExport fe)
          return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

tcFExport :: ForeignDecl Name -> TcM (LHsBind Id, ForeignDecl Id, Bag GlobalRdrElt)
tcFExport fo@(ForeignExport (L loc nm) hs_ty _ spec)
  = addErrCtxt (foreignDeclCtxt fo) $ do
      sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
      rhs <- tcPolyExpr (nlHsVar nm) sig_ty
      (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
      spec' <- tcCheckFEType norm_sig_ty spec
      id  <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
      return (mkVarBind id rhs, ForeignExport (L loc id) undefined norm_co spec', gres)

tcFExport d = pprPanic "tcFExport" (ppr d)

tcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
tcCheckFEType sigType exportspec = do
-- (CExport (L l (CExportStatic str cconv)) src)
    checkForeignArgs isFFIExternalTy argTypes
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy resType
    return exportspec
  where (_, ty)             = tcSplitForAllTys sigType
        (thetaType, ty')    = tcSplitPhiTy ty
        (argTypes, resType) = tcSplitFunTys ty'
        javaClassVars       = extendsVars thetaType
