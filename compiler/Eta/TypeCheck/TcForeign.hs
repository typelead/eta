module Eta.TypeCheck.TcForeign
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

import Eta.BasicTypes.DataCon
import Eta.BasicTypes.Unique
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.Name
-- import Eta.BasicTypes.VarSet
import qualified Eta.LanguageExtensions as LangExt
import Eta.BasicTypes.Id
import Eta.BasicTypes.RdrName
import Eta.TypeCheck.FamInst
import Eta.TypeCheck.TcRnMonad
import Eta.TypeCheck.TcHsType
import Eta.TypeCheck.TcExpr
import Eta.TypeCheck.TcEnv
import Eta.TypeCheck.TcType
-- import Eta.Prelude.TysWiredIn (unitTyCon)
import Eta.Prelude.PrelNames
import Eta.Prelude.ForeignCall
import Eta.Main.Hooks
import Eta.Main.ErrUtils
import Eta.Main.DynFlags
import Eta.Types.FamInstEnv
import Eta.Types.Type
import Eta.Types.TypeRep
import Eta.Types.Coercion
import Eta.Types.TyCon
import Eta.Debug
import Eta.HsSyn.HsSyn
import Eta.Utils.Bag
import Eta.Utils.Outputable
import Eta.Utils.FastString
-- import Eta.Utils.Maybes

-- import Data.Maybe(fromMaybe)

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

-- printDebug h s = do
--   dflags <- getDynFlags
--   liftIO . putStrLn . showSDoc dflags $ (ptext $ sLit h) <+> s

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
          | null tys  = nothing
          -- We add this case to support nested type families.
          | otherwise =
            let (co, tys') = normaliseTcArgs env Representational tc tys
            in return (co, mkTyConApp tc tys', emptyBag)
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
tcCheckFIType thetaType argTypes resType idecl@(CImport (L _lc cconv) (L _ls safety) _mh
                                               targetSpec _src)
  | CFunction target <- targetSpec
  = case cconv of
      PrimCallConv -> do
        dflags <- getDynFlags
        checkTc (xopt LangExt.GHCForeignImportPrim dflags)
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
  | CWrapper _target _isAbstract <- targetSpec
  , JavaCallConv <- cconv
  = do
      -- TODO: Validate target
      _dflags <- getDynFlags
      --let javaClassVars = extendsVars thetaType
      -- TODO: Typecheck foreign wrappers properly
      -- checkForeignArgs (isFFIArgumentTy dflags safety javaClassVars) argTypes
      -- checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) resType
      return idecl
  | otherwise = pprPanic "tcCheckFIType: Unsupported calling convention." (ppr idecl)

--tcCheckFIType _ _ _ idecl = pprPanic "tcCheckFIType: Unsupported calling convention." (ppr idecl)

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
checkJavaTarget (StaticTarget importFS _ _)
  = case validationPair of
      (condition, msg)
        | condition == True -> return ()
        | otherwise         -> addErrTc msg

  where importString = unpackFS importFS
        importParts = words importString
        staticMethodExample =
          vcat [  str "For example, if you want to import the static method parseBoolean"
              <+> str "from java.lang.Boolean" <> comma
              <+> str "you must type \"@static java.lang.Boolean.parseBoolean\"" <> dot ]
        staticFieldExample =
          vcat [ str "For example, if you want to import the static field 'TRUE' from"
             <+> str "the class 'java.lang.Boolean',"
               , str "you must type \"@static @field java.lang.Boolean.TRUE\"" <> dot ]
        fieldExample =
          vcat [ str "For example, if you want to import the field 'x' from"
             <+> str "a class 'Point',"
               , str "you must type \"@field x\"" <> dot ]
        interfaceExample =
          vcat [ str "For example, if you want to import the interface method 'run' from"
             <+> str "the interface 'java.lang.Runnable',"
               , str "you must type \"@interface run\"" <> dot ]
        superExample =
          vcat [ str "For example, if you want to import the super method 'run' from"
             <+> str "the super 'java.lang.Runnable',"
               , str "you must type \"@super run\"" <> dot ]
        interfaceWrapperExample =
          vcat [ str "For example, if you want to wrap the interface method 'run' from"
             <+> str "the interface 'java.lang.Runnable',"
               , str "you must type \"@wrapper run\"" <> dot ]
        abstractWrapperExample =
          vcat [ str "For example, if you want to wrap the abstract method 'intValue' from"
             <+> str "the abstract class 'java.lang.Number',"
               , str "you must type \"@wrapper @abstract intValue\"" <> dot ]

        checkDotInStatic annotation argument partsRest example
          | '.' `elem` argument = exactlyOneArgument annotation partsRest example
          | otherwise = (False,
                         vcat [ str annotation <+> str "annotation should contain a fully qualified Java class name"
                             <> comma <+> str "but you have given" <+> quotes (str argument)
                              , example ])
        exactlyOneArgument annotation partsRest example =
          (length partsRest == 1,
            vcat [ str annotation <+> str "annotation should contain exactly one argument" <> comma
                <+> str "but you have given" <+> int (length partsRest) <> dot
                 , example ])

        validationPair
          | (keyword:partsRest) <- importParts
          , ('@':keywordRest) <- keyword
          = case keywordRest of
              "static"
                 | null partsRest -> exactlyOneArgument "@static" partsRest staticMethodExample
                 | ('@':secondKeyword):partsRest2 <- partsRest
                 -> if secondKeyword == "field"
                    then case partsRest2 of
                           argument : _partsRest3 ->
                             checkDotInStatic "@static @field" argument partsRest2 staticFieldExample
                           _ -> exactlyOneArgument "@static @field" partsRest2 staticFieldExample
                    else (False,
                          vcat [ str "@static @" <> str secondKeyword
                             <+> str "is not a valid annotiation."
                               , str "Perhaps you meant to write @static @field?"])
                 | argument:_partsRest2 <- partsRest
                 -> checkDotInStatic "@static" argument partsRest staticMethodExample
              "field" -> exactlyOneArgument "@field" partsRest fieldExample
              "interface" -> exactlyOneArgument "@interface" partsRest interfaceExample
              "super" -> exactlyOneArgument "@super" partsRest superExample
              "new" -> (length partsRest == 0,
                vcat [ str "@new" <+> str "annotation should not contain any argument" <> comma
                    <+> str "but you have given" <+> int (length partsRest) <> dot ])
              "wrapper"
                  | ('@':secondKeyword):partsRest2 <- partsRest
                  -> if secondKeyword == "abstract"
                     then exactlyOneArgument "@wrapper @abstract" partsRest2 abstractWrapperExample
                     else (False,
                           vcat [ str "@wrapper @" <> str secondKeyword
                              <+> str "is not a valid annotiation."
                                , str "Perhaps you meant to write @wrapper @abstract?"])
                  | otherwise
                  -> exactlyOneArgument "@wrapper" partsRest interfaceWrapperExample
              _ -> (True, empty)
          | otherwise = (True, empty)
checkJavaTarget _ = error $ "checkJavaTarget: bad arguments"

-- isAnyTy :: Type -> Bool
-- isAnyTy = isTc anyTyConKey

-- isTc :: Unique -> Type -> Bool
-- isTc uniq ty = case tcSplitTyConApp_maybe ty of
--   Just (tc, _) -> uniq == getUnique tc
--   Nothing      -> False

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
          let binds'
                | Just b' <- b = b' `consBag` binds
                | otherwise    = binds
          return (binds', L loc f : fs, gres1 `unionBags` gres2)

tcFExport :: ForeignDecl Name -> TcM (Maybe (LHsBind Id), ForeignDecl Id, Bag GlobalRdrElt)
tcFExport fo@(ForeignExport (L loc nm) hs_ty _ spec)
  = addErrCtxt (foreignDeclCtxt fo) $ do
      sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
      rhs <- if isSuper
             then return Nothing
             else fmap Just $ tcPolyExpr (nlHsVar nm) sig_ty
      (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
      spec' <- tcCheckFEType norm_sig_ty spec
      id  <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
      return (fmap (mkVarBind id) rhs, ForeignExport (L loc id) undefined norm_co spec', gres)
  where isSuper = isForeignExportSuper spec

tcFExport d = pprPanic "tcFExport" (ppr d)

tcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
tcCheckFEType sigType exportspec = do
-- (CExport (L l (CExportStatic str cconv)) src)
    checkForeignArgs (isFFIExternalTy javaClassVars) argTypes
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy resType
    return exportspec
  where (_, ty)             = tcSplitForAllTys sigType
        (thetaType, ty')    = tcSplitPhiTy ty
        (argTypes, resType) = tcSplitFunTys ty'
        javaClassVars       = extendsVars thetaType
