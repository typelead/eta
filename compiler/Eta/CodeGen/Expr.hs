module Eta.CodeGen.Expr where

import Eta.Core.CoreSyn
import Eta.Types.Type
import Eta.Types.TyCon
import Eta.BasicTypes.Id
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.SrcLoc
import Eta.Prelude.PrimOp
import Eta.StgSyn.StgSyn
import Eta.BasicTypes.DataCon
import Eta.Utils.Panic
import Eta.Utils.Util
import Eta.Utils.FastString (unpackFS)
import Eta.Debug
import Eta.CodeGen.Utils
import Eta.CodeGen.Monad
import Eta.CodeGen.Name
import Eta.CodeGen.Layout
import Eta.CodeGen.Types
import Eta.CodeGen.Closure
import Eta.CodeGen.Env
import Eta.CodeGen.Rts
import Eta.CodeGen.Constr
import Eta.CodeGen.Prim
import Eta.CodeGen.ArgRep
import {-# SOURCE #-} Eta.CodeGen.Bind (cgBind)
import Codec.JVM

import System.FilePath (equalFilePath)
import Data.Monoid((<>))
import qualified Data.Set as Set
import Data.Foldable(fold)
import Data.List(zip4)
import Data.Maybe(mapMaybe)
import Control.Monad(when, forM_, unless)

cgExpr :: StgExpr -> CodeGen ()
cgExpr (StgApp fun args) = do
  traceCg (str "StgApp" <+> ppr fun <+> ppr args)
  cgIdApp fun args
cgExpr (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) = do
  cgIdApp a []
cgExpr (StgOpApp op args ty) = do
  traceCg (str "StgOpApp" <+> ppr op <+> ppr args <+> ppr ty)
  cgOpApp op args ty
cgExpr (StgConApp con args) = do
  traceCg (str "StgConApp" <+> ppr con <+> ppr args)
  cgConApp con args
cgExpr t@(StgTick _ _) = do
  cgTick t

cgExpr (StgLit lit) = do
  emitReturn [mkLocDirect False $ cgLit lit]
cgExpr (StgLet binds expr) = do
  forbidScoping (cgBind binds)
  cgExpr expr
cgExpr (StgLetNoEscape _ _ binds expr) = do
  cgLneBinds binds expr

cgExpr (StgCase expr _ _ binder _ altType alts) = do
  traceCg (str "StgCase" <+> ppr binder <+> ppr altType)
  cgCase expr binder altType alts
cgExpr _ = unimplemented "cgExpr"

cgLneBinds :: StgBinding -> StgExpr -> CodeGen ()
cgLneBinds (StgNonRec binder rhs) expr = do
  -- TODO: Optimize this to calculate the smallest type that can hold the target
  label <- newLabel
  targetLoc <- newTemp False jint
  (info, genBindCode) <- cgLetNoEscapeRhsBody binder rhs label 1 targetLoc
  n' <- peekNextLocal
  bindCode <- genBindCode n'
  addBinding info
  exprCode <- forkLneBody $ cgExpr expr
  let (bindLabel, argLocs) = expectJust "cgLneBinds:StgNonRec"
                                        . maybeLetNoEscape $ info
  emit $ storeDefault targetLoc
      <> fold (map storeDefault argLocs)
      <> letNoEscapeCodeBlocks label targetLoc [(bindLabel, bindCode)] exprCode

cgLneBinds (StgRec pairs) expr = do
  label <- newLabel
  targetLoc <- newTemp False jint
  let lneInfos = snd $ foldr (\(a,b) (n,xs) -> (n - 1, (a,b,label,n,targetLoc):xs))
                             (length pairs, []) pairs

  result <- sequence $ map (\(a,b,c,d,e) -> cgLetNoEscapeRhsBody a b c d e) lneInfos
  n' <- peekNextLocal
  let (infos, genBindCodes) = unzip result
      (labels, argLocss) = unzip $ map (expectJust "cgLneBinds:StgRec"
                                       . maybeLetNoEscape) infos
  addBindings infos
  bindCodes <- sequence $ ($ n') <$> genBindCodes
  exprCode <- forkLneBody $ cgExpr expr
  emit $ storeDefault targetLoc
      <> fold (fold (map (map storeDefault) argLocss))
      <> letNoEscapeCodeBlocks label targetLoc (zip labels bindCodes) exprCode

cgLetNoEscapeRhsBody :: Id -> StgRhs -> Label -> Int -> CgLoc -> CodeGen (CgIdInfo, Int -> CodeGen Code)
cgLetNoEscapeRhsBody binder (StgRhsClosure _ _ _ _ _ args body) jumpLabel target targetLoc
  = cgLetNoEscapeClosure binder (nonVoidIds args) body jumpLabel target targetLoc
cgLetNoEscapeRhsBody binder (StgRhsCon _ con args) jumpLabel target targetLoc
  = cgLetNoEscapeClosure binder [] (StgConApp con args) jumpLabel target targetLoc

cgLetNoEscapeClosure
  :: Id -> [NonVoid Id] -> StgExpr -> Label -> Int -> CgLoc -> CodeGen (CgIdInfo, Int -> CodeGen Code)
cgLetNoEscapeClosure binder args body label target targetLoc = do
  argLocs <- mapM newIdLoc args
  let code n' = forkLneBody $ do
        bindArgs $ zip args argLocs
        setNextLocal n'
        cgExpr body
  return (lneIdInfo binder label target targetLoc argLocs, code)

letNoEscapeCodeBlocks :: Label -> CgLoc -> [(Int, Code)] -> Code -> Code
letNoEscapeCodeBlocks label targetLoc labelledCode body =
     startLabel label
  <> intSwitch (loadLoc targetLoc) labelledCode (Just body)

maybeLetNoEscape :: CgIdInfo -> Maybe (Int, [CgLoc])
maybeLetNoEscape CgIdInfo { cgLocation = LocLne _label target _targetLoc argLocs }
  = Just (target, argLocs)
maybeLetNoEscape _ = Nothing

cgIdApp :: Id -> [StgArg] -> CodeGen ()
cgIdApp funId [] | isVoidTy (idType funId) = emitReturn []
cgIdApp funId args = do
  dflags       <- getDynFlags
  funInfo      <- getCgIdInfo funId
  selfLoopInfo <- getSelfLoop
  scoped       <- getScopedBindings
  allowScoping <- getAllowScoping
  let cgFunId = cgId funInfo
      funName = idName cgFunId
      lfInfo = cgLambdaForm funInfo
      funLoc = cgLocation funInfo
      nArgs  = length args
      vArgs  = length $ filter (isVoidTy . stgArgType) args
      mScopeInfo
        | isEmptyVarEnv scoped || not allowScoping = Nothing
        | otherwise = fmap cgLocation $ lookupVarEnv scoped funId
  case getCallMethod dflags funName cgFunId lfInfo nArgs vArgs funLoc
                     selfLoopInfo mScopeInfo of
    ReturnIt -> traceCg (str "cgIdApp: ReturnIt") >>
                emitReturn [funLoc]
    EnterIt  -> traceCg (str "cgIdApp: EnterIt") >>
                emitEnter funLoc
    SlowCall -> do
      traceCg (str "cgIdApp: SlowCall")
      argFtCodes  <- getRepFtCodes args
      loadContext <- getContextLoc
      sequel      <- getSequel
      let tailCall
            | Return <- sequel = True
            | otherwise        = False
          (contCode, lastCode) = slowCall dflags tailCall loadContext funLoc argFtCodes
      withContinuation contCode lastCode
    DirectEntry nodeLoc arity -> do
      traceCg (str "cgIdApp: DirectEntry")
      argFtCodes  <- getRepFtCodes args
      loadContext <- getContextLoc
      sequel      <- getSequel
      let tailCall
            | Return <- sequel = True
            | otherwise        = False
          (contCode, lastCode) =
            directCall tailCall loadContext (idType funId) nodeLoc arity argFtCodes
      withContinuation contCode lastCode
    JumpToIt label cgLocs mLne -> do
      traceCg (str "cgIdApp: JumpToIt")
      deps <- dependencies $ zip (filter (not . isVoidRep . argPrimRep) args) cgLocs
      emitMultiAssign deps
      emit $ maybe  mempty
                      (\(target, targetLoc) ->
                       storeLoc targetLoc (iconst (locFt targetLoc) $ fromIntegral target))
                       mLne
                  <>   goto label

    where
        dependencies::[(StgArg, CgLoc)] -> CodeGen [Assignment]
        dependencies  [] =  pure []
        dependencies (arg:args) = dependencies args  >>=  joinDependency  arg

        joinDependency :: (StgArg, CgLoc) ->[Assignment] -> CodeGen [Assignment]
        joinDependency  x deps =
            joinSingle  deps  <$> dep
            where dep = dependency x

        joinSingle :: [Assignment]->Assignment->[Assignment]
        joinSingle  deps x = x : deps

        dependency :: (StgArg, CgLoc) -> CodeGen Assignment
        dependency (arg, loc) = getGetDepCgLoad (NonVoid arg, loc)

        getGetDepCgLoad :: (NonVoid StgArg, CgLoc) -> CodeGen Assignment
        getGetDepCgLoad (NonVoid (StgVarArg var), loc)
          = (\x -> AnyAssignment { from = Right x, to = loc })
          <$> cgLocation <$> getCgIdInfo var
        getGetDepCgLoad (NonVoid arg, loc) =
          (\x -> AnyAssignment { from = Left x, to = loc })
          <$> getArgLoadCode (NonVoid arg)


emitEnter :: CgLoc -> CodeGen ()
emitEnter thunk = do
  sequel <- getSequel
  loadContext <- getContextLoc
  case sequel of
    Return ->
      -- TODO: Better type information for evaluate
      emit $ evaluateMethod True loadContext thunk <> greturn closureType
    AssignTo cgLocs -> do
      emit $ evaluateMethod False loadContext thunk <> mkReturnEntry loadContext cgLocs

cgConApp :: DataCon -> [StgArg] -> CodeGen ()
cgConApp con args
  | isUnboxedTupleCon con = do
      repCodes <- getNonVoidArgRepCodes args
      emitReturn $ map mkRepLocDirect repCodes
  | otherwise = do
      (idInfo, genInitCode) <- buildDynCon (dataConWorkId con) con args []
      (initCode, _recIndexes, _dataFt) <- genInitCode
      emit initCode
      emitReturn [cgLocation idInfo]

cgCase :: StgExpr -> Id -> AltType -> [StgAlt] -> CodeGen ()
cgCase (StgOpApp (StgPrimOp op) args _) binder (AlgAlt tyCon) alts
  | isEnumerationTyCon tyCon = do
      dflags <- getDynFlags
      tagExpr <- doEnumPrimop op args
      let closureCode = snd $ tagToClosure dflags tyCon tagExpr
      loadCode <- if not $ isDeadBinder binder then do
          bindLoc <- newIdLoc (NonVoid binder)
          bindArg (NonVoid binder) bindLoc
          emitAssign bindLoc closureCode
          return $ loadLoc bindLoc
        else return closureCode
      (maybeDefault, branches) <- cgAlgAltRhss (NonVoid binder) alts
      emit $ intSwitch (getTagMethod loadCode) branches maybeDefault
  where doEnumPrimop :: PrimOp -> [StgArg] -> CodeGen Code
        doEnumPrimop TagToEnumOp [arg] =
          getArgLoadCode (NonVoid arg)
        doEnumPrimop primop args = do
          codes <- cgPrimOp primop args
          return $ head codes

cgCase (StgOpApp (StgPrimOp op) args _) binder (PrimAlt _) alts
  | op `elem` [FreshStateTokenOp, FreshObjectTokenOp, FreshNullObjectTokenOp]
  = case op of
      FreshStateTokenOp -> do
        let [(DEFAULT, _, _, rhs)] = alts
        cgExpr rhs
      FreshObjectTokenOp -> do
        let (_:obj:_) = args
            nvObj = NonVoid obj
            [(DEFAULT, _, _, rhs)] = alts
        objCode <- getArgLoadCode nvObj
        bindArg nvBinder (newLocDirect nvBinder objCode)
        cgExpr rhs
      FreshNullObjectTokenOp -> do
        let [(DEFAULT, _, _, rhs)] = alts
        bindArg nvBinder (newLocDirect nvBinder (aconst_null jobject))
        cgExpr rhs
      _ -> panic "cgCase: StgOpApp: This shouldn't be possible!"
  | isDeadBinder binder
  , Just genCode <- comparisonPrimOp op
  = genCode nvBinder args alts
  where nvBinder = NonVoid binder

cgCase (StgApp v []) _ (PrimAlt _) alts
  | isVoidRep (idPrimRep v)
  , [(DEFAULT, _, _, rhs)] <- alts
  = cgExpr rhs

cgCase (StgApp v []) binder altType@(PrimAlt _) alts
  | isUnLiftedType (idType v)
  || repsCompatible
  = do
      unless repsCompatible $
        panic "cgCase: reps do not match, perhaps a dodgy unsafeCoerce?"
      vInfo <- getCgIdInfo v
      binderLoc <- newIdLoc nvBinder
      emitAssign binderLoc (idInfoLoadCode vInfo)
      bindArgs [(nvBinder, binderLoc)]
      cgAlts nvBinder altType alts
  where repsCompatible = vRep == idPrimRep binder
        -- TODO: Allow integer conversions?
        nvBinder = NonVoid binder
        vRep = idPrimRep v

cgCase scrut@(StgApp v []) _ (PrimAlt _) _ = do
  cgLoc <- newIdLoc (NonVoid v)
  withSequel (AssignTo [cgLoc]) $ forbidScoping $ cgExpr scrut
  panic "cgCase: bad unsafeCoerce!"
  -- TODO: Generate infinite loop here?

cgCase (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) binder altType alts
  = cgCase (StgApp a []) binder altType alts

cgCase scrut binder altType alts = do
  altLocs <- mapM newIdLoc retBinders
  -- Take into account uses for unboxed tuples
  -- Case-of-case expressions need special care
  -- Since there can be multiple bindings to return
  -- binder.
  when isCaseOfCase $ do
    emit $ fold (map storeDefault altLocs)
  let maybePreserve
        | isCaseOfCase = preserveCaseOfCase
        | otherwise    = id
  withSequel (AssignTo altLocs) $ maybePreserve $ forbidScoping $ cgExpr scrut
  bindArgs $ zip retBinders altLocs
  cgAlts (NonVoid binder) altType alts
  where retBinders = chooseReturnBinders binder altType alts
        isCaseOfCase = go scrut
        go (StgLet _ e) = go e
        go (StgTick _ e) = go e
        go (StgCase _ _ _ _ _ _ _) = True
        go _ = False

chooseReturnBinders :: Id -> AltType -> [StgAlt] -> [NonVoid Id]
chooseReturnBinders binder (PrimAlt _) _ = nonVoidIds [binder]
chooseReturnBinders _ (UbxTupAlt _) [(_, ids, _, _)] = nonVoidIds ids
chooseReturnBinders binder (AlgAlt _) _ = nonVoidIds [binder]
chooseReturnBinders binder PolyAlt _ = nonVoidIds [binder]
chooseReturnBinders _ _ _ = panic "chooseReturnBinders"

cgAlts :: NonVoid Id -> AltType -> [StgAlt] -> CodeGen ()
cgAlts _ PolyAlt [(_, _, _, rhs)] = cgExpr rhs
cgAlts _ (UbxTupAlt _) [(_, _, _, rhs)] = cgExpr rhs
cgAlts binder (PrimAlt _) alts = do
  taggedBranches <- cgAltRhss binder alts
  binderLoc <- if null (nonVoidIds [unsafeStripNV binder]) then
                 return $ mkLocDirect False (jint, mempty)
               else
                 getCgLoc binder
  let (DEFAULT, deflt) = head taggedBranches
      taggedBranches' = [(lit, code) | (LitAlt lit, code) <- taggedBranches]
  emit $ litSwitch (locFt binderLoc) (loadLoc binderLoc) taggedBranches' deflt
cgAlts binder (AlgAlt _) alts = do
  (maybeDefault, branches) <- cgAlgAltRhssCons binder alts
  binderLoc <- getCgLoc binder
  dflags <- getDynFlags
  emit $ instanceofTree dflags (loadLoc binderLoc) branches maybeDefault
cgAlts _ _ _ = panic "cgAlts"

cgAltRhss :: NonVoid Id -> [StgAlt] -> CodeGen [(AltCon, Code)]
cgAltRhss binder alts =
  forkAlts $ map cgAlt alts
  where cgAlt :: StgAlt -> (AltCon, CodeGen ())
        cgAlt (con, binders, uses, rhs) =
          ( con
          ,  bindConArgs con binder binders uses
          >> cgExpr rhs )

bindConArgs :: AltCon -> NonVoid Id -> [Id] -> [Bool] -> CodeGen ()
bindConArgs (DataAlt con) binder args uses
  | not (null args'), or uses'
  , length indexedFields >= 1 = do
    dflags <- getDynFlags
    base <- getCgLoc binder
    let conClass = dataConClass dflags con
        dataFt   = obj conClass
        castCode = loadLoc base <> gconv conType dataFt
    if length indexedFields > 1
    then do
      dataLoc <- newTemp True dataFt
      emit $ storeLoc dataLoc castCode
      forM_ indexedFields $ \(i, ft, arg) ->
        bindFieldCgLoc arg $
          loadLoc dataLoc <> getfield (mkFieldRef conClass (constrField i) ft)
    else do
      let (i, ft, arg) = head indexedFields
      bindFieldCgLoc arg $
        castCode <> getfield (mkFieldRef conClass (constrField i) ft)
    where indexedFields = mapMaybe (\(i, mb, arg, use) ->
                                      case mb of
                                        Just m
                                          | use       -> Just (i, m, arg)
                                          | otherwise -> Nothing
                                        Nothing -> Nothing)
                        $ zip4 [1..] maybeFields' args' uses'
          (args', uses', maybeFields') = unzip3
                         $ filter (\(arg,_,_) -> not . isVoidRep $ idPrimRep arg)
                         $ zip3 args uses maybeFields
          maybeFields  = map repFieldType_maybe $ dataConRepArgTys con
          bindFieldCgLoc arg loadCode =
            getFieldCgLoc >>= bindArg nvArg
            where getFieldCgLoc
                    | idUsedOnce arg = return $ newLocDirect nvArg loadCode
                    | otherwise = do
                      cgLoc <- newIdLoc nvArg
                      emitAssign cgLoc loadCode
                      return cgLoc
                  nvArg = NonVoid arg
bindConArgs _ _ _ _ = return ()

cgAlgAltRhss :: NonVoid Id -> [StgAlt] -> CodeGen (Maybe Code, [(Int, Code)])
cgAlgAltRhss = cgAlgAltRhss' getDataConTag

cgAlgAltRhssCons :: NonVoid Id -> [StgAlt] -> CodeGen (Maybe Code, [(DataCon, Code)])
cgAlgAltRhssCons = cgAlgAltRhss' id

cgAlgAltRhss' :: (DataCon -> a) -> NonVoid Id -> [StgAlt] ->
                 CodeGen (Maybe Code, [(a, Code)])
cgAlgAltRhss' f binder alts = do
  taggedBranches <- cgAltRhss binder alts
  let (maybeDefault, branches) =
        case taggedBranches of
          ((DEFAULT, rhs) : _) -> (Just rhs, allBranches)
          {- INVARIANT: length alts > 0

             We select the *last* alternative since that will
             typically have a higher tag and will require more
             bytecode to perform the check if it's a simple
             case with 2 alternatives (the most frequent case). -}
          _                    -> (Just $ snd remainingBranch, remainingBranches)
      (remainingBranches, remainingBranch) = (init allBranches, last allBranches)
      allBranches = [ (f con, code)
                    | (DataAlt con, code) <- taggedBranches ]
  return (maybeDefault, branches)

cgTick :: StgExpr -> CodeGen ()
cgTick expr = do
  mbCgFilePath <- getSourceFilePath
  let srcLoc = realSrcSpanStart
      isModuleTick (SourceNote srcSpan _) =
          let srcFile = unpackFS $ srcLocFile $ srcLoc srcSpan
          in  maybe False (equalFilePath srcFile) mbCgFilePath
      isModuleTick _ = False
      (ticks,subExpr) = stripStgTicksTop (const True) expr
  forM_ ticks $ \ t -> traceCg (str "StgTick, tickish:" <+> ppr t)
  case reverse $ filter isModuleTick ticks of
    (SourceNote srcSpan _:_) -> do
      let n = srcLocLine $ srcLoc srcSpan
      traceCg (str $ "Emitting line number: " ++ show n)
      emit $ emitLineNumber $ mkLineNumber n
    _ -> return ()
  cgExpr subExpr

comparisonPrimOp :: PrimOp -> Maybe (NonVoid Id -> [StgArg] -> [StgAlt] -> CodeGen ())
comparisonPrimOp primop
  | Set.member primop opsSet
  = Just $ \binder args alts -> do
      [(_, falseBranch), (_, trueBranch)] <- cgAltRhss binder alts
      argExprs <- getNonVoidArgCodes args
      emit $ (cmpOp primop) argExprs trueBranch falseBranch
  | otherwise = Nothing
  where opsSet = Set.fromList comparisonOps
        comparisonOps = [ IntEqOp
                        , IntNeOp
                        , IntLeOp
                        , IntLtOp
                        , IntGeOp
                        , IntGtOp

                        , Int64Eq
                        , Int64Ne
                        , Int64Le
                        , Int64Lt
                        , Int64Ge
                        , Int64Gt

                        , WordEqOp
                        , WordNeOp
                        , WordLeOp
                        , WordLtOp
                        , WordGeOp
                        , WordGtOp

                        , Word64Eq
                        , Word64Ne
                        , Word64Le
                        , Word64Lt
                        , Word64Ge
                        , Word64Gt

                        , CharEqOp
                        , CharNeOp
                        , CharLeOp
                        , CharLtOp
                        , CharGeOp
                        , CharGtOp

                        , DoubleEqOp
                        , DoubleNeOp
                        , DoubleLeOp
                        , DoubleLtOp
                        , DoubleGeOp
                        , DoubleGtOp

                        , FloatEqOp
                        , FloatNeOp
                        , FloatLeOp
                        , FloatLtOp
                        , FloatGeOp
                        , FloatGtOp

                        , AddrEqOp
                        , AddrNeOp
                        , AddrLeOp
                        , AddrLtOp
                        , AddrGeOp
                        , AddrGtOp


                        , SameMutableArrayOp
                        , SameSmallMutableArrayOp
                        , SameMutableArrayArrayOp
                        , SameMutVarOp
                        , SameTVarOp
                        , SameMVarOp
                        , SameMutableByteArrayOp
                        , ReallyUnsafePtrEqualityOp

                        , EqStablePtrOp
                        , EqStableNameOp

                        ]
        cmpOp IntEqOp = liftNormalOp if_icmpeq
        cmpOp IntNeOp = liftNormalOp if_icmpne
        cmpOp IntLeOp = liftNormalOp if_icmple
        cmpOp IntLtOp = liftNormalOp if_icmplt
        cmpOp IntGeOp = liftNormalOp if_icmpge
        cmpOp IntGtOp = liftNormalOp if_icmpgt

        cmpOp Int64Eq = liftTypedCmpOp jlong ifeq
        cmpOp Int64Ne = liftTypedCmpOp jlong ifne
        cmpOp Int64Le = liftTypedCmpOp jlong ifle
        cmpOp Int64Lt = liftTypedCmpOp jlong iflt
        cmpOp Int64Ge = liftTypedCmpOp jlong ifge
        cmpOp Int64Gt = liftTypedCmpOp jlong ifgt

        cmpOp WordEqOp = liftNormalOp if_icmpeq
        cmpOp WordNeOp = liftNormalOp if_icmpne
        cmpOp WordLeOp = liftUnsignedOp ifle
        cmpOp WordLtOp = liftUnsignedOp iflt
        cmpOp WordGeOp = liftUnsignedOp ifge
        cmpOp WordGtOp = liftUnsignedOp ifgt

        cmpOp Word64Eq = liftTypedCmpOp jlong ifeq
        cmpOp Word64Ne = liftTypedCmpOp jlong ifne
        cmpOp Word64Lt = liftUnsignedLongCmp iflt
        cmpOp Word64Le = liftUnsignedLongCmp ifle
        cmpOp Word64Gt = liftUnsignedLongCmp ifgt
        cmpOp Word64Ge = liftUnsignedLongCmp ifge

        cmpOp CharEqOp = liftNormalOp if_icmpeq
        cmpOp CharNeOp = liftNormalOp if_icmpne
        cmpOp CharLeOp = liftUnsignedOp ifle
        cmpOp CharLtOp = liftUnsignedOp iflt
        cmpOp CharGeOp = liftUnsignedOp ifge
        cmpOp CharGtOp = liftUnsignedOp ifgt

        cmpOp DoubleEqOp = liftNormalOp if_dcmpeq
        cmpOp DoubleNeOp = liftNormalOp if_dcmpne
        cmpOp DoubleGeOp = liftNormalOp if_dcmpge
        cmpOp DoubleLeOp = liftNormalOp if_dcmple
        cmpOp DoubleGtOp = liftNormalOp if_dcmpgt
        cmpOp DoubleLtOp = liftNormalOp if_dcmplt

        cmpOp FloatEqOp = liftNormalOp if_fcmpeq
        cmpOp FloatNeOp = liftNormalOp if_fcmpne
        cmpOp FloatGeOp = liftNormalOp if_fcmpge
        cmpOp FloatLeOp = liftNormalOp if_fcmple
        cmpOp FloatGtOp = liftNormalOp if_fcmpgt
        cmpOp FloatLtOp = liftNormalOp if_fcmplt

        cmpOp AddrEqOp = liftTypedCmpOp jlong ifeq
        cmpOp AddrNeOp = liftTypedCmpOp jlong ifne
        cmpOp AddrLeOp = liftTypedCmpOp jlong ifle
        cmpOp AddrLtOp = liftTypedCmpOp jlong iflt
        cmpOp AddrGeOp = liftTypedCmpOp jlong ifge
        cmpOp AddrGtOp = liftTypedCmpOp jlong ifgt

        cmpOp SameMutableArrayOp        = liftNormalOp if_acmpeq
        cmpOp SameSmallMutableArrayOp   = liftNormalOp if_acmpeq
        cmpOp SameMutableArrayArrayOp   = liftNormalOp if_acmpeq
        cmpOp SameMutVarOp              = liftNormalOp if_acmpeq
        cmpOp SameTVarOp                = liftNormalOp if_acmpeq
        cmpOp SameMVarOp                = liftNormalOp if_acmpeq
        cmpOp SameMutableByteArrayOp    = liftNormalOp if_acmpeq
        cmpOp ReallyUnsafePtrEqualityOp = liftNormalOp if_acmpeq

        cmpOp EqStablePtrOp             = liftNormalOp if_icmpeq
        cmpOp EqStableNameOp            = liftNormalOp if_icmpeq
        cmpOp op                        =
          pprPanic "comparisonPrimOp: Bad primop" (ppr op)
        liftNormalOp f = \args b1 b2 -> fold args <> f b1 b2
        liftUnsignedOp f = \args b1 b2 ->
          liftTypedCmpOp jlong f (map unsignedExtend args) b1 b2
        liftTypedCmpOp ft f = \[arg1, arg2] b1 b2 -> gcmp ft arg1 arg2 <> f b1 b2
        liftUnsignedLongCmp f = \args b1 b2 ->
          liftTypedCmpOp jlong f (map addMin args) b1 b2
