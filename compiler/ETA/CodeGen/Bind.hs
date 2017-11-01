{-# LANGUAGE OverloadedStrings, NamedFieldPuns, MultiWayIf #-}
module ETA.CodeGen.Bind where

import ETA.StgSyn.StgSyn
import ETA.Core.CoreSyn
import ETA.BasicTypes.Id
import ETA.BasicTypes.BasicTypes
import ETA.BasicTypes.VarEnv
import ETA.Utils.Util (unzipWith)
import ETA.Types.TyCon
import ETA.CodeGen.ArgRep
import ETA.CodeGen.Constr
import ETA.CodeGen.Types
import ETA.CodeGen.Monad
import ETA.CodeGen.Rts
import ETA.CodeGen.Env
import ETA.CodeGen.Expr
import ETA.CodeGen.Name
import ETA.CodeGen.Layout
import ETA.CodeGen.Closure
import ETA.Debug
import ETA.Util
import Data.Maybe (catMaybes)
import ETA.Main.Constants
import Codec.JVM
import Control.Monad (forM, foldM, when)
import Data.Text (unpack)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.List(delete, find, foldl', sortOn)

closureCodeBody
  :: Bool                  -- whether this is a top-level binding
  -> Id                    -- the closure's name
  -> LambdaFormInfo        -- Lots of information about this closure
  -> [NonVoid Id]          -- incoming args to the closure
  -> Maybe FunRecInfo      -- Recursive ids for functions
  -> Int                   -- arity, including void args
  -> StgExpr               -- body
  -> [NonVoid Id]          -- the closure's free vars
  -> Bool                  -- Is one of the free vars itself?
  -> [Id]                  -- For a recursive block, the ids of the other
                           -- closures in the group.
  -> CodeGen ([FieldType], RecIndexes)
closureCodeBody _ id lfInfo args mFunRecIds arity body fvs binderIsFV recIds = do
  dflags <- getDynFlags
  traceCg $ str $ "creating new closure..." ++ unpack (idNameText dflags id)
  setClosureClass $ idNameText dflags id
  thisClass <- getClass
  let hasStdLayout
        | all (== P) $ map (idArgRep . unsafeStripNV) fvs
        = lfStandardForm arity (length fvs)
        | otherwise = False
  (fvLocs', initCodes, recIndexes) <- generateFVs (not hasStdLayout) fvs recIds
  let fvLocs = if binderIsFV
               then (NonVoid id, mkLocLocal True thisFt 0) : fvLocs'
               else fvLocs'
      thisFt = obj thisClass
      (_, fts, _) = unzip3 initCodes
      (codes, _) = foldl' (\(initCode, n) (_, ft, code) ->
                            ( initCode
                            <> gload thisFt 0
                            <> gload ft n
                            <> code
                            , n + fieldSize ft
                            )
                          )
                          (mempty, 1) initCodes

  setSuperClass (lfClass hasStdLayout arity (length fvs) lfInfo)
  if arity == 0 then
    -- TODO: Implement eager blackholing
    withMethod [Public, Final] "thunkEnter" [contextType] (ret closureType) $ do
      mapM_ bindFV fvLocs
      cgExpr body
  else do
    let mCallPattern = lfCallPattern lfInfo
    when (arity > 6) $
      defineMethod $ mkMethodDef thisClass [Public] "arity" [] (ret jint)
                   $ iconst jint (fromIntegral arity)
                  <> greturn jint
    modClass <- getModClass
    if | Just (arity, fts) <- mCallPattern -> do
         withMethod [Public, Final] "enter" [contextType] (ret closureType) $ do
           argLocs <- mapM newIdLoc args
           loadContext <- getContextLoc
           emit $ gload thisFt 0
               <> loadContext
               <> mkCallEntry loadContext False False argLocs
               <> mkApFast arity thisClass (contextType:fts)
               <> greturn closureType
         withMethod [Public, Final] (mkApFun arity fts) (contextType:fts) (ret closureType) $ do
           let argLocs = argLocsFrom 2 args
           loadContext <- getContextLoc
           case mFunRecIds of
             Just (n, funRecIds)
               | Just (target, loadCode, allArgFts) <-
                   funRecIdsInfo loadContext True argLocs id funRecIds ->
               emit $ aconst_null closureType
                   <> loadContext
                   <> iconst jint (fromIntegral target)
                   <> loadCode
                   <> invokestatic (mkMethodRef modClass (mkRecBindingMethodName n)
                                       ([closureType, contextType, jint] ++ allArgFts)
                                       (ret closureType))
                   <> greturn closureType
             _ -> do
               bindArgs $ zip args argLocs
               label <- newLabel
               emit $ startLabel label
               withSelfLoop (id, label, argLocs) $ do
                 mapM_ bindFV fvLocs
                 cgExpr body
       | otherwise ->
         withMethod [Public, Final] "enter" [contextType] (ret closureType) $ do
           loadContext <- getContextLoc
           case mFunRecIds of
             Just (n, funRecIds)
               | let argLocs = argLocsFrom 2 args
               , Just (target, loadCode, allArgFts) <-
                   funRecIdsInfo loadContext False argLocs id funRecIds ->
               emit $ aconst_null closureType
                   <> loadContext
                   <> iconst jint (fromIntegral target)
                   <> loadCode
                   <> invokestatic (mkMethodRef modClass (mkRecBindingMethodName n)
                                       ([closureType, contextType, jint] ++ allArgFts)
                                       (ret closureType))
                   <> greturn closureType
             _ -> do
               argLocs <- mapM newIdLoc args
               emit $ mkCallEntry loadContext True True argLocs
               bindArgs $ zip args argLocs
               label <- newLabel
               emit $ startLabel label
               withSelfLoop (id, label, argLocs) $ do
                 mapM_ bindFV fvLocs
                 cgExpr body

  superClass <- getSuperClass
  -- Generate constructor
  defineMethod . mkMethodDef thisClass [Public] "<init>" fts void $
    if hasStdLayout
    then ( gload thisFt 0
        <> fold (map (uncurry $ flip gload)
                 $ zip (scanl (\n ft -> n + fieldSize ft) 1 fts) fts)
        <> invokespecial (mkMethodRef superClass "<init>" fts void)
        <> vreturn)
    else ( gload thisFt 0
        <> invokespecial (mkMethodRef superClass "<init>" [] void)
        <> codes
        <> vreturn)
  return (fts, recIndexes)

funRecIdsInfo :: Code -> Bool -> [CgLoc] -> Id -> FunRecMap -> Maybe (Int, Code, [FieldType])
funRecIdsInfo loadContext stdLayout argLocs funId funRecInfoMap
  | Just (target, _argFts) <- lookupVarEnv funRecInfoMap funId
  , let indexedArgFts = sortOn fst $ varEnvElts funRecInfoMap
        (code, fts) = foldl' (\(!code, !allFts) (i, fts) ->
                          (code <>
                           (if i == target
                            then (if stdLayout
                                  then foldMap loadLoc argLocs
                                  else mkCallEntry loadContext True False argLocs)
                            else foldMap defaultValue fts)
                          ,allFts ++ fts))
                        (mempty, []) indexedArgFts
  = Just (target, code, fts)
  | otherwise = Nothing

funRecIdsArgFts :: FunRecMap -> [FieldType]
funRecIdsArgFts = concatMap snd . sortOn fst . varEnvElts

generateFVs :: Bool -> [NonVoid Id] -> [Id]
            -> CodeGen ( [(NonVoid Id, CgLoc)]
                       , [(Int, FieldType, Code)]
                       , RecIndexes )
generateFVs defineFields fvs recIds = do
  clClass <- getClass
  when (not (null fvs)) $
    traceCg $ str "Free variables " <+> hcat (punctuate comma (map ppr fvs))
  result <- forM (indexList nonVoidFvs) $ \(i, (nvId@(NonVoid id), rep)) -> do
    let ft = expectJust "generateFVs" $ primRepFieldType_maybe rep
        fieldName = constrField i
        code = putfield $ mkFieldRef clClass fieldName ft
        recIndex = if id `elem` recIds then Just (i, id) else Nothing
    -- TODO: Find a better way to handle recursion
    --       that allows us to use 'final' in most cases.
    when defineFields $ defineField $ mkFieldDef [Public] fieldName ft
    return ((nvId, LocField (isGcPtrRep rep) ft clClass fieldName), (i, ft, code), recIndex)
  let (fvLocs, initCodes, recIndexes) = unzip3 result
  return (fvLocs, initCodes, catMaybes recIndexes)
  where nonVoidFvs = map addFt fvs
        addFt nvFV@(NonVoid fv) = (nvFV, rep)
          where rep = idPrimRep fv

bindFV :: (NonVoid Id, CgLoc) -> CodeGen ()
bindFV (id, cgLoc)= rebindId id cgLoc

cgBind :: StgBinding -> CodeGen ()
cgBind (StgNonRec name rhs) = do
  traceCg $ str "StgLet" <+> ppr name
  (info, genInitCode) <- cgRhs [] name rhs
  addBinding info
  (init, recIndexes, ft) <- genInitCode
  emit init
  postInitCode <- postInitRecBinds name recIndexes ft
  emit postInitCode

cgBind (StgRec pairs) = do
  traceCg $ str "StgLet" <+> ppr recIds
  result <- sequence $ unzipWith (cgRhs recIds) pairs
  let (idInfos, genInitCodes) = unzip result
  addBindings idInfos
  (results, body) <- getCodeWithResult $ sequence genInitCodes
  let (inits, recIndexess, fts) = unzip3 results
  emit $ fold inits
  postInitCodes <- mapM (\(recId, recIndexes, ft) ->
                           postInitRecBinds recId recIndexes ft)
                   $ zip3 recIds recIndexess fts
  emit $ fold postInitCodes
  emit $ body
  where recIds = map fst pairs

cgRhs :: [Id] -> Id -> StgRhs -> CodeGen (CgIdInfo, CodeGen (Code, RecIndexes, FieldType))
cgRhs recIds id (StgRhsCon _ con args) = buildDynCon id con args (id:recIds)
cgRhs recIds name (StgRhsClosure _ binderInfo fvs updateFlag _ args body)
  = mkRhsClosure name binderInfo nonVoidFvs updateFlag args body recIds
  where nonVoidFvs = nonVoidIds fvs

mkRhsClosure
  :: Id
  -> StgBinderInfo
  -> [NonVoid Id]
  -> UpdateFlag
  -> [Id]
  -> StgExpr
  -> [Id]
  -> CodeGen (CgIdInfo, CodeGen (Code, RecIndexes, FieldType))
mkRhsClosure binder _ [NonVoid theFv] updateFlag [] expr recIds
  | StgCase (StgApp scrutinee [])
      _ _ _ _
      (AlgAlt _)
      [(DataAlt _, params, _, selExpr)] <- strip expr
  , StgApp selectee [] <- strip selExpr
  , theFv == scrutinee
  , let indexedReps = zip [1..] [(p, rep) | p <- params, let rep = idArgRep p, isNonV rep]
  , Just (index, (_, rep)) <- find (\(_, (p, _)) -> p == selectee) indexedReps
  = let lfInfo = mkSelectorLFInfo binder index rep (isUpdatable updateFlag)
    in cgRhsStdThunk binder lfInfo [StgVarArg theFv] recIds
  where strip = snd . stripStgTicksTop (not . tickishIsCode)

mkRhsClosure binder _ fvs updateFlag [] (StgApp funId args) recIds
  | length args == arity - 1
   && all (isGcPtrRep . idPrimRep . unsafeStripNV) fvs
   && isUpdatable updateFlag
   && arity <= mAX_SPEC_AP_SIZE
  = cgRhsStdThunk binder lfInfo payload recIds
  where lfInfo = mkApLFInfo binder updateFlag arity
        payload = StgVarArg funId : args
        arity = length fvs

mkRhsClosure binder _ fvs updateFlag args body recIds = do
  let lfInfo = mkClosureLFInfo binder NotTopLevel fvs updateFlag args
  (idInfo, cgLoc) <- rhsIdInfo binder lfInfo
  return (idInfo, genCode lfInfo cgLoc)
  where genCode lfInfo cgLoc = do
          ((fields, recIndexes), CgState { cgClassName }) <- forkClosureBody $
            closureCodeBody False binder lfInfo (nonVoidIds args) Nothing (length args)
                            body reducedFVs binderIsFV recIds

          loads <- forM reducedFVs $ \(NonVoid id) ->
            if id `elem` recIds
            then return $ aconst_null closureType -- NOTE: Assumed only closure types
            else do idInfo <- getCgIdInfo id
                    return $ idInfoLoadCode idInfo

          let ft = obj cgClassName
              closureCode =
                  new ft
               <> dup ft
               <> fold loads
               <> invokespecial (mkMethodRef cgClassName "<init>" fields void)
          return (mkRhsInit cgLoc closureCode, recIndexes, ft)
          where nvBinder = NonVoid binder
                binderIsFV = nvBinder `elem` fvs
                reducedFVs
                  | binderIsFV = delete nvBinder fvs
                  | otherwise = fvs

cgRhsStdThunk :: Id
              -> LambdaFormInfo
              -> [StgArg]
              -> [Id]
              -> CodeGen ( CgIdInfo
                         , CodeGen (Code, RecIndexes, FieldType) )
cgRhsStdThunk binder lfInfo payload recIds = do
  let (ft, genThunk) = genStdThunk lfInfo
  (idInfo, cgLoc) <- rhsGenIdInfo binder lfInfo ft
  traceCg $ str "cgRhsStdThunk:" <+> ppr idInfo <+> ppr cgLoc <+> ppr binder <+> ppr payload
  return (idInfo, genCode cgLoc genThunk ft)
  where genCode cgLoc genThunk ft = do
          (recIndexes, loads) <- foldM foldLoads ([], mempty) $ indexList payload
          let thunkInitCode = genThunk loads
          return (mkRhsInit cgLoc thunkInitCode, recIndexes, ft)

        -- TODO: Generalize to accommodate DynCons as well
        foldLoads (is, code) (i, arg)
          | StgVarArg id <- arg
          , id `elem` recIds
          = return ((i, id):is, code <> aconst_null closureType)
          | otherwise = do
              loadCode <- getArgLoadCode (NonVoid arg)
              return (is, code <> loadCode)

postInitRecBinds :: Id -> RecIndexes -> FieldType -> CodeGen Code
postInitRecBinds _binder [] _ft = return mempty
postInitRecBinds binder recIndexes ft = do
  CgIdInfo { cgLocation } <- getCgIdInfo binder
  let binderLoad = loadLoc cgLocation
      clClass = getFtClass ft
  recInitCodes <- forM recIndexes $ \(i, recId) -> do
    CgIdInfo { cgLocation } <- getCgIdInfo recId
    let recLoad = loadLoc cgLocation
    -- NOTE: We assume that all recursive free vars are closures, which should be
    --       a reasonable assumption. Verify.
    return $ recLoad <> putfield (mkFieldRef clClass (constrField i) closureType)
  return $ binderLoad
        <> fold (map (dup ft <>) recInitCodes)
        <> pop ft
