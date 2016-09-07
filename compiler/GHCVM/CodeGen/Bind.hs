{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module GHCVM.CodeGen.Bind where

import GHCVM.StgSyn.StgSyn
import GHCVM.Core.CoreSyn
import GHCVM.BasicTypes.Id
import GHCVM.Utils.Util (unzipWith)
import GHCVM.Types.TyCon
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Con
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Expr
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Closure
import GHCVM.Debug
import GHCVM.Util

import GHCVM.Main.Constants
import Codec.JVM
import Control.Monad (forM, foldM)
import Data.Text (append, pack, unpack)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe, maybe, catMaybes)
import Data.Monoid ((<>))
import Data.List(delete, find, foldl')

closureCodeBody
  :: Bool                  -- whether this is a top-level binding
  -> Id                    -- the closure's name
  -> LambdaFormInfo        -- Lots of information about this closure
  -> [NonVoid Id]          -- incoming args to the closure
  -> Int                   -- arity, including void args
  -> StgExpr               -- body
  -> [NonVoid Id]          -- the closure's free vars
  -> Bool                  -- Is one of the free vars itself?
  -> [Id]                  -- For a recursive block, the ids of the other
                           -- closures in the group.
  -> CodeGen ([FieldType], RecIndexes)
closureCodeBody topLevel id lfInfo args arity body fvs binderIsFV recIds = do
  dflags <- getDynFlags
  debug $ "creating new closure..." ++ unpack (idNameText dflags id)
  setClosureClass $ idNameText dflags id
  thisClass <- getClass
  (fvLocs', initCodes, recIndexes) <- generateFVs fvs recIds
  let fvLocs = if binderIsFV
               then (NonVoid id, mkLocLocal True thisFt 0) : fvLocs'
               else fvLocs'
      thisFt = obj thisClass
      (_, fts, _) = unzip3 initCodes
      (codes, _) = foldl' (\(initCode, n) (i, ft, code) ->
                            ( initCode
                            <> gload thisFt 0
                            <> gload ft n
                            <> code
                            , n + fieldSize ft
                            )
                          )
                          (mempty, 1) initCodes

  if arity == 0 then
    thunkCode lfInfo fvLocs body
  else do
    setSuperClass stgFun
    defineMethod $ mkMethodDef thisClass [Public] "getArity" [] (ret jint)
                 $  iconst jint (fromIntegral arity)
                 <> greturn jint
    withMethod [Public] "enter" [contextType] void $ do
      n <- peekNextLocal
      let (argLocs, code, n') = mkCallEntry n args
          (_ , cgLocs) = unzip argLocs
      emit code
      setNextLocal n'
      bindArgs argLocs
      label <- newLabel
      emit $ startLabel label
      withSelfLoop (id, label, cgLocs) $ do
        mapM_ bindFV fvLocs
        cgExpr body
    return ()
  superClass <- getSuperClass
  -- Generate constructor
  defineMethod . mkMethodDef thisClass [Public] "<init>" fts void $
      gload thisFt 0
   <> invokespecial (mkMethodRef superClass "<init>" [] void)
   <> codes
   <> vreturn
  return (fts, recIndexes)

generateFVs :: [NonVoid Id] -> [Id]
            -> CodeGen ( [(NonVoid Id, CgLoc)]
                       , [(Int, FieldType, Code)]
                       , RecIndexes )
generateFVs fvs recIds = do
  clClass <- getClass
  result <- forM (indexList nonVoidFvs) $ \(i, (nvId@(NonVoid id), rep)) -> do
    let ft = expectJust "generateFVs" $ primRepFieldType_maybe rep
        fieldName = constrField i
        code = putfield $ mkFieldRef clClass fieldName ft
        recIndex = if id `elem` recIds then Just (i, id) else Nothing
    defineField $ mkFieldDef [Public, Final] fieldName ft
    return ((nvId, LocField (isGcPtrRep rep) ft clClass fieldName), (i, ft, code), recIndex)
  let (fvLocs, initCodes, recIndexes) = unzip3 result
  return (fvLocs, initCodes, catMaybes recIndexes)
  where nonVoidFvs = map addFt fvs
        addFt nvFV@(NonVoid fv) = (nvFV, rep)
          where rep = idPrimRep fv

-- TODO: Implement eager blackholing
thunkCode :: LambdaFormInfo -> [(NonVoid Id, CgLoc)] -> StgExpr -> CodeGen ()
thunkCode lfInfo fvs body =
  setupUpdate lfInfo $ do
    mapM_ bindFV fvs
    cgExpr body

bindFV :: (NonVoid Id, CgLoc) -> CodeGen ()
bindFV (id, cgLoc)= rebindId id cgLoc

setupUpdate :: LambdaFormInfo -> CodeGen () -> CodeGen ()
setupUpdate lfInfo body
  -- ASSERT lfInfo is of form LFThunk
  | not (lfUpdatable lfInfo) = withEnterMethod stgThunk "enter"
  | lfStaticThunk lfInfo = withEnterMethod stgIndStatic "thunkEnter"
  | otherwise = withEnterMethod stgInd "thunkEnter"
  where withEnterMethod thunkType name = do
          setSuperClass thunkType
          withMethod [Public] "thunkEnter" [contextType] void body
          return ()

cgBind :: StgBinding -> CodeGen ()
cgBind (StgNonRec name rhs) = do
  debugDoc $ str "StgLet" <+> ppr name
  (info, genInitCode) <- cgRhs [] name rhs
  addBinding info
  (init, recIndexes) <- genInitCode
  emit init
  postInitCode <- postInitRecBinds name recIndexes
  emit postInitCode

cgBind (StgRec pairs) = do
  debugDoc $ str "StgLet" <+> ppr recIds
  result <- sequence $ unzipWith (cgRhs recIds) pairs
  let (idInfos, genInitCodes) = unzip result
  addBindings idInfos
  (results, body) <- getCodeWithResult $ sequence genInitCodes
  let (inits, recIndexess) = unzip results
  emit $ fold inits
  postInitCodes <- mapM (\(recId, recIndexes) -> postInitRecBinds recId recIndexes)
                   $ zip recIds recIndexess
  emit $ fold postInitCodes
  -- TODO: What exactly is the body?
  emit $ body
  where recIds = map fst pairs

cgRhs :: [Id] -> Id -> StgRhs -> CodeGen (CgIdInfo, CodeGen (Code, RecIndexes))
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
  -> CodeGen (CgIdInfo, CodeGen (Code, RecIndexes))
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
            closureCodeBody False binder lfInfo (nonVoidIds args) (length args)
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
          return (mkRhsInit cgLoc closureCode, recIndexes)
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
                         , CodeGen (Code, RecIndexes) )
cgRhsStdThunk binder lfInfo payload recIds = do
  let (ft, genThunk) = genStdThunk lfInfo
  (idInfo, cgLoc) <- rhsGenIdInfo binder lfInfo ft
  debugDoc $ str "cgRhsStdThunk:" <+> ppr idInfo <+> ppr cgLoc <+> ppr binder <+> ppr payload
  return (idInfo, genCode cgLoc genThunk)
  where genCode cgLoc genThunk = do
          (recIndexes, loads) <- foldM foldLoads ([], mempty) $ indexList payload
          let thunkInitCode = genThunk loads
          return (mkRhsInit cgLoc thunkInitCode, recIndexes)

        -- TODO: Generalize to accommodate DynCons as well
        foldLoads (is, code) (i, arg)
          | StgVarArg id <- arg
          , id `elem` recIds
          = return ((i, id):is, code <> aconst_null closureType)
          | otherwise = do
              loadCode <- getArgLoadCode (NonVoid arg)
              return (is, code <> loadCode)

postInitRecBinds :: Id -> RecIndexes -> CodeGen Code
postInitRecBinds binder [] = return mempty
postInitRecBinds binder recIndexes = do
  CgIdInfo { cgLocation } <- getCgIdInfo binder
  let binderLoad = loadLoc cgLocation
      ft = locFt cgLocation
      clClass = getFtClass (locFt cgLocation)
  recInitCodes <- forM recIndexes $ \(i, recId) -> do
    CgIdInfo { cgLocation } <- getCgIdInfo recId
    let recLoad = loadLoc cgLocation
    -- NOTE: We assume that all recursive free vars are closures, which should be
    --       a reasonable assumption. Verify.
    return $ recLoad <> putfield (mkFieldRef clClass (constrField i) closureType)
  return $ binderLoad
        <> fold (map (dup ft <>) recInitCodes)
        <> pop ft
