module GHCVM.CodeGen.Bind where

import StgSyn
import Id
import Util (unzipWith)
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
import GHCVM.Util
import GHCVM.Primitive
import GHCVM.Constants
import Codec.JVM
import Control.Monad (forM)
import Data.Text (append, pack)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe, maybe, fromJust)
import Data.Monoid ((<>))
import Data.List(delete)

closureCodeBody
  :: Bool                  -- whether this is a top-level binding
  -> Id                    -- the closure's name
  -> LambdaFormInfo        -- Lots of information about this closure
  -> [NonVoid Id]          -- incoming args to the closure
  -> Int                   -- arity, including void args
  -> StgExpr               -- body
  -> [NonVoid Id]                  -- the closure's free vars
  -> CodeGen [FieldType]
closureCodeBody topLevel id lfInfo args arity body fvs = do
  setClosureClass $ idNameText id
  (fvLocs, initCodes) <- generateFVs fvs
  thisClass <- getClass
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
  -- Generate constructor
  let thisFt = obj thisClass
  let (fts, _) = unzip initCodes
  let codes = flip concatMap (indexList initCodes) $ \(i, (ft, code)) ->
        [
          gload thisFt 0,
          gload ft i,
          code
        ]
  superClass <- getSuperClass
  defineMethod . mkMethodDef thisClass [Public] "<init>" fts void $ fold
    [
       gload thisFt 0,
       invokespecial $ mkMethodRef superClass "<init>" [] void,
       fold codes,
       vreturn
    ]
  return fts

generateFVs :: [NonVoid Id] -> CodeGen ([(NonVoid Id, CgLoc)], [(FieldType, Code)])
generateFVs fvs = do
  clClass <- getClass
  result <- forM (indexList nonVoidFvs) $ \(i, (nvId, ft)) -> do
    let fieldName = append "x" . pack . show $ i
    defineField $ mkFieldDef [Public, Final] fieldName ft
    let code = putfield $ mkFieldRef clClass fieldName ft
    return ((nvId, LocField ft clClass fieldName), (ft, code))
  return $ unzip result
  where nonVoidFvs = map addFt fvs
        addFt nvFV@(NonVoid fv) = (nvFV, ft)
          where ft = fromJust . repFieldType . idType $ fv

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
  (info, genInitCode) <- cgRhs name rhs
  addBinding info
  init <- genInitCode
  emit init

cgBind (StgRec pairs) = do
  result <- sequence $ unzipWith cgRhs pairs
  let (idInfos, initCodes) = unzip result
  addBindings idInfos
  (inits, body) <- getCodeWithResult $ sequence initCodes
  emit (fold inits <> body)

cgRhs :: Id -> StgRhs -> CodeGen (CgIdInfo, CodeGen Code)
cgRhs id (StgRhsCon _ con args) = buildDynCon con args
cgRhs name (StgRhsClosure _ binderInfo fvs updateFlag _ args body)
  = mkRhsClosure name binderInfo (nonVoidIds fvs) updateFlag args body

mkRhsClosure
  :: Id
  -> StgBinderInfo
  -> [NonVoid Id]
  -> UpdateFlag
  -> [Id]
  -> StgExpr
  -> CodeGen (CgIdInfo, CodeGen Code)
-- TODO: Selector thunks
mkRhsClosure binder _ fvs updateFlag [] (StgApp funId args)
  | length args == arity - 1
   && all (not . isVoidJRep . idJPrimRep . unsafeStripNV) fvs
   && isUpdatable updateFlag
   && arity <= mAX_SPEC_AP_SIZE
  = cgRhsStdThunk binder lfInfo payload
  where lfInfo = mkApLFInfo binder updateFlag arity
        payload = StgVarArg funId : args
        arity = length fvs
mkRhsClosure binder _ fvs updateFlag args body = do
  let lfInfo = mkClosureLFInfo binder NotTopLevel fvs updateFlag args
  (idInfo, cgLoc) <- rhsIdInfo binder lfInfo
  return (idInfo, genCode lfInfo cgLoc)
  where genCode lfInfo cgLoc = do
          (fields, CgState { cgClassName }) <- forkClosureBody $
            closureCodeBody False binder lfInfo
                            (nonVoidIds args) (length args) body reducedFVs

          -- TODO: Check if this is correct
          loads <- forM reducedFVs $ \(NonVoid id) -> do
            idInfo <- getCgIdInfo id
            return $ idInfoLoadCode idInfo

          let ft = obj cgClassName
              closureCode = fold
                [
                  new cgClassName,
                  dup ft,
                  fold loads,
                  invokespecial $ mkMethodRef cgClassName "<init>" fields void
                ]
          return $ mkRhsInit cgLoc closureCode
          where nvBinder = NonVoid binder
                binderIsFV = nvBinder `elem` fvs
                reducedFVs
                  | binderIsFV = delete nvBinder fvs
                  | otherwise = fvs

cgRhsStdThunk :: Id -> LambdaFormInfo -> [StgArg] -> CodeGen (CgIdInfo, CodeGen Code)
cgRhsStdThunk binder lfInfo payload = do
  (idInfo, cgLoc) <- rhsIdInfo binder lfInfo
  return (idInfo, genCode cgLoc)
  where genCode cgLoc = do
          loads <- mapM (getArgLoadCode . NonVoid) payload
          let apUpdCode = fold
                [
                  new apUpdClass,
                  dup ft,
                  fold loads,
                  invokespecial $ mkMethodRef apUpdClass "<init>" fields void
                ]
          return $ mkRhsInit cgLoc apUpdCode
          where (apUpdClass, n) = apUpdThunk stdForm
                ft = obj apUpdClass
                fields = replicate n closureType
                stdForm = lfStandardFormInfo lfInfo
