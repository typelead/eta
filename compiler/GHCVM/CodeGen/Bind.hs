module GHCVM.CodeGen.Bind where

import StgSyn
import Id
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Expr
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Name
import GHCVM.Util
import Codec.JVM
import Control.Monad (forM)
import Data.Text (append, pack)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe, maybe, fromJust)
import Data.Monoid ((<>))

closureCodeBody
  :: Bool                  -- whether this is a top-level binding
  -> Id                    -- the closure's name
  -> LambdaFormInfo        -- Lots of information about this closure
  -> [NonVoid Id]          -- incoming args to the closure
  -> Int                   -- arity, including void args
  -> StgExpr               -- body
  -> [Id]                  -- the closure's free vars
  -> CodeGen ()
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
      let (argLocs, code) = mkCallEntry args
          (_ , cgLocs) = unzip argLocs
      emit code
      bindArgs argLocs
      -- TODO: Implement marking for self loop
      -- markOffset
      withSelfLoop (id, cgLocs) $ do
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
  return ()

generateFVs :: [Id] -> CodeGen ([(NonVoid Id, CgLoc)], [(FieldType, Code)])
generateFVs fvs = do
  clClass <- getClass
  result <- forM (indexList nonVoidFvs) $ \(i, (nvId, ft)) -> do
    let fieldName = append "x" . pack . show $ i
    defineField $ mkFieldDef [Public, Final] fieldName ft
    let code = putfield $ mkFieldRef clClass fieldName ft
    return ((nvId, LocField ft clClass fieldName), (ft, code))
  return $ unzip result
  where nonVoidFvs = mapMaybe filterNonVoid fvs
        filterNonVoid fv = fmap (NonVoid fv,) ft
          where ft = repFieldType . idType $ fv

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

-- TODO: Beautify this code
-- TODO: There are a lot of bangs in this function. Verify that they do
--       indeed help.
mkCallEntry :: [NonVoid Id] -> ([(NonVoid Id, CgLoc)], Code)
mkCallEntry nvArgs = (zip nvArgs locs, code)
  where fts' = map (fromJust . repFieldType . idType) args'
        args' = map unsafeStripNV nvArgs
        argReps' = map fieldTypeArgRep fts'
        (!code, !locs) = loadArgs 2 mempty [] args' fts' argReps' 2 1 1 1 1 1
        loadArgs !n !code !locs (arg:args) (ft:fts) (argRep:argReps)
                 !r !i !l !f !d !o =
          case argRep of
            P -> loadRec (context r) (r + 1) i l f d o
            N -> loadRec (context i <> gconv jint ft) r (i + 1) l f d o
            L -> loadRec (context l) r i (l + 1) f d o
            F -> loadRec (context f) r i l (f + 1) d o
            D -> loadRec (context d) r i l f (d + 1) o
            O -> loadRec (context o) r i l f d (o + 1)
            _ -> error "contextLoad: V"
          where context = contextLoad ft argRep
                loadRec nextCode =
                  loadArgs (n + ftSize) (code <> nextCode <> gstore ft n)
                           (loc:locs) args fts argReps
                ftSize = fieldSize ft
                loc = LocLocal ft n
        loadArgs _ !code !locs _ _ _ _ _ _ _ _ _ = (code, reverse locs)
