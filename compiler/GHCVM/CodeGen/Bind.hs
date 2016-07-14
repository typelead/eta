module GHCVM.CodeGen.Bind where

import StgSyn
import Id
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Expr
import GHCVM.Util
import Codec.JVM
import Control.Monad (forM)
import Data.Text (append, pack)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe, maybe)

closureCodeBody
  :: Bool                  -- whether this is a top-level binding
  -> Id                    -- the closure's name
  -> LambdaFormInfo        -- Lots of information about this closure
  -> [NonVoid Id]          -- incoming args to the closure
  -> Int                   -- arity, including void args
  -> StgExpr               -- body
  -> [Id]                  -- the closure's free vars
  -> CodeGen ()
closureCodeBody topLevel binder lfInfo args arity body fvs = do
  fvLocs <- generateFVs fvs
  if arity == 0 then
    thunkCode lfInfo fvLocs body
  else
    return ()

generateFVs :: [Id] -> CodeGen [(NonVoid Id, CgLoc)]
generateFVs fvs = do
  clClass <- getClass
  result <- forM (indexList nonVoidFvs) $ \(i, (nvId, ft)) -> do
    let fieldName = append "x" . pack . show $ i
    defineField $ mkFieldDef [Public, Final] fieldName ft
    let code = putfield $ mkFieldRef clClass fieldName ft
    return ((nvId, LocField ft clClass fieldName), (ft, code))
  let (results, initCodes) = unzip result
  let thisFt = obj clClass
  let codes = flip concatMap (indexList initCodes) $ \(i, (ft, code)) ->
        [
          gload thisFt 0,
          gload ft i,
          code
        ]
  defineMethod . mkMethodDef clClass [Public] "<init>" [] void $ fold codes
  return results
  where nonVoidFvs = mapMaybe filterNonVoid fvs
        filterNonVoid fv = fmap (NonVoid fv,) ft
          where ft = repFieldType . idType $ fv

-- TODO: Implement eager blackholing
thunkCode :: LambdaFormInfo -> [(NonVoid Id, CgLoc)] -> StgExpr -> CodeGen ()
thunkCode lfInfo fvs body =
  setupUpdate lfInfo $
    cgExpr body

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

