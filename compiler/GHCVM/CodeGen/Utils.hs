module GHCVM.CodeGen.Utils where

import Outputable
import Literal
import Codec.JVM
import Data.Char (ord)
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IntMap

cgLit :: Literal -> (FieldType, Code)
cgLit (MachStr s)           = (jString, sconst s)
cgLit (MachChar   c)        = (jint, iconst jint . fromIntegral $ ord c)
cgLit MachNullAddr          = (jobject, aconst_null)
cgLit (MachInt i)           = (jint, iconst jint $ fromIntegral i)
cgLit (MachWord i)          = (jint, iconst jint $ fromIntegral i)
cgLit (MachInt64 i)         = (jint, iconst jlong $ fromIntegral i)
cgLit (MachWord64 i)        = (jint, iconst jlong $ fromIntegral i)
cgLit (MachFloat r)         = (jfloat, fconst $ fromRational r)
cgLit (MachDouble r)        = (jdouble, dconst $ fromRational r)
-- TODO: Implement MachLabel
cgLit (MachLabel fs ms fod) = error $ "cgLit: MachLabel"
cgLit other                 = pprPanic "mkSimpleLit" (ppr other)

intSwitch :: Code -> [(Int, Code)] -> Maybe Code -> Code
intSwitch expr branches maybeDefault = undefined
  -- gswitch expr sortedBranches maybeDefault low high
  -- where sortedBranches = IntMap.assocs branches
  --       branchMap = IntMap.fromList branches
  --       low = fst . IntMap.findMin branchMap
  --       high = fst . IntMap.findMax branchMap

litSwitch :: Code -> [(Literal, Code)] -> Code -> Code
litSwitch = undefined
