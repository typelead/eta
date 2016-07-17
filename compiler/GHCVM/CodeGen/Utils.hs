module GHCVM.CodeGen.Utils where

import Outputable
import Literal
import Codec.JVM
import Data.Char (ord)
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IntMap

cgLit :: Literal -> Code
cgLit (MachStr s)           = sconst s
cgLit (MachChar   c)        = iconst jint . fromIntegral $ ord c -- Verify this
cgLit MachNullAddr          = aconst_null -- TODO: Is this correct?
cgLit (MachInt i)           = iconst jint $ fromIntegral i
cgLit (MachWord i)          = iconst jint $ fromIntegral i
cgLit (MachInt64 i)         = iconst jlong $ fromIntegral i
cgLit (MachWord64 i)        = iconst jlong $ fromIntegral i
cgLit (MachFloat r)         = fconst $ fromRational r
cgLit (MachDouble r)        = dconst $ fromRational r
-- TODO: Handle this later
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
