module Macros (subs) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

type Macros = M.Map String String

defaultMacros :: Macros
defaultMacros = M.singleton "CLOSURE_PTR" "long"

validMacroIdChars = "A-Za-z_"

allMacros :: Macros -> Macros
allMacros initMap = M.insert "REF_CLOSURE_PTR" (rEF_CLOSURE_PTR cLOSURE_PTR) initMap
  where
    cLOSURE_PTR = fromJust (M.lookup "CLOSURE_PTR" initMap)
    rEF_CLOSURE_PTR "long" = "LongPtr"
    rEF_CLOSURE_PTR "int" = "IntPtr"

subs :: [String]
subs = concatMap (\expr -> ["-e", expr]) sedExprs
  where
    sedExprs = M.foldrWithKey (\k v accum -> (genSedExpr k v) : accum) [] $ allMacros defaultMacros
    genSedExpr before after = "s/\\([^" ++ validMacroIdChars ++ "]\\)" ++ before ++  " /\\1" ++ after ++ " /g"
