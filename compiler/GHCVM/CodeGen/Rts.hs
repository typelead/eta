module GHCVM.CodeGen.Rts where

import Data.Text
import Codec.JVM

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg :: Text -> Text
rts = merge "ghcvm/runtime"
apply = merge (rts "apply")
thunk = merge (rts "thunk")
stg = merge (rts "stg")

closureType, indStaticType, contextType, funType :: FieldType
closureType = obj stgClosure
indStaticType = obj stgIndStatic
contextType = obj stgContext
funType = obj stgFun

stgConstr, stgClosure, stgContext, stgInd, stgIndStatic, stgThunk, stgFun
  :: Text
stgConstr = stg "StgConstr"
stgClosure = stg "StgClosure"
stgContext = stg "StgContext"
stgInd = thunk "StgInd"
stgIndStatic = thunk "StgIndStatic"
stgThunk = thunk "StgThunk"
stgFun = apply "StgFun"
