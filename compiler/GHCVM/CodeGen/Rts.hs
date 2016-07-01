module GHCVM.CodeGen.Rts where

import Data.Text

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg :: Text -> Text
rts = merge "ghcvm/runtime"
apply = merge (rts "apply")
thunk = merge (rts "thunk")
stg = merge (rts "stg")

stgConstr, stgClosure, stgInd, stgIndStatic, stgThunk, stgFun :: Text
stgConstr = stg "StgConstr"
stgClosure = stg "StgClosure"
stgInd = thunk "StgInd"
stgIndStatic = thunk "StgIndStatic"
stgThunk = thunk "StgThunk"
stgFun = thunk "StgFun"
