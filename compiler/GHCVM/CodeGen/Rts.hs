module GHCVM.CodeGen.Rts where

import Data.Text
import Codec.JVM
import GHCVM.Util

import Data.Monoid((<>))

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg :: Text -> Text
rts = merge "ghcvm/runtime"
apply = merge (rts "apply")
thunk = merge (rts "thunk")
stg = merge (rts "stg")

closureType, indStaticType, contextType, funType, tsoType, frameType,
  rtsFunType, conType, thunkType :: FieldType
closureType = obj stgClosure
indStaticType = obj stgIndStatic
contextType = obj stgContext
funType = obj stgFun
tsoType = obj stgTSO
frameType = obj stackFrame
rtsFunType = obj rtsFun
conType = obj stgConstr
thunkType = obj stgThunk

stgConstr, stgClosure, stgContext, stgInd, stgIndStatic, stgThunk, stgFun,
  stgTSO, stackFrame :: Text
stgConstr = stg "StgConstr"
stgClosure = stg "StgClosure"
stgContext = stg "StgContext"
stgInd = thunk "StgInd"
stgIndStatic = thunk "StgIndStatic"
stgThunk = thunk "StgThunk"
stgFun = apply "StgFun"
stgTSO = stg "StgTSO"
stackFrame = stg "StackFrame"
rtsFun = stg "RtsFun"

storeR, loadR, storeI, loadI, storeL, loadL, storeF, loadF, storeD, loadD,
 storeO, loadO :: Code

(storeR, loadR) = contextLoadStore "R" closureType
(storeI, loadI) = contextLoadStore "I" jint
(storeL, loadL) = contextLoadStore "L" jlong
(storeF, loadF) = contextLoadStore "F" jfloat
(storeD, loadD) = contextLoadStore "D" jdouble
(storeO, loadO) = contextLoadStore "O" jobject

contextLoadStore :: Text -> FieldType -> (Code, Code)
contextLoadStore name ft =
  ( invokevirtual $ mkMethodRef stgContext name [jint, ft] void
  , invokevirtual $ mkMethodRef stgContext name [jint] (ret ft))

argPatToFrame :: Text -> Text
argPatToFrame patText = append (upperFirst a) (toUpper b)
  where [a,b] = split (== '_') patText

loadContext :: Code
loadContext = gload contextType 1

currentTSOField :: Code
currentTSOField = getfield (mkFieldRef stgContext "currentTSO" tsoType)

spPushMethod :: Code
spPushMethod = invokevirtual (mkMethodRef stgTSO "spPush" [frameType] void)

mkApFast :: Text -> Code
mkApFast patText =
     getstatic (mkFieldRef (apply "Apply") fullPat rtsFunType)
  <> loadContext
  <> invokevirtual (mkMethodRef rtsFun fullPat [contextType] void)
  -- TODO: We can do better than rtsFun, but it depends on the
  --       determinism of javac.
  where fullPat = append patText "_fast"
