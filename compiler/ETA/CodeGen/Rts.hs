{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Rts where



import Data.Text
import Codec.JVM
import ETA.Util

import Data.Monoid((<>))


import qualified Data.Text as T

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg, exception, io, util, stm, par, interp, conc :: Text -> Text
rts       = merge "eta/runtime"
apply     = merge (rts "apply")
thunk     = merge (rts "thunk")
stg       = merge (rts "stg")
exception = merge (rts "exception")
io        = merge (rts "io")
conc      = merge (rts "concurrent")
util      = merge (rts "util")
stm       = merge (rts "stm")
par       = merge (rts "parallel")
interp    = merge (rts "interpreter")

closureType, indStaticType, contextType, capabilityType, taskType, funType, tsoType,
  frameType, rtsFunType, conType, thunkType, rtsConfigType, exitCodeType,
  rtsOptsEnbledType, stgArrayType, stgByteArrayType, stgMutVarType, stgMVarType,
  hsResultType, stgTVarType, stgBCOType, stgWeakType :: FieldType
closureType       = obj stgClosure
indStaticType     = obj stgIndStatic
contextType       = obj stgContext
capabilityType    = obj capability
taskType          = obj task
funType           = obj stgFun
tsoType           = obj stgTSO
frameType         = obj stackFrame
rtsFunType        = obj rtsFun
conType           = obj stgConstr
thunkType         = obj stgThunk
rtsConfigType     = obj rtsConfig
rtsOptsEnbledType = obj rtsOptsEnbled
exitCodeType      = obj exitCode
stgArrayType      = obj stgArray
stgMutVarType     = obj stgMutVar
stgByteArrayType  = obj stgByteArray
stgMVarType       = obj stgMVar
stgTVarType       = obj stgTVar
hsResultType      = obj hsResult
stgBCOType        = obj stgBCO
stgWeakType       = obj stgWeak

stgConstr, stgClosure, stgContext, capability, task, stgInd, stgIndStatic, stgThunk,
  stgFun, stgTSO, stackFrame, rtsConfig, rtsOptsEnbled, exitCode, stgArray,
  stgByteArray, rtsUnsigned, stgMutVar, stgMVar, stgTVar, rtsGroup, hsResult, rtsFun,
  stgBCO, stgWeak :: Text
stgConstr     = stg "StgConstr"
stgClosure    = stg "StgClosure"
stgContext    = stg "StgContext"
capability    = stg "Capability"
task          = stg "Task"
stgInd        = thunk "StgInd"
stgIndStatic  = thunk "StgIndStatic"
stgThunk      = thunk "StgThunk"
stgFun        = apply "StgFun"
stgTSO        = stg "StgTSO"
stackFrame    = stg "StackFrame"
rtsFun        = stg "RtsFun"
rtsConfig     = rts "RtsConfig"
rtsOptsEnbled = rts "RtsFlags$RtsOptsEnabled"
exitCode      = rts "Rts$ExitCode"
stgArray      = io "StgArray"
stgByteArray  = io "StgByteArray"
rtsUnsigned   = merge "eta/integer" "Utils"
stgMutVar     = io "StgMutVar"
stgMVar       = conc "StgMVar"
stgTVar       = stm "StgTVar"
stgBCO        = interp "StgBCO"
stgWeak       = stg "StgWeak"
rtsGroup      = rts "Rts"
hsResult      = rts "Rts$HaskellResult"


memoryManager :: Text
memoryManager = io "MemoryManager"

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

spTopIndexMethod :: Code
spTopIndexMethod = invokevirtual (mkMethodRef stgContext "stackTopIndex" [] (ret jint))

spTopMethod :: Code
spTopMethod = invokevirtual (mkMethodRef stgContext "stackTop" [] (ret frameType))

checkForStackFramesMethod :: Code
checkForStackFramesMethod =
  invokevirtual (mkMethodRef stgContext "checkForStackFrames" [jint, frameType] (ret jbool))

mkApFast :: Text -> Code
mkApFast patText =
     getstatic (mkFieldRef (apply "Apply") fullPat rtsFunType)
  <> loadContext
  <> invokevirtual (mkMethodRef rtsFun "enter" [contextType] void)
  -- TODO: We can do better than rtsFun, but it depends on the
  --       determinism of javac.
  where fullPat = append patText "_fast"

apUpdName :: Int -> Text
apUpdName n = thunk $ T.concat ["Ap",  pack $ show n, "Upd"]

selectThunkName :: Bool -> Text -> Text
selectThunkName updatable repText = thunk $ T.concat ["Selector", repText, updText]
  where updText = if updatable then "Upd" else "NoUpd"

constrField :: Int -> Text
constrField = cons 'x' . pack . show

constrFieldGetter :: Int -> Text
constrFieldGetter = append "get" . pack . show

myCapability :: FieldRef
myCapability = mkFieldRef stgContext "myCapability" capabilityType

contextMyCapability :: Code
contextMyCapability = getfield myCapability

contextMyCapabilitySet :: Code
contextMyCapabilitySet = putfield myCapability

suspendThreadMethod :: Bool -> Code
suspendThreadMethod interruptible =
     loadContext
  <> contextMyCapability
  -- <> dup capabilityType
  <> iconst jbool (boolToInt interruptible)
  <> invokevirtual (mkMethodRef capability "suspendThread" [jbool] (ret taskType))
  where boolToInt True = 1
        boolToInt False = 0

resumeThreadMethod :: Code
resumeThreadMethod =
     invokestatic (mkMethodRef capability "resumeThread" [taskType] (ret capabilityType))
  <> loadContext
  <> swap capabilityType contextType
  <> contextMyCapabilitySet

stgExceptionGroup, ioGroup, stmGroup, concGroup, parGroup, interpGroup, stgGroup :: Text
stgExceptionGroup = exception "StgException"
ioGroup = io "IO"
stmGroup = stm "STM"
concGroup = conc "Concurrent"
stgGroup = stg "Stg"
parGroup = par "Parallel"
interpGroup = interp "Interpreter"

mkRtsFunCall :: (Text, Text) -> Code
mkRtsFunCall (group, name) =
     getstatic (mkFieldRef group name rtsFunType)
  <> loadContext
  <> invokevirtual (mkMethodRef rtsFun "enter" [contextType] void)

-- Types
buffer :: Text
buffer = "java/nio/Buffer"

bufferType :: FieldType
bufferType = obj buffer

byteBuffer :: Text
byteBuffer = "java/nio/ByteBuffer"

byteBufferType :: FieldType
byteBufferType = obj byteBuffer

byteArrayBuf :: Code
byteArrayBuf = getfield $ mkFieldRef stgByteArray "buf" byteBufferType

byteBufferCapacity :: Code
byteBufferCapacity = invokevirtual $ mkMethodRef byteBuffer "capacity" [] (ret jint)

byteBufferGet :: FieldType -> Code
byteBufferGet ft = invokevirtual $ mkMethodRef byteBuffer name [jint] (ret ft)
  where name = append "get" $ fieldTypeSuffix ft

byteBufferPut :: FieldType -> Code
byteBufferPut ft = invokevirtual $ mkMethodRef byteBuffer name [jint, ft] (ret byteBufferType)
  where name = append "put" $ fieldTypeSuffix ft

byteBufferPosGet :: Code
byteBufferPosGet = invokevirtual $ mkMethodRef byteBuffer "position" [] (ret jint)

byteBufferAddrGet :: Code
byteBufferAddrGet = invokestatic $ mkMethodRef memoryManager "getAddress" [byteBufferType] (ret jint)

byteBufferPosSet :: Code
byteBufferPosSet = invokevirtual $ mkMethodRef byteBuffer "position" [jint] (ret bufferType)

byteBufferDup :: Code
byteBufferDup = invokevirtual $ mkMethodRef byteBuffer "duplicate" [] (ret byteBufferType)

fieldTypeSuffix :: FieldType -> Text
fieldTypeSuffix (BaseType prim) =
  case prim of
    JBool   -> "Int"
    JChar   -> "Char"
    JFloat  -> "Float"
    JDouble -> "Double"
    JByte   -> ""
    JShort  -> "Short"
    JInt    -> "Int"
    JLong   -> "Long"
fieldTypeSuffix ft = error $ "fieldTypeSuffix: " ++ show ft

mutVarValue :: Code
mutVarValue = getfield $ mkFieldRef stgMutVar "value" closureType

mutVarSetValue :: Code
mutVarSetValue = putfield $ mkFieldRef stgMutVar "value" closureType

mVarValue :: Code
mVarValue = getfield $ mkFieldRef stgMVar "value" closureType

barf :: Text -> Code
barf text = sconst text
         <> iconst jint (0)
         <> new arrayFt
         <> invokestatic (mkMethodRef (rts "RtsMessages") "barf" [jstring, arrayFt] void)
  where arrayFt = jarray jobject

hsResultCap :: Code
hsResultCap = getfield $ mkFieldRef hsResult "cap" capabilityType

hsResultValue :: Code
hsResultValue = getfield $ mkFieldRef hsResult "result" closureType

trueClosure :: Code
trueClosure = getstatic $ mkFieldRef "ghczmprim/ghc/Types" "DTrue_closure" closureType

falseClosure :: Code
falseClosure = getstatic $ mkFieldRef "ghczmprim/ghc/Types" "DFalse_closure" closureType

getTagMethod :: Code -> Code
getTagMethod code
  = code
 <> gconv closureType conType
 <> invokevirtual (mkMethodRef stgConstr "getTag" [] (ret jint))

printStream :: Text
printStream = "java/io/PrintStream"

printStreamType :: FieldType
printStreamType = obj printStream

debugPrint :: FieldType -> Code
debugPrint ft = dup ft
             <> getstatic (mkFieldRef "java/lang/System" "out" printStreamType)
             <> swap ft printStreamType
             <> invokevirtual (mkMethodRef printStream "println" [genFt ft] void)
  where genFt (ObjectType _) = jobject
        genFt (ArrayType _)  = jobject
        genFt ft             = ft

nullAddr :: Code
nullAddr = getstatic $ mkFieldRef memoryManager "nullAddress" byteBufferType
