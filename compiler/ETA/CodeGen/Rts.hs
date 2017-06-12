{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Rts where

import Data.Text (Text)
import Codec.JVM
import ETA.Util

import Data.Monoid((<>))

import qualified Data.Text as T

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = T.append x . T.cons '/' $ y

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
  frameType, conType, thunkType, rtsConfigType, exitCodeType,
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
  stgByteArray, rtsUnsigned, stgMutVar, stgMVar, stgTVar, rtsGroup, hsResult,
  stgBCO, stgWeak :: Text
stgConstr     = stg "DataCon"
stgClosure    = stg "Closure"
stgContext    = stg "StgContext"
capability    = stg "Capability"
task          = stg "Task"
stgInd        = thunk "UpdatableThunk"
stgIndStatic  = thunk "CAF"
stgThunk      = thunk "Thunk"
stgFun        = apply "Function"
stgTSO        = stg "TSO"
stackFrame    = stg "StackFrame"
rtsConfig     = rts "RtsConfig"
rtsOptsEnbled = rts "RuntimeOptions$RtsOptsEnabled"
exitCode      = rts "Runtime$ExitCode"
stgArray      = io "Array"
stgByteArray  = io "ByteArray"
rtsUnsigned   = merge "eta/integer" "Utils"
stgMutVar     = io "MutVar"
stgMVar       = conc "MVar"
stgTVar       = stm "TVar"
stgBCO        = interp "BCO"
stgWeak       = stg "WeakPtr"
rtsGroup      = rts "Runtime"
hsResult      = rts "Runtime$StgResult"


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

mkApFast :: Int -> [FieldType] -> Code
mkApFast arity rawFts = invokevirtual (mkMethodRef stgClosure applyFun rawFts (Just closureType))
  where fts = drop 1 rawFts
        applyFun
          | arity == 0 && null fts = "evaluate"
          | otherwise              = T.concat ["apply", foldMap toLetter fts, withV]
        withV
          | arity /= length fts    = "V"
          | otherwise              = ""
        toLetter ft
          | ft == closureType      = "P"
          | ft == jobject          = "O"
          | ft == jint             = "I"
          | ft == jlong            = "L"
          | ft == jfloat           = "F"
          | ft == jdouble          = "D"
          | otherwise              = error $ "mkApFast: Invalid field type ["
                                          ++ show ft ++ "]."

apUpdName :: Int -> Text
apUpdName n = thunk $ T.concat ["Ap",  T.pack $ show n, "Upd"]

selectThunkName :: Bool -> Text -> Text
selectThunkName updatable repText = thunk $ T.concat ["Selector", repText, updText]
  where updText = if updatable then "Upd" else "NoUpd"

constrField :: Int -> Text
constrField = T.cons 'x' . T.pack . show

constrFieldGetter :: Int -> Text
constrFieldGetter = T.append "get" . T.pack . show

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
stgExceptionGroup = exception "Exception"
ioGroup = io "IO"
stmGroup = stm "STM"
concGroup = conc "Concurrent"
stgGroup = stg "Stg"
parGroup = par "Parallel"
interpGroup = interp "Interpreter"

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
  where name = T.append "get" $ fieldTypeSuffix ft

byteBufferPut :: FieldType -> Code
byteBufferPut ft = invokevirtual $ mkMethodRef byteBuffer name [jint, ft] (ret byteBufferType)
  where name = T.append "put" $ fieldTypeSuffix ft

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
         <> invokestatic (mkMethodRef (rts "RuntimeLogging") "barf" [jstring, arrayFt] void)
  where arrayFt = jarray jobject

hsResultCap :: Code
hsResultCap = getfield $ mkFieldRef hsResult "cap" capabilityType

hsResultValue :: Code
hsResultValue = getfield $ mkFieldRef hsResult "result" closureType

trueClosure :: Code
trueClosure = invokestatic . mkMethodRef "ghc_prim/ghc/Types" "DTrue_closure" [] $ Just closureType

falseClosure :: Code
falseClosure = invokestatic . mkMethodRef "ghc_prim/ghc/Types" "DFalse_closure" [] $ Just closureType

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
