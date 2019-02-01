{-# LANGUAGE OverloadedStrings #-}
module Eta.CodeGen.Rts where

import Data.Text (Text)
import Codec.JVM
import Codec.JVM.Const

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
  hsResultType, stgTVarType, stgBCOType, stgWeakType, valueType :: FieldType
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
valueType         = obj stgValue

stgConstr, stgClosure, stgContext, capability, task, stgInd, stgIndStatic, stgThunk,
  stgFun, stgTSO, stackFrame, rtsConfig, rtsOptsEnbled, exitCode, stgArray,
  stgByteArray, rtsUnsigned, stgMutVar, stgMVar, stgTVar, rtsGroup, hsResult,
  stgBCO, stgWeak, stablePtrTable, stgValue :: Text
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
stablePtrTable = stg "StablePtrTable"
stgValue      = stg "Value"

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

fieldStoreR, fieldLoadR, fieldStoreI, fieldLoadI, fieldStoreL, fieldLoadL, fieldStoreF, fieldLoadF, fieldStoreD, fieldLoadD,
 fieldStoreO, fieldLoadO :: Int -> Code

(fieldStoreR, fieldLoadR) = contextLoadStoreField "R" closureType
(fieldStoreI, fieldLoadI) = contextLoadStoreField "I" jint
(fieldStoreL, fieldLoadL) = contextLoadStoreField "L" jlong
(fieldStoreF, fieldLoadF) = contextLoadStoreField "F" jfloat
(fieldStoreD, fieldLoadD) = contextLoadStoreField "D" jdouble
(fieldStoreO, fieldLoadO) = contextLoadStoreField "O" jobject

contextLoadStore :: Text -> FieldType -> (Code, Code)
contextLoadStore name ft =
  ( invokevirtual $ mkMethodRef stgContext name [jint, ft] void
  , invokevirtual $ mkMethodRef stgContext name [jint] (ret ft))

contextLoadStoreField :: Text -> FieldType -> (Int -> Code, Int -> Code)
contextLoadStoreField name ft =
  ( \n -> putfield (fieldRef n)
  , \n -> getfield (fieldRef n))
  where fieldRef n = mkFieldRef stgContext (name <> T.pack (show n)) ft

currentTSOField :: Code
currentTSOField = getfield (mkFieldRef stgContext "currentTSO" tsoType)

trampolineField :: Code
trampolineField = getfield (mkFieldRef stgContext "trampoline" jbool)

putTrampolineField :: Code
putTrampolineField = putfield (mkFieldRef stgContext "trampoline" jbool)

getAndSetTrampolineMethod :: Code
getAndSetTrampolineMethod = invokevirtual (mkMethodRef stgContext "getAndSetTrampoline" [] (ret jbool))

mkApFast :: Bool -> Int -> Text -> [FieldType] -> Code
mkApFast tailCall arity realCls fts =
  invokevirtual (mkMethodRef realCls applyFun (contextType:fts) (ret closureType))
  where applyFun = mkApFun tailCall arity fts

enterTailMethod :: Code
enterTailMethod =
  invokestatic (mkMethodRef stgGroup "enterTail" [contextType,closureType] void)

mkApFastTail :: Int -> [FieldType] -> Code
mkApFastTail arity fts =
  invokestatic (mkMethodRef stgGroup applyFun ctxtFts void)
  where applyFun = mkApFun False arity fts <> "Tail"
        ctxtFts  = contextType:closureType:fts

mkApFun :: Bool -> Int -> [FieldType] -> Text
mkApFun tailCall arity fts = applyFun
  where applyFun
          | arity == 0 && null fts = if tailCall then "evaluateTail" else "evaluate"
          | otherwise              = T.concat ["apply", applyExt, withV]
          where (ps, rest) = span (== "P") letters
                showPs
                  | len > 0   = T.pack (show len)
                  | otherwise = mempty
                  where len = length ps
                letters    = map toLetter fts
                applyExt   = showPs <> mconcat rest
        withV
          | arity /= length fts    = "V"
          | otherwise              = ""
        toLetter ft
          | ft == closureType      = "P"
          | ft == jobject          = "O"
          | ft == jint             = "N"
          | ft == jlong            = "L"
          | ft == jfloat           = "F"
          | ft == jdouble          = "D"
          | otherwise              = error $ "mkApFast: Invalid field type ["
                                          ++ show ft ++ "]."

apUpdName :: Int -> Text
apUpdName n = thunk $ T.concat ["Ap",  T.pack $ show n, "Upd"]

selectorThunk :: Text
selectorThunk = thunk "SelectorThunk"

selectorThunkType :: FieldType
selectorThunkType = obj selectorThunk

selectorThunkCreate :: Code
selectorThunkCreate = invokestatic $
  mkMethodRef selectorThunk "create" [contextType, jint, closureType] (ret selectorThunkType)

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

suspendInterruptsMethod :: Code -> Bool -> Code
suspendInterruptsMethod loadContext interruptible =
     loadContext
  <> currentTSOField
  <> dup tsoType
  <> iconst jbool (boolToInt interruptible)
  <> invokevirtual (mkMethodRef stgTSO "suspendInterrupts" [jbool] (ret jbool))
  where boolToInt True  = 1
        boolToInt False = 0

resumeInterruptsMethod :: Code
resumeInterruptsMethod =
  invokevirtual $ mkMethodRef stgTSO "resumeInterrupts" [jbool] void

stgExceptionGroup, ioGroup, stmGroup, concGroup, parGroup, interpGroup, stgGroup :: Text
stgExceptionGroup = exception "Exception"
ioGroup = io "IO"
stmGroup = stm "STM"
concGroup = conc "Concurrent"
stgGroup = stg "Stg"
parGroup = par "Parallel"
interpGroup = interp "Interpreter"

-- Types
byteArrayBuf :: Code
byteArrayBuf = getfield $ mkFieldRef stgByteArray "bufferAddress" jlong

byteArraySize :: Code
byteArraySize = getfield $ mkFieldRef stgByteArray "size" jint

addressGet :: FieldType -> Code
addressGet ft = invokestatic $ mkMethodRef memoryManager name [jlong] (ret ft)
  where name = T.append "get" $ fieldTypeSuffix ft

addressPut :: FieldType -> Code
addressPut ft = invokestatic $ mkMethodRef memoryManager name [jlong, ft] void
  where name = T.append "put" $ fieldTypeSuffix ft

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
mutVarSetValue = invokevirtual $ mkMethodRef stgMutVar "set" [closureType] void

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
trueClosure = invokestatic . mkMethodRef "ghc_prim/ghc/Types" "DTrue" [] $ ret closureType

falseClosure :: Code
falseClosure = invokestatic . mkMethodRef "ghc_prim/ghc/Types" "DFalse" [] $ ret closureType

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

ftClassObject :: FieldType -> Code
ftClassObject ft@(BaseType _) =
  getstatic $ mkFieldRef (ftWrapper ft) "TYPE" classFt
ftClassObject (ObjectType iclassName) = gldc classFt (CClass iclassName)
ftClassObject (ArrayType ft') = gldc classFt (CClass . IClassName $ mkFieldDesc' ft')

ftWrapper :: FieldType -> Text
ftWrapper (BaseType prim) =
  prefix <> case prim of
    JBool   -> "Boolean"
    JChar   -> "Character"
    JFloat  -> "Float"
    JDouble -> "Double"
    JByte   -> "Byte"
    JShort  -> "Short"
    JInt    -> "Integer"
    JLong   -> "Long"
  where prefix = "java/lang/"
ftWrapper ft = error $ "ftWrapper: Not a base type: " ++ show ft

classType, methodType :: Text
classType = "java/lang/Class"
methodType = "java/lang/reflect/Method"

classFt, methodFt :: FieldType
classFt  = obj classType
methodFt = obj methodType

mkRecInitMethodName :: Int -> Text
mkRecInitMethodName n = "$recInit" <> T.pack (show n)

mkRecBindingMethodName :: Int -> Text
mkRecBindingMethodName n = "$recBinding" <> T.pack (show n)

loadStringLatin1, loadStringUTF8 :: Bool -> Code
loadStringLatin1 arrayForm
  | arrayForm =
    invokestatic $ mkMethodRef memoryManager "loadStringsLatin1" [jarray jstring] (ret jlong)
  | otherwise =
    invokestatic $ mkMethodRef memoryManager "loadStringLatin1" [jstring] (ret jlong)
loadStringUTF8 arrayForm
  | arrayForm =
    invokestatic $ mkMethodRef memoryManager "loadStringsUTF8" [jarray jstring] (ret jlong)
  | otherwise =
    invokestatic $ mkMethodRef memoryManager "loadStringUTF8"   [jstring] (ret jlong)

isClosureFt :: FieldType -> Bool
isClosureFt ft = ft == closureType

-- Tail methods

singletonInstanceName :: Text
singletonInstanceName = "INSTANCE"

getClosureMethod :: Int -> Code
getClosureMethod int =
     iconst jint (fromIntegral int)
  <> invokestatic (mkMethodRef "eta/serv/Utils" "getClosure" [jint] (ret jobject))
  <> gconv jobject closureType

indirecteeField :: Code
indirecteeField = getfield (mkFieldRef stgThunk "indirectee" closureType)

stringCAF :: Text
stringCAF = "eta/base/StringCAF"
