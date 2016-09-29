{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Rts where

import GHCVM.Main.DynFlags

import Data.Text
import Codec.JVM
import GHCVM.Util
import GHCVM.CodeGen.Name

import Data.Monoid((<>))
import Data.Foldable(fold)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg, exception, io, util, stm, par :: Text -> Text
rts       = merge "ghcvm/runtime"
apply     = merge (rts "apply")
thunk     = merge (rts "thunk")
stg       = merge (rts "stg")
exception = merge (rts "exception")
io        = merge (rts "io")
conc      = merge (rts "concurrent")
util      = merge (rts "util")
stm       = merge (rts "stm")
par       = merge (rts "parallel")

closureType, indStaticType, contextType, funType, tsoType, frameType, rtsFunType, conType,
  thunkType, rtsConfigType, exitCodeType, rtsOptsEnbledType, stgArrayType, stgByteArrayType,
  stgMutVarType, stgMVarType :: FieldType
closureType       = obj stgClosure
indStaticType     = obj stgIndStatic
contextType       = obj stgContext
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

stgConstr, stgClosure, stgContext, stgInd, stgIndStatic, stgThunk, stgFun, stgTSO, stackFrame,
  rtsConfig, rtsOptsEnbled, exitCode, stgArray, stgByteArray, rtsUnsigned, stgMutVar,
  stgMVar :: Text
stgConstr     = stg "StgConstr"
stgClosure    = stg "StgClosure"
stgContext    = stg "StgContext"
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
rtsUnsigned   = merge "ghcvm/integer" "Utils"
stgMutVar     = io "StgMutVar"
stgMVar       = conc "StgMVar"

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

-- suspendThreadMethod :: Bool -> Code
-- suspendThreadMethod interruptible
--   = iconst jbool (boolToInt)
--  <> loadContext
--  <>

mkRtsMainClass :: DynFlags -> String -> ClassFile
mkRtsMainClass dflags mainClass
  = mkClassFile java7 [Public, Super] mainClass' Nothing []
  [
    mkMethodDef mainClass' [Public, Static] "main" [jarray jstring] void $ fold
    [
      invokestatic $ mkMethodRef rtsConfig "getDefault" [] (ret rtsConfigType),
      putRtsHsMain,
      putRtsOptsEnabled,
      putRtsOpts,
      gstore rtsConfigType 1,
      gload (jarray jstring) 0,
      -- TODO: Find main module
      getstatic $ mkFieldRef (moduleJavaClass mainMod) "DZCmain_closure"
                             closureType,
      gload rtsConfigType 1,
      invokestatic $ mkMethodRef (rts "Rts") "hsMain" [ jarray jstring
                                                      , closureType
                                                      , rtsConfigType]
                                                      (ret exitCodeType),
      invokevirtual $ mkMethodRef exitCode "code" [] (ret jint),
      invokestatic $ mkMethodRef "java/lang/System" "exit" [jint] void,
      vreturn
    ]
  ]
  where mainClass' = pack mainClass
        mainMod = mainModIs dflags
        rtsOptsEnabledText = pack . show . rtsOptsEnabled $ dflags
        putRtsHsMain =  dup rtsConfigType
                     <> iconst jbool 1
                     <> putfield (mkFieldRef rtsConfig "rtsHsMain" jbool)
        putRtsOptsEnabled
          =  dup rtsConfigType
          <> getstatic (mkFieldRef rtsOptsEnbled rtsOptsEnabledText
                                   rtsOptsEnbledType)
          <> putfield (mkFieldRef rtsConfig "rtsOptsEnabled"
                                  rtsOptsEnbledType)
        putRtsOpts = case rtsOpts dflags of
          Nothing -> mempty
          Just s -> dup rtsConfigType
                 <> sconst (T.pack s)
                 <> putfield (mkFieldRef rtsConfig "rtsOpts" jstring)

stgExceptionGroup, ioGroup, stmGroup, concGroup, parGroup :: Text
stgExceptionGroup = exception "StgException"
ioGroup = io "IO"
stmGroup = stm "STM"
concGroup = conc "Concurrent"
stgGroup = stg "Stg"
parGroup = par "Parallel"

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

mutVarValue :: Code
mutVarValue = getfield $ mkFieldRef stgMutVar "value" closureType

mutVarSetValue :: Code
mutVarSetValue = putfield $ mkFieldRef stgMutVar "value" closureType

mVarValue :: Code
mVarValue = getfield $ mkFieldRef stgMVar "value" closureType

barf :: Text -> Code
barf text = sconst text
         <> iconst jint (fromIntegral 0)
         <> new arrayFt
         <> invokestatic (mkMethodRef (rts "RtsMessages") "barf" [jstring, arrayFt] void)
  where arrayFt = jarray jobject
