module GHCVM.CodeGen.Rts where

import DynFlags

import Data.Text
import Codec.JVM
import GHCVM.Util
import GHCVM.CodeGen.Name

import Data.Monoid((<>))
import Data.Foldable(fold)
import qualified Data.ByteString.Char8 as BC

-- NOTE: If the RTS is refactored, this file must also be updated accordingly

-- merge "a" "b" == "a/b"
merge :: Text -> Text -> Text
merge x y = append x . cons '/' $ y

rts, apply, thunk, stg :: Text -> Text
rts   = merge "ghcvm/runtime"
apply = merge (rts "apply")
thunk = merge (rts "thunk")
stg   = merge (rts "stg")

closureType, indStaticType, contextType, funType, tsoType, frameType,
  rtsFunType, conType, thunkType, rtsConfigType, exitCodeType,
  rtsOptsEnbledType :: FieldType
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

stgConstr, stgClosure, stgContext, stgInd, stgIndStatic, stgThunk, stgFun,
  stgTSO, stackFrame, rtsConfig, rtsOptsEnbled, exitCode :: Text
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

apUpdName :: Int -> Text
apUpdName n = thunk $ append (append "Ap" (pack . show $ n)) "Upd"

constrField :: Int -> Text
constrField = cons 'x' . pack . show

constrFieldGetter :: Int -> Text
constrFieldGetter = append "get" . pack . show

getTagMethod :: Code -> Code
getTagMethod code
  = code
 <> invokevirtual (mkMethodRef stgConstr "getTag" [] (ret jint))

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
      getstatic $ mkFieldRef (moduleJavaClass mainMod) "ZCmain_closure"
                             funType,
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
                 <> sconst (BC.pack s)
                 <> putfield (mkFieldRef rtsConfig "rtsOpts" jstring)
