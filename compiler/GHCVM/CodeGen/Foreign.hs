{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Foreign where

import GHCVM.Main.DynFlags
import GHCVM.Types.Type
import GHCVM.Types.TyCon
import GHCVM.StgSyn.StgSyn
import GHCVM.Prelude.ForeignCall
import GHCVM.Utils.FastString
import GHCVM.Utils.Util
import GHCVM.Util

import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.Types

import GHCVM.Debug
import GHCVM.Util
import Codec.JVM
import Data.Monoid ((<>))
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromJust, isJust, maybe)
import Data.Foldable (fold)
import Control.Monad (when)
import qualified Data.Text as T

cgForeignCall :: ForeignCall -> [StgArg] -> Type -> CodeGen ()
cgForeignCall (CCall (CCallSpec target cconv safety)) args resType
  | StaticTarget label _ _ <- target = do
    let (hasObj, isStatic, callTarget) = deserializeTarget (unpackFS label)
        shuffledArgs = if hasObj then last args : init args else args
    dflags <- getDynFlags
    argFtCodes <- getNonVoidArgFtCodes shuffledArgs
    let (argFts, callArgs') = unzip argFtCodes
        callArgs = if hasObj && isStatic then drop 1 callArgs' else callArgs'
        mbObj = if hasObj then Just (expectHead "cgForiegnCall: empty callArgs'"
                                     callArgs') else Nothing
        mbObjFt = safeHead argFts
    sequel <- getSequel
    case sequel of
      AssignTo targetLocs ->
        emitForeignCall safety mbObj targetLocs (callTarget mbObjFt) callArgs
      _ -> do
        resLocs <- newUnboxedTupleLocs resType
        emitForeignCall safety mbObj resLocs (callTarget mbObjFt) callArgs
        emitReturn resLocs

deserializeTarget :: String -> (Bool, Bool, Maybe FieldType -> [Code] -> Code)
deserializeTarget label = (hasObj, isStatic, callTarget)
  where (hasObj':isStatic':callTargetSpec:_) = split '|' label
        hasObj = read hasObj'
        isStatic = read isStatic'
        (tag:restSpec) = split ',' callTargetSpec

        callTarget = case read tag of
          0 -> genNewTarget restSpec
          1 -> genFieldTarget restSpec
          2 -> genMethodTarget restSpec
          _ -> error $ "deserializeTarget: deserialization failed: " ++ label

        genNewTarget [clsName', methodDesc'] =
          \_ args -> new clsFt
                  <> dup clsFt
                  <> fold args
                  <> invokespecial (mkMethodRef clsName "<init>" argFts void)
          where clsName = read clsName'
                clsFt = obj clsName
                (argFts, _) = expectJust ("deserializeTarget: bad method desc: " ++ label)
                            $ decodeMethodDesc (read methodDesc')

        genFieldTarget [clsName', fieldName', fieldDesc', instr'] =
          \_ args -> fold args
                  <> instr (mkFieldRef clsName fieldName fieldFt)
          where (getInstr, putInstr) = if isStatic
                                     then (getstatic, putstatic)
                                     else (getfield, putfield)
                clsName = read clsName'
                fieldName = read fieldName'
                fieldFt = expectJust ("deserializeTarget: bad field desc: " ++ label)
                        $ decodeFieldDesc (read fieldDesc')
                instr = case read instr' of
                  0 -> putInstr
                  1 -> getInstr
                  _ -> error $ "deserializeTarget: bad instr: " ++ label

        genMethodTarget [isInterface', hasSubclass', clsName', methodName', methodDesc'] =
          \mbObjFt args -> fold args
                        <> instr (mkMethodRef (clsName mbObjFt) methodName argFts resFt)
          where clsName mbObjFt =
                  if hasSubclass
                  then maybe (error "deserializeTarget: no subclass field type.")
                             getFtClass mbObjFt
                  else read clsName'
                methodName = read methodName'
                isInterface = read isInterface'
                hasSubclass = read hasSubclass'
                (argFts, resFt) = expectJust ("deserializeTarget: bad method desc: " ++ label)
                                $ decodeMethodDesc (read methodDesc')
                instr = if isInterface
                        then invokeinterface
                        else if isStatic
                             then invokestatic
                             else invokevirtual

emitForeignCall :: Safety -> Maybe Code -> [CgLoc] -> ([Code] -> Code) -> [Code] -> CodeGen ()
emitForeignCall safety mbObj results target args =
  wrapSafety $ do
    maybe (emit callCode) (flip emitAssign callCode) resLoc
    maybe (return ()) (flip emitAssign (fromJust mbObj)) objLoc
  where wrapSafety code = do
          whenSafe $ emit $ suspendThreadMethod (playInterruptible safety)
          code
          whenSafe $ emit resumeThreadMethod
          where whenSafe = when (playSafe safety)
        callCode = target args
        (resLoc, objLoc) =
          if isJust mbObj then
            case results of
              [a]   -> (Nothing, Just a)
              [a,b] -> (Just b, Just a)
          else
            (case results of
               []  -> Nothing
               [a] -> Just a,
             Nothing)
