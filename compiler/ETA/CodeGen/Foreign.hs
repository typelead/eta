{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Foreign where

import ETA.Types.Type
import ETA.StgSyn.StgSyn
import ETA.Prelude.ForeignCall
import ETA.Utils.FastString
import ETA.Utils.Util
import ETA.Util

import ETA.CodeGen.Env
import ETA.CodeGen.Monad
import ETA.CodeGen.Layout
import ETA.CodeGen.Rts
import ETA.CodeGen.Types

import Codec.JVM
import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust)
import Data.Foldable (fold)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

cgForeignCall :: ForeignCall -> [StgArg] -> Type -> CodeGen ()
cgForeignCall (CCall (CCallSpec target _cconv safety)) args resType
  | StaticTarget label _ isRef <- target = do
    sequel <- getSequel
    if isRef
    then do
      let (clsName, methodName, argFts) = deserializeMethodDesc (unpackFS label)
          arrayFt = jarray classFt
          grabResLoc = do
            case sequel of
              AssignTo (resLoc:_) -> return resLoc
              _                   -> fmap head $ newUnboxedTupleLocs resType
      resLoc <- grabResLoc
      emitAssign resLoc $
           sconst (T.replace "/" "." clsName)
        <> invokestatic (mkMethodRef classType "forName" [jstring] (ret classFt))
        <> sconst methodName
        <> iconst jint (fromIntegral (length argFts))
        <> new arrayFt
        <> fold (map (\(i, ft) ->
                         dup arrayFt
                      <> iconst jint i
                      <> ftClassObject ft
                      <> gastore classFt)
                  $ zip [0..] argFts)
        <> invokevirtual (mkMethodRef classType "getMethod" [jstring, arrayFt]
                          (ret methodFt))
        <> invokestatic (mkMethodRef "eta/runtime/stg/FunPtr" "registerFunPtr"
                         [methodFt] (ret jlong))
      case sequel of
        Return -> emitReturn [resLoc]
        _      -> return ()
    else do
      let (hasObj, isStatic, callTarget) = deserializeTarget (unpackFS label)
          shuffledArgs = if hasObj then last args : init args else args
      argFtCodes <- getNonVoidArgFtCodes shuffledArgs
      let (argFts, callArgs') = unzip argFtCodes
          callArgs = if hasObj && isStatic then drop 1 callArgs' else callArgs'
          mbObj = if hasObj then Just (expectHead "cgForiegnCall: empty callArgs'"
                                      callArgs') else Nothing
          mbObjFt = safeHead argFts
      case sequel of
        AssignTo targetLocs ->
          emitForeignCall safety mbObj targetLocs (callTarget mbObjFt) callArgs
        _ -> do
          resLocs <- newUnboxedTupleLocs resType
          emitForeignCall safety mbObj resLocs (callTarget mbObjFt) callArgs
          emitReturn resLocs
cgForeignCall _ _ _ = error $ "cgForeignCall: bad arguments"

deserializeMethodDesc :: String -> (Text, Text, [FieldType])
deserializeMethodDesc label = (read clsName', read methodName', argFts)
  where (_:_:callTargetSpec:_) = split '|' label
        (_:_:_:_:clsName':methodName':methodDesc':_) = split ',' callTargetSpec
        (argFts, _) = expectJust ("deserializeTarget: bad method desc: " ++ label ++ " " ++ methodDesc')
                    $ decodeMethodDesc (read methodDesc')

deserializeTarget :: String -> (Bool, Bool, Maybe FieldType -> [Code] -> Code)
deserializeTarget label = (hasObj, isStatic, callTarget)
  where (hasObj':isStatic':callTargetSpec:_) = split '|' label
        hasObj = read hasObj'
        isStatic = read isStatic'
        (tag:restSpec) = split ',' callTargetSpec

        callTarget = case read tag :: Int of
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
                (argFts, _) = expectJust ("deserializeTarget: bad method desc: " ++ label ++ " " ++ methodDesc')
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
                instr = case read instr' :: Int of
                  0 -> putInstr
                  1 -> getInstr
                  _ -> error $ "deserializeTarget: bad instr: " ++ label

        genMethodTarget [isInterface', isSuper', hasSubclass', clsName', methodName', methodDesc'] =
          \mbObjFt args -> foldedArgs mbObjFt args
                        <> instr (mkMethodRef (snd $ clsName mbObjFt) methodName argFts resFt)
          where clsName mbObjFt =
                  if hasSubclass && not (isInterface || isSuper)
                  then let cls' = maybe (error "deserializeTarget: no subclass field type.") getFtClass mbObjFt
                       in if (cls' == jobjectC) && (className /= jobjectC)
                          then (True, className)
                          else (False, cls')
                  else (False, className)
                foldedArgs mbObjFt args
                  | (doCast, clsName') <- clsName mbObjFt
                  , let (thisArg:restArgs) = args
                        maybeConv
                          | doCast    = gconv jobject (obj clsName')
                          | otherwise = mempty
                  = thisArg <> maybeConv <> fold restArgs
                className  = read clsName'
                methodName = read methodName'
                isInterface = read isInterface'
                isSuper = read isSuper'
                hasSubclass = read hasSubclass'
                (argFts, resFt) = expectJust ("deserializeTarget: bad method desc: " ++ label ++ " " ++ methodDesc')
                                $ decodeMethodDesc (read methodDesc')
                instr = if isInterface
                        then invokeinterface
                        else if isStatic
                             then invokestatic
                             else invokevirtual

emitForeignCall :: Safety -> Maybe Code -> [CgLoc] -> ([Code] -> Code) -> [Code] -> CodeGen ()
emitForeignCall safety mbObj results target args = do
  loadContext <- getContextLoc
  wrapSafety loadContext $ do
    maybe (emit callCode) (flip emitAssign callCode) resLoc
    maybe (return ()) (flip emitAssign (fromJust mbObj)) objLoc
  where -- As of now, interruptible & unsafe mean pretty much the same thing.
        wrapSafety loadContext code = do
          whenSafe $ emit $ suspendInterruptsMethod loadContext (playInterruptible safety)
          _ <- code
          whenSafe $ emit resumeInterruptsMethod
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
