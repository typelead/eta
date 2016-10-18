{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Foreign where

import GHCVM.Main.DynFlags
import GHCVM.Types.Type
import GHCVM.Types.TyCon
import GHCVM.StgSyn.StgSyn
import GHCVM.Prelude.ForeignCall
import GHCVM.Utils.FastString

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
  | StaticTarget label' _ _ <- target = do
    let labelStr'       = unpackFS label'
        shuffledArgs    = if hasObj then last args : init args else args
        (label, hasObj) = maybe (labelStr', False) (, True) $ stripPrefix "@java " labelStr'
    dflags <- getDynFlags
    argFtCodes <- getNonVoidArgFtCodes shuffledArgs
    let (argFts, callArgs') = unzip argFtCodes
        (isStatic, callTarget) = labelToTarget hasObj label (map fst argFtCodes) resultReps
        callArgs = if hasObj && isStatic then drop 1 callArgs' else callArgs'
        mbObj = if hasObj then Just (head callArgs') else Nothing
    sequel <- getSequel
    case sequel of
      AssignTo targetLocs ->
        emitForeignCall safety mbObj targetLocs callTarget callArgs
      _ -> do
        resLocs <- newUnboxedTupleLocs resType
        emitForeignCall safety mbObj resLocs callTarget callArgs
        emitReturn resLocs
  where resultReps = getUnboxedResultReps resType

labelToTarget :: Bool -> String -> [FieldType] -> [PrimRep] -> (Bool, [Code] -> Code)
labelToTarget hasObj label' argFts reps = (isStatic, result)
  where (label, isStatic) = maybe (label', False) (, True) $ stripPrefix "@static" label'
        result = case words label of
          ["@new"]              -> genNewTarget
          ["@field",label1]     -> genFieldTarget label1
          ["@interface",label1] -> genMethodTarget True label1
          [label1]              -> genMethodTarget False label1
          _                     -> pprPanic "labelToTarget: bad label: " (ppr label')
        -- Remove the passed 'this'
        (thisRep, resRep) =
          if hasObj then
            case reps of
              [a]     -> (a, VoidRep)
              (a:b:_) -> (a, b)
              []      -> (VoidRep, VoidRep)
          else
            case reps of
              [] -> (VoidRep, VoidRep)
              (a:_) -> (VoidRep, a)
        argFts' dropArg = if dropArg then drop 1 argFts else argFts
        genNewTarget =
          let clsName = getObjectClass resRep
              clsFt = obj clsName
          in \args -> new clsFt
                   <> dup clsFt
                   <> fold (if hasObj then drop 1 args else args)
                   <> invokespecial (mkMethodRef clsName "<init>" (argFts' hasObj) void)
        genFieldTarget label =
          let (getInstr, putInstr) = if isStatic
                                     then (getstatic, putstatic)
                                     else (getfield, putfield)
              (clsName, fieldName) =
                if isStatic
                then labelToMethod label
                else (getFtClass (let args = argFts' False
                                  in if length args > 0
                                     then head args
                                     else primRepFieldType resRep), T.pack label)
              (instr, fieldFt) =
                if isVoidRep resRep then
                  (putInstr,
                   if isStatic
                   then head (argFts' hasObj)
                   else head (argFts' True))
                else
                  (getInstr, primRepFieldType resRep)
          in \args -> fold args
                   <> instr (mkFieldRef clsName fieldName fieldFt)
        genMethodTarget isInterface label =
          let instr = if isInterface
                      then invokeinterface
                      else if isStatic
                           then invokestatic
                           else invokevirtual
              (clsName, methodName) =
                if isStatic
                then labelToMethod label
                else (getFtClass (head (argFts' False)), T.pack label)
              resFt = primRepFieldType_maybe resRep
          in \args -> fold args
                   <> instr (mkMethodRef clsName methodName (argFts' (not isStatic)) resFt)

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
