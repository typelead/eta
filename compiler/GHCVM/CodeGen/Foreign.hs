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
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Foldable (fold)
import Control.Monad (when)
import qualified Data.Text as T

cgForeignCall :: ForeignCall -> [StgArg] -> Type -> CodeGen ()
cgForeignCall (CCall (CCallSpec target cconv safety)) args resType = do
  debugDoc $ str "cgForeignCall:" <+> ppr args <+> ppr resType
  dflags <- getDynFlags
  argFtCodes <- getNonVoidArgFtCodes shuffledArgs
  let (argFts, callArgs') = unzip argFtCodes
      (isStatic, callTarget) = case target of
        StaticTarget label _ _ -> labelToTarget hasObj (unpackFS label)
                                                (map fst argFtCodes) resultReps
        _ -> panic "cgForeignCall: unimplemented"
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
  where hasObj =
          if length args >= 2 then
               (isObjectRep . typePrimRep . stgArgType $ lastArg)
            && (isVoidRep   . typePrimRep . stgArgType $ penArg)
          else False
        (initArgs, penArg, lastArg) = initLast2 args
        shuffledArgs = if hasObj then lastArg : initArgs else args
        resultReps = getUnboxedResultReps resType
        initLast2 = go []
          where go res [x, y] = (reverse (x:res), x, y)
                go res (x:xs) = go (x:res) xs

labelToTarget :: Bool -> String -> [FieldType] -> [PrimRep] -> (Bool, Code -> Code)
labelToTarget hasObj label argFts reps = case words label of
  ("@static":label1) ->
    let isStatic = True
    in ( isStatic,
         case label1 of
           ["@new"] -> genNewTarget isStatic (getObjectClass resRep)
           ["@field",label] -> genFieldTarget isStatic label getstatic putstatic
           [label] -> genMethodTarget isStatic label invokestatic
           _ -> pprPanic "labelToTarget: static label: " (ppr label1))
  label2 ->
    let notStatic = False
    in ( notStatic
       , case label2 of
           ["@field",label] -> genFieldTarget notStatic label getfield putfield
           ["@interface",label] -> genMethodTarget notStatic label invokeinterface
           ["@new",label] -> genNewTarget notStatic (T.pack label)
           [label] -> genMethodTarget notStatic label invokevirtual
           _ -> pprPanic "labelToTarget: instance label: " (ppr label2) )
  where (thisRep, resRep) =
          if hasObj then
            case reps of
              [a]     -> (a, VoidRep)
              (a:b:_) -> (a, b)
              []      -> (VoidRep, VoidRep)
          else
            case reps of
              [] -> (VoidRep, VoidRep)
              (a:_) -> (VoidRep, a)
        -- Remove the passed 'this'
        argFts' isStatic = if isStatic then argFts else drop 1 argFts
        genNewTarget isStatic clsName =
          let clsFt = obj clsName
          in \c -> new clsFt
                <> dup clsFt
                <> c
                <> invokespecial (mkMethodRef clsName "<init>" (argFts' isStatic) void)
        genFieldTarget isStatic label getInstr putInstr =
          let (clsName, fieldName) = labelToMethod label
              (instr, fieldFt) =
                if isVoidRep resRep then
                  (putInstr, head (argFts' isStatic))
                else
                  (getInstr, primRepFieldType resRep)
          in \c -> c <> instr (mkFieldRef clsName fieldName fieldFt)
        genMethodTarget isStatic label instr =
          let (clsName, methodName) = labelToMethod label
              resFt = primRepFieldType_maybe resRep
          in \c -> c <> instr (mkMethodRef clsName methodName (argFts' isStatic) resFt)

emitForeignCall :: Safety -> Maybe Code -> [CgLoc] -> (Code -> Code) -> [Code] -> CodeGen ()
emitForeignCall safety mbObj results target args =
  wrapSafety $ do
    maybe (emit callCode) (flip emitAssign callCode) resLoc
    maybe (return ()) (flip emitAssign (fromJust mbObj)) objLoc
  where wrapSafety code = do
          whenSafe $ emit $ suspendThreadMethod (playInterruptible safety)
          code
          whenSafe $ emit resumeThreadMethod
          where whenSafe = when (playSafe safety)
        callCode = target $ fold args
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
