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
import GHCVM.CodeGen.Types

import GHCVM.Debug
import GHCVM.Util
import Codec.JVM
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Foldable (fold)
import qualified Data.Text as T

cgForeignCall :: ForeignCall -> [StgArg] -> Type -> CodeGen ()
cgForeignCall (CCall (CCallSpec target cconv safety)) args resType = do
  debugDoc $ str "cgForeignCall:" <+> ppr args <+> ppr resType
  dflags <- getDynFlags
  argFtCodes <- getFCallArgs shuffledArgs
  let (argFts, callArgs) = unzip argFtCodes
      callTarget = case target of
        StaticTarget label _ _ -> labelToTarget maybeObjArg (unpackFS label)
                                                (map fst argFtCodes) resultReps
        _ -> panic "cgForeignCall: unimplemented"
  sequel <- getSequel
  case sequel of
    AssignTo targetLocs ->
      emitForeignCall safety targetLocs callTarget callArgs
    _ -> do
      resLocs <- newUnboxedTupleLocs resType
      emitForeignCall safety resLocs callTarget callArgs
      emitReturn resLocs
  where maybeObjArg =
          if length args >= 2 then
               (isObjectRep . typePrimRep . stgArgType $ lastArg)
            && (isVoidRep   . typePrimRep . stgArgType $ penArg)
          else False
        (initArgs, penArg, lastArg) = initLast2 args
        shuffledArgs = if maybeObjArg then lastArg : initArgs else args
        resultReps = getUnboxedResultReps resType
        initLast2 = go []
          where go res [x, y] = (reverse (x:res), x, y)
                go res (x:xs) = go (x:res) xs

labelToTarget :: Bool -> String -> [FieldType] -> [PrimRep] -> Code -> Code
labelToTarget hasObj label argFts reps = case words label of
  ("static":label1) ->
    case label1 of
      ["@new"] ->
        let clsName = getObjectClass thisRep
            clsFt = obj clsName
        in \c -> new clsFt
              <> dup clsFt
              <> c
              <> invokespecial (mkMethodRef clsName "<init>" argFts' void)
      ["@field",label] ->
        let (clsName, fieldName) = labelToMethod label
            (instr, fieldFt) =
              if isVoidRep resRep then
                (putstatic, head argFts')
              else
                (getstatic, primRepFieldType resRep)
        in \c -> c <> instr (mkFieldRef clsName fieldName fieldFt)
      [label] ->
        let (clsName, methodName) = labelToMethod label
            resFt = primRepFieldType_maybe resRep
        in \c -> c <> invokestatic (mkMethodRef clsName methodName argFts' resFt)
      _ -> pprPanic "labelToTarget: static label: " (ppr label1)
  (clsName':label2) ->
    let clsName = T.pack clsName'
    in case label2 of
      ["@field",fieldName'] ->
        let fieldName = T.pack fieldName'
            (instr, fieldFt) =
              if isVoidRep resRep then
                (putfield, head argFts')
              else
                (getfield, primRepFieldType resRep)
        in \c -> c <> instr (mkFieldRef clsName fieldName fieldFt)
      [methodName'] ->
        let methodName = T.pack methodName'
            resFt      = primRepFieldType_maybe resRep
        in \c -> c <> invokevirtual (mkMethodRef clsName methodName argFts' resFt)
      _ -> pprPanic "labelToTarget: instance label: " (ppr label2)
  _ -> pprPanic "labelToTarget: full: " (ppr label)
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
        argFts' = if hasObj then drop 1 argFts else argFts

emitForeignCall :: Safety -> [CgLoc] -> (Code -> Code) -> [Code] -> CodeGen ()
emitForeignCall safety results target args
  | not (playSafe safety) =
      -- NOTE: We assume that the running Java code WILL NOT change the context
      --      which will be true in almost all cases
      -- TODO: Only works for static calls right now
      if null results then emit callCode
      else emitAssign (head results) callCode
  | otherwise = pprPanic "emitForeignCall: Safety not implemented" (ppr safety)
  where callCode = target $ fold args

getFCallArgs :: [StgArg] -> CodeGen [(FieldType, Code)]
getFCallArgs args = do
  maybeFtCodes <- mapM get args
  return $ catMaybes maybeFtCodes
  where get arg
          | isVoidRep argRep = return Nothing
          | otherwise = do
              argCode <- getArgLoadCode (NonVoid arg)
              -- TODO: Add shims for special cases
              return $ Just (argFt, argCode)
          where argTy  = stgArgType arg
                argRep = typePrimRep argTy
                argFt  = expectJust "getFCallArgs"
                       $ primRepFieldType_maybe argRep
