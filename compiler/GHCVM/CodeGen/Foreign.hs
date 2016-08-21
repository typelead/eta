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
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Foldable (fold)
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
  ("static":label1) -> (True,
    case label1 of
      ["@new"] ->
        -- TODO: Accomodate creation of array types
        let clsName = getObjectClass resRep
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
      _ -> pprPanic "labelToTarget: static label: " (ppr label1))
  (clsName':label2) -> (False,
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
      _ -> pprPanic "labelToTarget: instance label: " (ppr label2))
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

emitForeignCall :: Safety -> Maybe Code -> [CgLoc] -> (Code -> Code) -> [Code] -> CodeGen ()
emitForeignCall safety mbObj results target args
  | not (playSafe safety) = do
      -- NOTE: We assume that the running Java code WILL NOT change the context
      --       which will be true in almost all cases
      maybe (emit callCode) (flip emitAssign callCode) resLoc
      maybe (return ()) (flip emitAssign (fromJust mbObj)) objLoc
  | otherwise = pprPanic "emitForeignCall: Safety not implemented" (ppr safety)
  where callCode = target $ fold args
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
