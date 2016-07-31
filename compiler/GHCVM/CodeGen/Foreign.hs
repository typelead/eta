module GHCVM.CodeGen.Foreign where

import GHCVM.Main.DynFlags
import GHCVM.Types.Type
import GHCVM.StgSyn.StgSyn
import GHCVM.Prelude.ForeignCall

import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.Primitive
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
  argFtCodes <- getFCallArgs args
  resLocs <- newUnboxedTupleLocs resType
  let (argFts, callArgs) = unzip argFtCodes
      callTarget = case target of
        StaticTarget label mPkgId True ->
          let (clsName, methodName) = labelToMethod label
          in invokestatic $ mkMethodRef clsName methodName argFts
                                        ( maybe void (ret . locFt)
                                        $ safeHead resLocs )
             -- TODO: Verify the result
        _ -> panic "cgForeignCall: unimplemented"
  sequel <- getSequel
  case sequel of
    AssignTo targetLocs ->
      emitForeignCall safety targetLocs callTarget callArgs
    _ -> do
      emitForeignCall safety resLocs callTarget callArgs
      emitReturn resLocs

emitForeignCall :: Safety -> [CgLoc] -> Code -> [Code] -> CodeGen ()
emitForeignCall safety results target args
  | not (playSafe safety) =
      -- NOTE: We assume that the running Java code WILL NOT change the context
      --      which will be true in almost all cases
      -- TODO: Only works for static calls right now
      if null results then emit callCode
      else emitAssign (head results) callCode
  | otherwise = pprPanic "emitForeignCall: Safety not implemented" (ppr safety)
  where callCode = fold args
                <> target

getFCallArgs :: [StgArg] -> CodeGen [(FieldType, Code)]
getFCallArgs args = do
  maybeFtCodes <- mapM get args
  return $ catMaybes maybeFtCodes
  where get arg
          | isVoidJRep argRep = return Nothing
          | otherwise = do
              argCode <- getArgLoadCode (NonVoid arg)
              -- TODO: Add shims for special cases
              return $ Just (argFt, argCode)
          where argTy  = stgArgType arg
                argRep = typeJPrimRep argTy
                argFt  = expectJust "getFCallArgs"
                       $ primRepFieldType_maybe argRep
