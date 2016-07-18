module GHCVM.CodeGen.Prim where

import Type
import StgSyn
import PrimOp
import Panic

import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Foreign
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Utils

cgOpApp :: StgOp
        -> [StgArg]
        -> Type
        -> CodeGen ()
cgOpApp (StgFCallOp fcall _) args resType = cgForeignCall fcall args resType
-- TODO: Is this primop necessary like in GHC?
cgOpApp (StgPrimOp TagToEnumOp) args@[arg] resType = do
  dflags <- getDynFlags
  codes <- getNonVoidArgLoadCodes args
  let code = case codes of
        [code'] -> code'
        _ -> panic "TagToEnumOp had void arg"
  emitReturn [mkLocDirect $ tagToClosure tyCon code]
  where tyCon = tyConAppTyCon resType

cgOpApp (StgPrimOp primop) args resType = do
    dflags <- getDynFlags
    codes <- getNonVoidArgLoadCodes args
    -- TODO: Implement after foreign calls
    unimplemented "cgOpApp: StgPrimOp"

-- NOTE: The GHCVM specific primops will get handled here
cgOpApp (StgPrimCallOp primcall) args resType = do
  codes <- getNonVoidArgLoadCodes args
  unimplemented "cgOpApp: StgPrimOp"
