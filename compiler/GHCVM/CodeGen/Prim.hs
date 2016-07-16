module GHCVM.CodeGen.Prim where

import Type
import StgSyn
import PrimOp
import Panic

import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Foreign
import GHCVM.CodeGen.Env

cgOpApp :: StgOp
        -> [StgArg]
        -> Type
        -> CodeGen ()
cgOpApp (StgFCallOp fcall _) args resType = cgForeignCall fcall args resType
cgOpApp (StgPrimOp TagToEnumOp) args@[arg] resType = do
  dflags <- getDynFlags
  codes <- getNonVoidArgLoadCodes args
  let code = case codes of
        [code'] -> code'
        _ -> panic "TagToEnumOp had void arg"
  -- TODO: Do a lookup in the closure table for enum tycons
  --       Add code to generate the table for enum tycons
  unimplemented "cgOpApp: TagToEnumUp"
  where tycon = tyConAppTyCon resType

cgOpApp (StgPrimOp primop) args resType = do
    dflags <- getDynFlags
    codes <- getNonVoidArgLoadCodes args
    -- TODO: Implement after foreign calls
    unimplemented "cgOpApp: StgPrimOp"

-- NOTE: The GHCVM specific primops will get handled here
cgOpApp (StgPrimCallOp primcall) args resType = do
  codes <- getNonVoidArgLoadCodes args
  unimplemented "cgOpApp: StgPrimOp"
