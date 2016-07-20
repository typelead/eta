module GHCVM.CodeGen.Prim where

import Type
import StgSyn
import PrimOp
import Panic

import Codec.JVM

import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Foreign
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Utils

import Data.Monoid ((<>))
import Data.Foldable (fold)

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

cgPrimOp   :: [CgLoc]        -- where to put the results
           -> PrimOp            -- the op
           -> [StgArg]          -- arguments
           -> CodeGen ()
cgPrimOp results op args = do
  argExprs <- getNonVoidArgLoadCodes args
  emitPrimOp results op argExprs

emitPrimOp :: [CgLoc]        -- where to put the results
           -> PrimOp         -- the op
           -> [Code]         -- arguments
           -> CodeGen ()
emitPrimOp [res] op [arg]
  | nopOp op = emitAssign res arg
emitPrimOp r@[res] op args
  | Just execute <- simpleOp op
  = emit $ execute args
emitPrimOp _ _ _ = unimplemented "emitPrimOp"

nopOp :: PrimOp -> Bool
nopOp Int2WordOp = True
nopOp Word2IntOp = True
nopOp _          = False

normalOp :: Code -> [Code] -> Code
normalOp code = (<> code) . fold

simpleOp :: PrimOp -> Maybe ([Code] -> Code)
simpleOp IntAddOp = Just $ normalOp iadd
simpleOp _ = error "simpleOp"
