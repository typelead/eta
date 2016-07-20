module GHCVM.CodeGen.Prim where

import DynFlags
import TyCon
import Type
import StgSyn
import PrimOp
import Panic

import Codec.JVM

import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Foreign
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Layout
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Utils
import GHCVM.CodeGen.Rts
import GHCVM.Primitive
import GHCVM.Debug

import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Maybe (fromJust)

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

cgOpApp (StgPrimOp primOp) args resType = do
    dflags <- getDynFlags
    argCodes <- getNonVoidArgLoadCodes args
    case shouldInlinePrimOp dflags primOp argCodes of
      Left rtsPrimOpCode -> do
        args' <- getFtsLoadCode args
        emit $ mkCallExit True args'
            <> loadContext
            <> rtsPrimOpCode
      Right f
        | ReturnsPrim VoidRep <- resultInfo
        -> f [] >> emitReturn []
        | ReturnsPrim rep <- resultInfo
        -> do let ft = fromJust . primRepFieldType $ mkJPrimRep rep
              res <- newTemp ft
              f [res]
              emitReturn [res]
        | ReturnsAlg tyCon <- resultInfo, isUnboxedTupleTyCon tyCon
        -> do locs <- newUnboxedTupleLocs resType
              f locs
              emitReturn locs
        | otherwise -> panic "cgPrimOp"
        where resultInfo = getPrimOpResultInfo primOp

-- NOTE: The GHCVM specific primops will get handled here
cgOpApp (StgPrimCallOp (PrimCall label pkgKey)) args resType = do
  pprPanic "cgOpApp: PrimCall: label, pkgKey:" $ ppr label <+> ppr pkgKey
  args' <- getFtsLoadCode args
  emit $ mkCallExit True args'
      <> loadContext
      <> undefined

shouldInlinePrimOp :: DynFlags -> PrimOp -> [Code]
  -> Either Code ([CgLoc] -> CodeGen ())
-- TODO: Inline array operations conditionally
shouldInlinePrimOp dflags primOp args
  | primOpOutOfLine primOp = Left $ mkRtsPrimOp primOp
  | otherwise = Right $ \locs -> emitPrimOp locs primOp args

mkRtsPrimOp :: PrimOp -> Code
mkRtsPrimOp primop = pprPanic "mkRtsPrimOp: unimplemented!" (ppr primop)

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
emitPrimOp _ op _ = pprPanic "emitPrimOp: unimplemented" (ppr op)

nopOp :: PrimOp -> Bool
nopOp Int2WordOp = True
nopOp Word2IntOp = True
nopOp _          = False

normalOp :: Code -> [Code] -> Code
normalOp code = (<> code) . fold

simpleOp :: PrimOp -> Maybe ([Code] -> Code)
simpleOp IntAddOp = Just $ normalOp iadd
simpleOp _ = error "simpleOp"
