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
import GHCVM.CodeGen.Name
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
cgOpApp (StgPrimCallOp (PrimCall label _)) args resType = do
  locs <- newUnboxedTupleLocs resType
  args' <- getFtsLoadCode args
  emit $ mkCallExit True args'
      <> loadContext
      <> invokestatic (mkMethodRef clsName methodName [contextType] void)
      <> mkReturnEntry locs
  where (clsName, methodName) = labelToMethod label

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
  = emitAssign res (execute args)
emitPrimOp _ op _ = pprPanic "emitPrimOp: unimplemented" (ppr op)

nopOp :: PrimOp -> Bool
nopOp Int2WordOp = True
nopOp Word2IntOp = True
nopOp OrdOp      = True
nopOp ChrOp      = True
nopOp _          = False

normalOp :: Code -> [Code] -> Code
normalOp code = (<> code) . fold

intCompOp :: (Code -> Code -> Code) -> [Code] -> Code
intCompOp op args = fold args <> op (iconst jint 1) (iconst jint 0)

simpleOp :: PrimOp -> Maybe ([Code] -> Code)
simpleOp IntAddOp = Just $ normalOp iadd
simpleOp IntEqOp = Just $ intCompOp if_icmpeq
simpleOp IntNeOp = Just $ intCompOp if_icmpne
simpleOp IntLeOp = Just $ intCompOp if_icmple
simpleOp IntLtOp = Just $ intCompOp if_icmplt
simpleOp IntGeOp = Just $ intCompOp if_icmpge
simpleOp IntGtOp = Just $ intCompOp if_icmpgt
-- simpleOp IntQuotOp = Just $ intCompOp idiv
simpleOp CharEqOp = Just $ intCompOp if_icmpeq
simpleOp CharNeOp = Just $ intCompOp if_icmpne
-- TODO: Chars are unsigned so the ordering comparison is wrong
--       figure out how to handle unsigned comparisons
simpleOp CharLeOp = Just $ intCompOp if_icmple
simpleOp CharLtOp = Just $ intCompOp if_icmplt
simpleOp _ = Nothing
