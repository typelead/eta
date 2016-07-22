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
  emitReturn [mkLocDirect True $ tagToClosure tyCon code]
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
        -> do let rep' = mkJPrimRep rep
              res <- newTemp rep'
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
-- Int# ops
simpleOp IntAddOp = Just $ normalOp iadd
simpleOp IntSubOp = Just $ normalOp isub
simpleOp IntMulOp = Just $ normalOp imul
simpleOp IntQuotOp = Just $ normalOp idiv
simpleOp IntRemOp = Just $ normalOp irem
simpleOp AndIOp = Just $ normalOp iand
simpleOp OrIOp = Just $ normalOp ior
simpleOp XorIOp = Just $ normalOp ixor
simpleOp NotIOp = Just $ normalOp inot
simpleOp IntNegOp = Just $ normalOp ineg
simpleOp IntEqOp = Just $ intCompOp if_icmpeq
simpleOp IntNeOp = Just $ intCompOp if_icmpne
simpleOp IntLeOp = Just $ intCompOp if_icmple
simpleOp IntLtOp = Just $ intCompOp if_icmplt
simpleOp IntGeOp = Just $ intCompOp if_icmpge
simpleOp IntGtOp = Just $ intCompOp if_icmpgt
-- simpleOp IntQuotOp = Just $ intCompOp idiv

-- Char# ops
simpleOp CharEqOp = Just $ intCompOp if_icmpeq
simpleOp CharNeOp = Just $ intCompOp if_icmpne
simpleOp CharGtOp = Just $ unsignedCmp ifgt
simpleOp CharGeOp = Just $ unsignedCmp ifge
simpleOp CharLeOp = Just $ unsignedCmp ifle
simpleOp CharLtOp = Just $ unsignedCmp iflt

-- Double# ops
simpleOp DoubleEqOp = Just $ typedCmp jdouble ifeq
simpleOp DoubleNeOp = Just $ typedCmp jdouble ifne
simpleOp DoubleGeOp = Just $ typedCmp jdouble ifge
simpleOp DoubleLeOp = Just $ typedCmp jdouble ifle
simpleOp DoubleGtOp = Just $ typedCmp jdouble ifgt
simpleOp DoubleLtOp = Just $ typedCmp jdouble iflt

simpleOp DoubleAddOp = Just $ normalOp dadd
simpleOp DoubleSubOp = Just $ normalOp dsub
simpleOp DoubleMulOp = Just $ normalOp dmul
simpleOp DoubleDivOp = Just $ normalOp ddiv
simpleOp DoubleNegOp = Just $ normalOp dneg

-- Float# ops
simpleOp FloatEqOp = Just $ typedCmp jfloat ifeq
simpleOp FloatNeOp = Just $ typedCmp jfloat ifne
simpleOp FloatGeOp = Just $ typedCmp jfloat ifge
simpleOp FloatLeOp = Just $ typedCmp jfloat ifle
simpleOp FloatGtOp = Just $ typedCmp jfloat ifgt
simpleOp FloatLtOp = Just $ typedCmp jfloat iflt

simpleOp FloatAddOp = Just $ normalOp fadd
simpleOp FloatSubOp = Just $ normalOp fsub
simpleOp FloatMulOp = Just $ normalOp fmul
simpleOp FloatDivOp = Just $ normalOp fdiv
simpleOp FloatNegOp = Just $ normalOp fneg

simpleOp _ = Nothing

typedCmp :: FieldType -> (Code -> Code -> Code) -> [Code] -> Code
typedCmp ft ifop [arg1, arg2]
  = gcmp ft arg1 arg2
 <> ifop (iconst jint 1) (iconst jint 0)

unsignedCmp :: (Code -> Code -> Code) -> [Code] -> Code
unsignedCmp ifop [arg1, arg2]
  = typedCmp jlong ifop [unsignedExtend arg1, unsignedExtend arg2]

unsignedExtend :: Code -> Code
unsignedExtend i = i <> gconv jint jlong <> lconst 0xFFFFFFFF
