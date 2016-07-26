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
import GHCVM.Util

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
  emitReturn [mkLocDirect True $ tagToClosure dflags tyCon code]
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
      -- TODO: Optimize: Remove the intermediate temp locations
      --       and allow direct code locations
      Right codes'
        | ReturnsPrim VoidRep <- resultInfo
        -> --f [] >> emitReturn []
          emitReturn []
        | ReturnsPrim rep <- resultInfo
              -- res <- newTemp rep'
              -- f [res]
              -- emitReturn [res]
        -> do let rep' = mkJPrimRep rep
              -- Assumes Returns Prim is of Non-closure type
              codes <- codes'
              emitReturn
                . (:[])
                . mkLocDirect False
                $ ( expectJust "cgOpApp: StgPrimOp"
                  . primRepFieldType_maybe
                  $ rep'
                  , (head codes))
        | ReturnsAlg tyCon <- resultInfo, isUnboxedTupleTyCon tyCon
        -> do -- locs <- newUnboxedTupleLocs resType
              -- f locs
              codes <- codes'
              let UbxTupleRep tyArgs         = repType resType
                  reps = [ (isPtrJRep rep, rep)
                         | ty <- tyArgs
                         , let rep           = typeJPrimRep ty
                         , not (isVoidJRep rep) ]
              emitReturn . map (\((isClosure, rep), code)
                                -> mkLocDirect isClosure
                                   (primRepFieldType rep
                                   , code))
                         $ zip reps codes
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

shouldInlinePrimOp :: DynFlags -> PrimOp -> [Code] -> Either Code (CodeGen [Code])
-- TODO: Inline array operations conditionally
shouldInlinePrimOp dflags primOp args
  | primOpOutOfLine primOp = Left $ mkRtsPrimOp primOp
  | otherwise = Right $ emitPrimOp primOp args

mkRtsPrimOp :: PrimOp -> Code
mkRtsPrimOp primop = pprPanic "mkRtsPrimOp: unimplemented!" (ppr primop)

cgPrimOp   :: PrimOp            -- the op
           -> [StgArg]          -- arguments
           -> CodeGen [Code]
cgPrimOp op args = do
  argExprs <- getNonVoidArgLoadCodes args
  emitPrimOp op argExprs

-- emitPrimOp :: [CgLoc]        -- where to put the results
--            -> PrimOp         -- the op
--            -> [Code]         -- arguments
--            -> CodeGen ()
emitPrimOp :: PrimOp -> [Code] -> CodeGen [Code]
emitPrimOp DataToTagOp [arg] = return [getTagMethod arg]

emitPrimOp IntQuotRemOp args = do
  codes1 <- emitPrimOp IntQuotOp args
  codes2 <- emitPrimOp IntRemOp args
  return $ codes1 ++ codes2

emitPrimOp op [arg]
  | nopOp op = return [mempty] -- TODO: Verify this is OK
emitPrimOp op args
  | Just execute <- simpleOp op
  = return [execute args]
emitPrimOp op _ = pprPanic "emitPrimOp: unimplemented" (ppr op)

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
simpleOp ISllOp = Just $ normalOp ishl
simpleOp ISraOp = Just $ normalOp ishr
simpleOp ISrlOp = Just $ normalOp iushr

simpleOp IntNegOp = Just $ normalOp ineg
simpleOp IntEqOp = Just $ intCompOp if_icmpeq
simpleOp IntNeOp = Just $ intCompOp if_icmpne
simpleOp IntLeOp = Just $ intCompOp if_icmple
simpleOp IntLtOp = Just $ intCompOp if_icmplt
simpleOp IntGeOp = Just $ intCompOp if_icmpge
simpleOp IntGtOp = Just $ intCompOp if_icmpgt

-- Word# ops
-- TODO: Take a look at compareUnsigned in JDK 8
--       and see if that's more efficient
simpleOp WordEqOp   = Just $ intCompOp if_icmpeq
simpleOp WordNeOp   = Just $ intCompOp if_icmpeq
simpleOp WordAddOp  = Just $ normalOp iadd
simpleOp WordSubOp  = Just $ normalOp isub
simpleOp WordMulOp  = Just $ normalOp imul
simpleOp WordQuotOp = Just $ unsignedOp ldiv
simpleOp WordRemOp  = Just $ unsignedOp lrem
simpleOp WordGtOp   = Just $ unsignedCmp ifgt
simpleOp WordGeOp   = Just $ unsignedCmp ifge
simpleOp WordLeOp   = Just $ unsignedCmp ifle
simpleOp WordLtOp   = Just $ unsignedCmp iflt
--Verify true for unsigned operations
simpleOp AndOp = Just $ normalOp iand
simpleOp OrOp = Just $ normalOp ior
simpleOp XorOp = Just $ normalOp ixor
simpleOp NotOp = Just $ normalOp inot
simpleOp SllOp = Just $ normalOp ishl
simpleOp SrlOp = Just $ normalOp iushr

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

unsignedOp :: Code -> [Code] -> Code
unsignedOp op [arg1, arg2]
  = unsignedExtend arg1
 <> unsignedExtend arg2
 <> ldiv
 <> gconv jlong jint

typedCmp :: FieldType -> (Code -> Code -> Code) -> [Code] -> Code
typedCmp ft ifop [arg1, arg2]
  = gcmp ft arg1 arg2
 <> ifop (iconst jint 1) (iconst jint 0)

unsignedCmp :: (Code -> Code -> Code) -> [Code] -> Code
unsignedCmp ifop [arg1, arg2]
  = typedCmp jlong ifop [unsignedExtend arg1, unsignedExtend arg2]

unsignedExtend :: Code -> Code
unsignedExtend i = i <> gconv jint jlong <> lconst 0xFFFFFFFF
