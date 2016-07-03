module GHCVM.Primitive where

import ForeignCall(CType(..))
import Outputable
import Type
import TypeRep
import Unique
import FastString
import TyCon
import PrelInfo
import BasicTypes
import Name
import Id
import Avail
import PrelNames (gHC_PRIM)
import Data.Maybe
import HscTypes
import PrimOp
import TysPrim
import MkId

import Data.Text (Text)
import GHCVM.CodeGen.Name

ghcvmPrimIface :: ModIface
ghcvmPrimIface = (emptyModIface gHC_PRIM) {
  mi_exports = ghcvmPrimExports, -- TODO: Change
  mi_decls    = [],
  mi_fixities = fixities,
  mi_fix_fn  = mkIfaceFixCache fixities }
  where
    fixities = (getOccName seqId, Fixity 0 InfixR)  -- seq is infixr 0
             : mapMaybe mkFixity ghcvmPrimOps
    mkFixity op = (,) (primOpOcc op) <$> primOpFixity op

-- TODO: Comment out all primops that are not implemented
ghcvmPrimOps =
  [  CharGtOp
   , CharGeOp
   , CharEqOp
   , CharNeOp
   , CharLtOp
   , CharLeOp
   , OrdOp
   , IntAddOp
   , IntSubOp
   , IntMulOp
   , IntMulMayOfloOp
   , IntQuotOp
   , IntRemOp
   , IntQuotRemOp
   , AndIOp
   , OrIOp
   , XorIOp
   , NotIOp
   , IntNegOp
   , IntAddCOp
   , IntSubCOp
   , IntGtOp
   , IntGeOp
   , IntEqOp
   , IntNeOp
   , IntLtOp
   , IntLeOp
   , ChrOp
   , Int2WordOp
   , Int2FloatOp
   , Int2DoubleOp
   , Word2FloatOp
   , Word2DoubleOp
   , ISllOp
   , ISraOp
   , ISrlOp
   , WordAddOp
   --, WordSubCOp TODO: Why not is scope?
   , WordAdd2Op
   , WordSubOp
   , WordMulOp
   , WordMul2Op
   , WordQuotOp
   , WordRemOp
   , WordQuotRemOp
   , WordQuotRem2Op
   , AndOp
   , OrOp
   , XorOp
   , NotOp
   , SllOp
   , SrlOp
   , Word2IntOp
   , WordGtOp
   , WordGeOp
   , WordEqOp
   , WordNeOp
   , WordLtOp
   , WordLeOp
   , PopCnt8Op
   , PopCnt16Op
   , PopCnt32Op
   , PopCnt64Op
   , PopCntOp
   , Clz8Op
   , Clz16Op
   , Clz32Op
   , Clz64Op
   , ClzOp
   , Ctz8Op
   , Ctz16Op
   , Ctz32Op
   , Ctz64Op
   , CtzOp
   , BSwap16Op
   , BSwap32Op
   , BSwap64Op
   , BSwapOp
   , Narrow8IntOp
   , Narrow16IntOp
   , Narrow32IntOp
   , Narrow8WordOp
   , Narrow16WordOp
   , Narrow32WordOp
   , DoubleGtOp
   , DoubleGeOp
   , DoubleEqOp
   , DoubleNeOp
   , DoubleLtOp
   , DoubleLeOp
   , DoubleAddOp
   , DoubleSubOp
   , DoubleMulOp
   , DoubleDivOp
   , DoubleNegOp
   , Double2IntOp
   , Double2FloatOp
   , DoubleExpOp
   , DoubleLogOp
   , DoubleSqrtOp
   , DoubleSinOp
   , DoubleCosOp
   , DoubleTanOp
   , DoubleAsinOp
   , DoubleAcosOp
   , DoubleAtanOp
   , DoubleSinhOp
   , DoubleCoshOp
   , DoubleTanhOp
   , DoublePowerOp
   , DoubleDecode_2IntOp
   , DoubleDecode_Int64Op
   , FloatGtOp
   , FloatGeOp
   , FloatEqOp
   , FloatNeOp
   , FloatLtOp
   , FloatLeOp
   , FloatAddOp
   , FloatSubOp
   , FloatMulOp
   , FloatDivOp
   , FloatNegOp
   , Float2IntOp
   , FloatExpOp
   , FloatLogOp
   , FloatSqrtOp
   , FloatSinOp
   , FloatCosOp
   , FloatTanOp
   , FloatAsinOp
   , FloatAcosOp
   , FloatAtanOp
   , FloatSinhOp
   , FloatCoshOp
   , FloatTanhOp
   , FloatPowerOp
   , Float2DoubleOp
   , FloatDecode_IntOp
   , NewArrayOp
   , SameMutableArrayOp
   , ReadArrayOp
   , WriteArrayOp
   , SizeofArrayOp
   , SizeofMutableArrayOp
   , IndexArrayOp
   , UnsafeFreezeArrayOp
   , UnsafeThawArrayOp
   , CopyArrayOp
   , CopyMutableArrayOp
   , CloneArrayOp
   , CloneMutableArrayOp
   , FreezeArrayOp
   , ThawArrayOp
   , CasArrayOp
   , NewSmallArrayOp
   , SameSmallMutableArrayOp
   , ReadSmallArrayOp
   , WriteSmallArrayOp
   , SizeofSmallArrayOp
   , SizeofSmallMutableArrayOp
   , IndexSmallArrayOp
   , UnsafeFreezeSmallArrayOp
   , UnsafeThawSmallArrayOp
   , CopySmallArrayOp
   , CopySmallMutableArrayOp
   , CloneSmallArrayOp
   , CloneSmallMutableArrayOp
   , FreezeSmallArrayOp
   , ThawSmallArrayOp
   , CasSmallArrayOp
   , NewByteArrayOp_Char
   , NewPinnedByteArrayOp_Char
   , NewAlignedPinnedByteArrayOp_Char
   , ByteArrayContents_Char
   , SameMutableByteArrayOp
   , ShrinkMutableByteArrayOp_Char
   , ResizeMutableByteArrayOp_Char
   , UnsafeFreezeByteArrayOp
   , SizeofByteArrayOp
   , SizeofMutableByteArrayOp
   -- , GetSizeofMutableByteArrayOp TODO: Why not in scope?
   , IndexByteArrayOp_Char
   , IndexByteArrayOp_WideChar
   , IndexByteArrayOp_Int
   , IndexByteArrayOp_Word
   , IndexByteArrayOp_Addr
   , IndexByteArrayOp_Float
   , IndexByteArrayOp_Double
   , IndexByteArrayOp_StablePtr
   , IndexByteArrayOp_Int8
   , IndexByteArrayOp_Int16
   , IndexByteArrayOp_Int32
   , IndexByteArrayOp_Int64
   , IndexByteArrayOp_Word8
   , IndexByteArrayOp_Word16
   , IndexByteArrayOp_Word32
   , IndexByteArrayOp_Word64
   , ReadByteArrayOp_Char
   , ReadByteArrayOp_WideChar
   , ReadByteArrayOp_Int
   , ReadByteArrayOp_Word
   , ReadByteArrayOp_Addr
   , ReadByteArrayOp_Float
   , ReadByteArrayOp_Double
   , ReadByteArrayOp_StablePtr
   , ReadByteArrayOp_Int8
   , ReadByteArrayOp_Int16
   , ReadByteArrayOp_Int32
   , ReadByteArrayOp_Int64
   , ReadByteArrayOp_Word8
   , ReadByteArrayOp_Word16
   , ReadByteArrayOp_Word32
   , ReadByteArrayOp_Word64
   , WriteByteArrayOp_Char
   , WriteByteArrayOp_WideChar
   , WriteByteArrayOp_Int
   , WriteByteArrayOp_Word
   , WriteByteArrayOp_Addr
   , WriteByteArrayOp_Float
   , WriteByteArrayOp_Double
   , WriteByteArrayOp_StablePtr
   , WriteByteArrayOp_Int8
   , WriteByteArrayOp_Int16
   , WriteByteArrayOp_Int32
   , WriteByteArrayOp_Int64
   , WriteByteArrayOp_Word8
   , WriteByteArrayOp_Word16
   , WriteByteArrayOp_Word32
   , WriteByteArrayOp_Word64
   , CopyByteArrayOp
   , CopyMutableByteArrayOp
   , CopyByteArrayToAddrOp
   , CopyMutableByteArrayToAddrOp
   , CopyAddrToByteArrayOp
   , SetByteArrayOp
   , AtomicReadByteArrayOp_Int
   , AtomicWriteByteArrayOp_Int
   , CasByteArrayOp_Int
   , FetchAddByteArrayOp_Int
   , FetchSubByteArrayOp_Int
   , FetchAndByteArrayOp_Int
   , FetchNandByteArrayOp_Int
   , FetchOrByteArrayOp_Int
   , FetchXorByteArrayOp_Int
   , NewArrayArrayOp
   , SameMutableArrayArrayOp
   , UnsafeFreezeArrayArrayOp
   , SizeofArrayArrayOp
   , SizeofMutableArrayArrayOp
   , IndexArrayArrayOp_ByteArray
   , IndexArrayArrayOp_ArrayArray
   , ReadArrayArrayOp_ByteArray
   , ReadArrayArrayOp_MutableByteArray
   , ReadArrayArrayOp_ArrayArray
   , ReadArrayArrayOp_MutableArrayArray
   , WriteArrayArrayOp_ByteArray
   , WriteArrayArrayOp_MutableByteArray
   , WriteArrayArrayOp_ArrayArray
   , WriteArrayArrayOp_MutableArrayArray
   , CopyArrayArrayOp
   , CopyMutableArrayArrayOp
   , AddrAddOp
   , AddrSubOp
   , AddrRemOp
   , Addr2IntOp
   , Int2AddrOp
   , AddrGtOp
   , AddrGeOp
   , AddrEqOp
   , AddrNeOp
   , AddrLtOp
   , AddrLeOp
   , IndexOffAddrOp_Char
   , IndexOffAddrOp_WideChar
   , IndexOffAddrOp_Int
   , IndexOffAddrOp_Word
   , IndexOffAddrOp_Addr
   , IndexOffAddrOp_Float
   , IndexOffAddrOp_Double
   , IndexOffAddrOp_StablePtr
   , IndexOffAddrOp_Int8
   , IndexOffAddrOp_Int16
   , IndexOffAddrOp_Int32
   , IndexOffAddrOp_Int64
   , IndexOffAddrOp_Word8
   , IndexOffAddrOp_Word16
   , IndexOffAddrOp_Word32
   , IndexOffAddrOp_Word64
   , ReadOffAddrOp_Char
   , ReadOffAddrOp_WideChar
   , ReadOffAddrOp_Int
   , ReadOffAddrOp_Word
   , ReadOffAddrOp_Addr
   , ReadOffAddrOp_Float
   , ReadOffAddrOp_Double
   , ReadOffAddrOp_StablePtr
   , ReadOffAddrOp_Int8
   , ReadOffAddrOp_Int16
   , ReadOffAddrOp_Int32
   , ReadOffAddrOp_Int64
   , ReadOffAddrOp_Word8
   , ReadOffAddrOp_Word16
   , ReadOffAddrOp_Word32
   , ReadOffAddrOp_Word64
   , WriteOffAddrOp_Char
   , WriteOffAddrOp_WideChar
   , WriteOffAddrOp_Int
   , WriteOffAddrOp_Word
   , WriteOffAddrOp_Addr
   , WriteOffAddrOp_Float
   , WriteOffAddrOp_Double
   , WriteOffAddrOp_StablePtr
   , WriteOffAddrOp_Int8
   , WriteOffAddrOp_Int16
   , WriteOffAddrOp_Int32
   , WriteOffAddrOp_Int64
   , WriteOffAddrOp_Word8
   , WriteOffAddrOp_Word16
   , WriteOffAddrOp_Word32
   , WriteOffAddrOp_Word64
   , NewMutVarOp
   , ReadMutVarOp
   , WriteMutVarOp
   , SameMutVarOp
   , AtomicModifyMutVarOp
   , CasMutVarOp
   , CatchOp
   , RaiseOp
   , RaiseIOOp
   , MaskAsyncExceptionsOp
   , MaskUninterruptibleOp
   , UnmaskAsyncExceptionsOp
   , MaskStatus
   , AtomicallyOp
   , RetryOp
   , CatchRetryOp
   , CatchSTMOp
   , Check
   , NewTVarOp
   , ReadTVarOp
   , ReadTVarIOOp
   , WriteTVarOp
   , SameTVarOp
   , NewMVarOp
   , TakeMVarOp
   , TryTakeMVarOp
   , PutMVarOp
   , TryPutMVarOp
   , ReadMVarOp
   , TryReadMVarOp
   , SameMVarOp
   , IsEmptyMVarOp
   , DelayOp
   , WaitReadOp
   , WaitWriteOp
   , ForkOp
   , ForkOnOp
   , KillThreadOp
   , YieldOp
   , MyThreadIdOp
   , LabelThreadOp
   , IsCurrentThreadBoundOp
   , NoDuplicateOp
   , ThreadStatusOp
   , MkWeakOp
   , MkWeakNoFinalizerOp
   , AddCFinalizerToWeakOp
   , DeRefWeakOp
   , FinalizeWeakOp
   , TouchOp
   , MakeStablePtrOp
   , DeRefStablePtrOp
   , EqStablePtrOp
   , MakeStableNameOp
   , EqStableNameOp
   , StableNameToIntOp
   , ReallyUnsafePtrEqualityOp
   , ParOp
   , SparkOp
   , SeqOp
   , GetSparkOp
   , NumSparks
   , DataToTagOp
   , TagToEnumOp
   , AddrToAnyOp
   , MkApUpd0_Op
   , NewBCOOp
   , UnpackClosureOp
   , GetApStackValOp
   , GetCCSOfOp
   , GetCurrentCCSOp
 --  , ClearCCSOp TODO: Why not in scope?
   , TraceEventOp
   , TraceMarkerOp
   -- , (VecBroadcastOp IntVec 16 W8)
   -- , (VecBroadcastOp IntVec 8 W16)
   -- , (VecBroadcastOp IntVec 4 W32)
   -- , (VecBroadcastOp IntVec 2 W64)
   -- , (VecBroadcastOp IntVec 32 W8)
   -- , (VecBroadcastOp IntVec 16 W16)
   -- , (VecBroadcastOp IntVec 8 W32)
   -- , (VecBroadcastOp IntVec 4 W64)
   -- , (VecBroadcastOp IntVec 64 W8)
   -- , (VecBroadcastOp IntVec 32 W16)
   -- , (VecBroadcastOp IntVec 16 W32)
   -- , (VecBroadcastOp IntVec 8 W64)
   -- , (VecBroadcastOp WordVec 16 W8)
   -- , (VecBroadcastOp WordVec 8 W16)
   -- , (VecBroadcastOp WordVec 4 W32)
   -- , (VecBroadcastOp WordVec 2 W64)
   -- , (VecBroadcastOp WordVec 32 W8)
   -- , (VecBroadcastOp WordVec 16 W16)
   -- , (VecBroadcastOp WordVec 8 W32)
   -- , (VecBroadcastOp WordVec 4 W64)
   -- , (VecBroadcastOp WordVec 64 W8)
   -- , (VecBroadcastOp WordVec 32 W16)
   -- , (VecBroadcastOp WordVec 16 W32)
   -- , (VecBroadcastOp WordVec 8 W64)
   -- , (VecBroadcastOp FloatVec 4 W32)
   -- , (VecBroadcastOp FloatVec 2 W64)
   -- , (VecBroadcastOp FloatVec 8 W32)
   -- , (VecBroadcastOp FloatVec 4 W64)
   -- , (VecBroadcastOp FloatVec 16 W32)
   -- , (VecBroadcastOp FloatVec 8 W64)
   -- , (VecPackOp IntVec 16 W8)
   -- , (VecPackOp IntVec 8 W16)
   -- , (VecPackOp IntVec 4 W32)
   -- , (VecPackOp IntVec 2 W64)
   -- , (VecPackOp IntVec 32 W8)
   -- , (VecPackOp IntVec 16 W16)
   -- , (VecPackOp IntVec 8 W32)
   -- , (VecPackOp IntVec 4 W64)
   -- , (VecPackOp IntVec 64 W8)
   -- , (VecPackOp IntVec 32 W16)
   -- , (VecPackOp IntVec 16 W32)
   -- , (VecPackOp IntVec 8 W64)
   -- , (VecPackOp WordVec 16 W8)
   -- , (VecPackOp WordVec 8 W16)
   -- , (VecPackOp WordVec 4 W32)
   -- , (VecPackOp WordVec 2 W64)
   -- , (VecPackOp WordVec 32 W8)
   -- , (VecPackOp WordVec 16 W16)
   -- , (VecPackOp WordVec 8 W32)
   -- , (VecPackOp WordVec 4 W64)
   -- , (VecPackOp WordVec 64 W8)
   -- , (VecPackOp WordVec 32 W16)
   -- , (VecPackOp WordVec 16 W32)
   -- , (VecPackOp WordVec 8 W64)
   -- , (VecPackOp FloatVec 4 W32)
   -- , (VecPackOp FloatVec 2 W64)
   -- , (VecPackOp FloatVec 8 W32)
   -- , (VecPackOp FloatVec 4 W64)
   -- , (VecPackOp FloatVec 16 W32)
   -- , (VecPackOp FloatVec 8 W64)
   -- , (VecUnpackOp IntVec 16 W8)
   -- , (VecUnpackOp IntVec 8 W16)
   -- , (VecUnpackOp IntVec 4 W32)
   -- , (VecUnpackOp IntVec 2 W64)
   -- , (VecUnpackOp IntVec 32 W8)
   -- , (VecUnpackOp IntVec 16 W16)
   -- , (VecUnpackOp IntVec 8 W32)
   -- , (VecUnpackOp IntVec 4 W64)
   -- , (VecUnpackOp IntVec 64 W8)
   -- , (VecUnpackOp IntVec 32 W16)
   -- , (VecUnpackOp IntVec 16 W32)
   -- , (VecUnpackOp IntVec 8 W64)
   -- , (VecUnpackOp WordVec 16 W8)
   -- , (VecUnpackOp WordVec 8 W16)
   -- , (VecUnpackOp WordVec 4 W32)
   -- , (VecUnpackOp WordVec 2 W64)
   -- , (VecUnpackOp WordVec 32 W8)
   -- , (VecUnpackOp WordVec 16 W16)
   -- , (VecUnpackOp WordVec 8 W32)
   -- , (VecUnpackOp WordVec 4 W64)
   -- , (VecUnpackOp WordVec 64 W8)
   -- , (VecUnpackOp WordVec 32 W16)
   -- , (VecUnpackOp WordVec 16 W32)
   -- , (VecUnpackOp WordVec 8 W64)
   -- , (VecUnpackOp FloatVec 4 W32)
   -- , (VecUnpackOp FloatVec 2 W64)
   -- , (VecUnpackOp FloatVec 8 W32)
   -- , (VecUnpackOp FloatVec 4 W64)
   -- , (VecUnpackOp FloatVec 16 W32)
   -- , (VecUnpackOp FloatVec 8 W64)
   -- , (VecInsertOp IntVec 16 W8)
   -- , (VecInsertOp IntVec 8 W16)
   -- , (VecInsertOp IntVec 4 W32)
   -- , (VecInsertOp IntVec 2 W64)
   -- , (VecInsertOp IntVec 32 W8)
   -- , (VecInsertOp IntVec 16 W16)
   -- , (VecInsertOp IntVec 8 W32)
   -- , (VecInsertOp IntVec 4 W64)
   -- , (VecInsertOp IntVec 64 W8)
   -- , (VecInsertOp IntVec 32 W16)
   -- , (VecInsertOp IntVec 16 W32)
   -- , (VecInsertOp IntVec 8 W64)
   -- , (VecInsertOp WordVec 16 W8)
   -- , (VecInsertOp WordVec 8 W16)
   -- , (VecInsertOp WordVec 4 W32)
   -- , (VecInsertOp WordVec 2 W64)
   -- , (VecInsertOp WordVec 32 W8)
   -- , (VecInsertOp WordVec 16 W16)
   -- , (VecInsertOp WordVec 8 W32)
   -- , (VecInsertOp WordVec 4 W64)
   -- , (VecInsertOp WordVec 64 W8)
   -- , (VecInsertOp WordVec 32 W16)
   -- , (VecInsertOp WordVec 16 W32)
   -- , (VecInsertOp WordVec 8 W64)
   -- , (VecInsertOp FloatVec 4 W32)
   -- , (VecInsertOp FloatVec 2 W64)
   -- , (VecInsertOp FloatVec 8 W32)
   -- , (VecInsertOp FloatVec 4 W64)
   -- , (VecInsertOp FloatVec 16 W32)
   -- , (VecInsertOp FloatVec 8 W64)
   -- , (VecAddOp IntVec 16 W8)
   -- , (VecAddOp IntVec 8 W16)
   -- , (VecAddOp IntVec 4 W32)
   -- , (VecAddOp IntVec 2 W64)
   -- , (VecAddOp IntVec 32 W8)
   -- , (VecAddOp IntVec 16 W16)
   -- , (VecAddOp IntVec 8 W32)
   -- , (VecAddOp IntVec 4 W64)
   -- , (VecAddOp IntVec 64 W8)
   -- , (VecAddOp IntVec 32 W16)
   -- , (VecAddOp IntVec 16 W32)
   -- , (VecAddOp IntVec 8 W64)
   -- , (VecAddOp WordVec 16 W8)
   -- , (VecAddOp WordVec 8 W16)
   -- , (VecAddOp WordVec 4 W32)
   -- , (VecAddOp WordVec 2 W64)
   -- , (VecAddOp WordVec 32 W8)
   -- , (VecAddOp WordVec 16 W16)
   -- , (VecAddOp WordVec 8 W32)
   -- , (VecAddOp WordVec 4 W64)
   -- , (VecAddOp WordVec 64 W8)
   -- , (VecAddOp WordVec 32 W16)
   -- , (VecAddOp WordVec 16 W32)
   -- , (VecAddOp WordVec 8 W64)
   -- , (VecAddOp FloatVec 4 W32)
   -- , (VecAddOp FloatVec 2 W64)
   -- , (VecAddOp FloatVec 8 W32)
   -- , (VecAddOp FloatVec 4 W64)
   -- , (VecAddOp FloatVec 16 W32)
   -- , (VecAddOp FloatVec 8 W64)
   -- , (VecSubOp IntVec 16 W8)
   -- , (VecSubOp IntVec 8 W16)
   -- , (VecSubOp IntVec 4 W32)
   -- , (VecSubOp IntVec 2 W64)
   -- , (VecSubOp IntVec 32 W8)
   -- , (VecSubOp IntVec 16 W16)
   -- , (VecSubOp IntVec 8 W32)
   -- , (VecSubOp IntVec 4 W64)
   -- , (VecSubOp IntVec 64 W8)
   -- , (VecSubOp IntVec 32 W16)
   -- , (VecSubOp IntVec 16 W32)
   -- , (VecSubOp IntVec 8 W64)
   -- , (VecSubOp WordVec 16 W8)
   -- , (VecSubOp WordVec 8 W16)
   -- , (VecSubOp WordVec 4 W32)
   -- , (VecSubOp WordVec 2 W64)
   -- , (VecSubOp WordVec 32 W8)
   -- , (VecSubOp WordVec 16 W16)
   -- , (VecSubOp WordVec 8 W32)
   -- , (VecSubOp WordVec 4 W64)
   -- , (VecSubOp WordVec 64 W8)
   -- , (VecSubOp WordVec 32 W16)
   -- , (VecSubOp WordVec 16 W32)
   -- , (VecSubOp WordVec 8 W64)
   -- , (VecSubOp FloatVec 4 W32)
   -- , (VecSubOp FloatVec 2 W64)
   -- , (VecSubOp FloatVec 8 W32)
   -- , (VecSubOp FloatVec 4 W64)
   -- , (VecSubOp FloatVec 16 W32)
   -- , (VecSubOp FloatVec 8 W64)
   -- , (VecMulOp IntVec 16 W8)
   -- , (VecMulOp IntVec 8 W16)
   -- , (VecMulOp IntVec 4 W32)
   -- , (VecMulOp IntVec 2 W64)
   -- , (VecMulOp IntVec 32 W8)
   -- , (VecMulOp IntVec 16 W16)
   -- , (VecMulOp IntVec 8 W32)
   -- , (VecMulOp IntVec 4 W64)
   -- , (VecMulOp IntVec 64 W8)
   -- , (VecMulOp IntVec 32 W16)
   -- , (VecMulOp IntVec 16 W32)
   -- , (VecMulOp IntVec 8 W64)
   -- , (VecMulOp WordVec 16 W8)
   -- , (VecMulOp WordVec 8 W16)
   -- , (VecMulOp WordVec 4 W32)
   -- , (VecMulOp WordVec 2 W64)
   -- , (VecMulOp WordVec 32 W8)
   -- , (VecMulOp WordVec 16 W16)
   -- , (VecMulOp WordVec 8 W32)
   -- , (VecMulOp WordVec 4 W64)
   -- , (VecMulOp WordVec 64 W8)
   -- , (VecMulOp WordVec 32 W16)
   -- , (VecMulOp WordVec 16 W32)
   -- , (VecMulOp WordVec 8 W64)
   -- , (VecMulOp FloatVec 4 W32)
   -- , (VecMulOp FloatVec 2 W64)
   -- , (VecMulOp FloatVec 8 W32)
   -- , (VecMulOp FloatVec 4 W64)
   -- , (VecMulOp FloatVec 16 W32)
   -- , (VecMulOp FloatVec 8 W64)
   -- , (VecDivOp FloatVec 4 W32)
   -- , (VecDivOp FloatVec 2 W64)
   -- , (VecDivOp FloatVec 8 W32)
   -- , (VecDivOp FloatVec 4 W64)
   -- , (VecDivOp FloatVec 16 W32)
   -- , (VecDivOp FloatVec 8 W64)
   -- , (VecQuotOp IntVec 16 W8)
   -- , (VecQuotOp IntVec 8 W16)
   -- , (VecQuotOp IntVec 4 W32)
   -- , (VecQuotOp IntVec 2 W64)
   -- , (VecQuotOp IntVec 32 W8)
   -- , (VecQuotOp IntVec 16 W16)
   -- , (VecQuotOp IntVec 8 W32)
   -- , (VecQuotOp IntVec 4 W64)
   -- , (VecQuotOp IntVec 64 W8)
   -- , (VecQuotOp IntVec 32 W16)
   -- , (VecQuotOp IntVec 16 W32)
   -- , (VecQuotOp IntVec 8 W64)
   -- , (VecQuotOp WordVec 16 W8)
   -- , (VecQuotOp WordVec 8 W16)
   -- , (VecQuotOp WordVec 4 W32)
   -- , (VecQuotOp WordVec 2 W64)
   -- , (VecQuotOp WordVec 32 W8)
   -- , (VecQuotOp WordVec 16 W16)
   -- , (VecQuotOp WordVec 8 W32)
   -- , (VecQuotOp WordVec 4 W64)
   -- , (VecQuotOp WordVec 64 W8)
   -- , (VecQuotOp WordVec 32 W16)
   -- , (VecQuotOp WordVec 16 W32)
   -- , (VecQuotOp WordVec 8 W64)
   -- , (VecRemOp IntVec 16 W8)
   -- , (VecRemOp IntVec 8 W16)
   -- , (VecRemOp IntVec 4 W32)
   -- , (VecRemOp IntVec 2 W64)
   -- , (VecRemOp IntVec 32 W8)
   -- , (VecRemOp IntVec 16 W16)
   -- , (VecRemOp IntVec 8 W32)
   -- , (VecRemOp IntVec 4 W64)
   -- , (VecRemOp IntVec 64 W8)
   -- , (VecRemOp IntVec 32 W16)
   -- , (VecRemOp IntVec 16 W32)
   -- , (VecRemOp IntVec 8 W64)
   -- , (VecRemOp WordVec 16 W8)
   -- , (VecRemOp WordVec 8 W16)
   -- , (VecRemOp WordVec 4 W32)
   -- , (VecRemOp WordVec 2 W64)
   -- , (VecRemOp WordVec 32 W8)
   -- , (VecRemOp WordVec 16 W16)
   -- , (VecRemOp WordVec 8 W32)
   -- , (VecRemOp WordVec 4 W64)
   -- , (VecRemOp WordVec 64 W8)
   -- , (VecRemOp WordVec 32 W16)
   -- , (VecRemOp WordVec 16 W32)
   -- , (VecRemOp WordVec 8 W64)
   -- , (VecNegOp IntVec 16 W8)
   -- , (VecNegOp IntVec 8 W16)
   -- , (VecNegOp IntVec 4 W32)
   -- , (VecNegOp IntVec 2 W64)
   -- , (VecNegOp IntVec 32 W8)
   -- , (VecNegOp IntVec 16 W16)
   -- , (VecNegOp IntVec 8 W32)
   -- , (VecNegOp IntVec 4 W64)
   -- , (VecNegOp IntVec 64 W8)
   -- , (VecNegOp IntVec 32 W16)
   -- , (VecNegOp IntVec 16 W32)
   -- , (VecNegOp IntVec 8 W64)
   -- , (VecNegOp FloatVec 4 W32)
   -- , (VecNegOp FloatVec 2 W64)
   -- , (VecNegOp FloatVec 8 W32)
   -- , (VecNegOp FloatVec 4 W64)
   -- , (VecNegOp FloatVec 16 W32)
   -- , (VecNegOp FloatVec 8 W64)
   -- , (VecIndexByteArrayOp IntVec 16 W8)
   -- , (VecIndexByteArrayOp IntVec 8 W16)
   -- , (VecIndexByteArrayOp IntVec 4 W32)
   -- , (VecIndexByteArrayOp IntVec 2 W64)
   -- , (VecIndexByteArrayOp IntVec 32 W8)
   -- , (VecIndexByteArrayOp IntVec 16 W16)
   -- , (VecIndexByteArrayOp IntVec 8 W32)
   -- , (VecIndexByteArrayOp IntVec 4 W64)
   -- , (VecIndexByteArrayOp IntVec 64 W8)
   -- , (VecIndexByteArrayOp IntVec 32 W16)
   -- , (VecIndexByteArrayOp IntVec 16 W32)
   -- , (VecIndexByteArrayOp IntVec 8 W64)
   -- , (VecIndexByteArrayOp WordVec 16 W8)
   -- , (VecIndexByteArrayOp WordVec 8 W16)
   -- , (VecIndexByteArrayOp WordVec 4 W32)
   -- , (VecIndexByteArrayOp WordVec 2 W64)
   -- , (VecIndexByteArrayOp WordVec 32 W8)
   -- , (VecIndexByteArrayOp WordVec 16 W16)
   -- , (VecIndexByteArrayOp WordVec 8 W32)
   -- , (VecIndexByteArrayOp WordVec 4 W64)
   -- , (VecIndexByteArrayOp WordVec 64 W8)
   -- , (VecIndexByteArrayOp WordVec 32 W16)
   -- , (VecIndexByteArrayOp WordVec 16 W32)
   -- , (VecIndexByteArrayOp WordVec 8 W64)
   -- , (VecIndexByteArrayOp FloatVec 4 W32)
   -- , (VecIndexByteArrayOp FloatVec 2 W64)
   -- , (VecIndexByteArrayOp FloatVec 8 W32)
   -- , (VecIndexByteArrayOp FloatVec 4 W64)
   -- , (VecIndexByteArrayOp FloatVec 16 W32)
   -- , (VecIndexByteArrayOp FloatVec 8 W64)
   -- , (VecReadByteArrayOp IntVec 16 W8)
   -- , (VecReadByteArrayOp IntVec 8 W16)
   -- , (VecReadByteArrayOp IntVec 4 W32)
   -- , (VecReadByteArrayOp IntVec 2 W64)
   -- , (VecReadByteArrayOp IntVec 32 W8)
   -- , (VecReadByteArrayOp IntVec 16 W16)
   -- , (VecReadByteArrayOp IntVec 8 W32)
   -- , (VecReadByteArrayOp IntVec 4 W64)
   -- , (VecReadByteArrayOp IntVec 64 W8)
   -- , (VecReadByteArrayOp IntVec 32 W16)
   -- , (VecReadByteArrayOp IntVec 16 W32)
   -- , (VecReadByteArrayOp IntVec 8 W64)
   -- , (VecReadByteArrayOp WordVec 16 W8)
   -- , (VecReadByteArrayOp WordVec 8 W16)
   -- , (VecReadByteArrayOp WordVec 4 W32)
   -- , (VecReadByteArrayOp WordVec 2 W64)
   -- , (VecReadByteArrayOp WordVec 32 W8)
   -- , (VecReadByteArrayOp WordVec 16 W16)
   -- , (VecReadByteArrayOp WordVec 8 W32)
   -- , (VecReadByteArrayOp WordVec 4 W64)
   -- , (VecReadByteArrayOp WordVec 64 W8)
   -- , (VecReadByteArrayOp WordVec 32 W16)
   -- , (VecReadByteArrayOp WordVec 16 W32)
   -- , (VecReadByteArrayOp WordVec 8 W64)
   -- , (VecReadByteArrayOp FloatVec 4 W32)
   -- , (VecReadByteArrayOp FloatVec 2 W64)
   -- , (VecReadByteArrayOp FloatVec 8 W32)
   -- , (VecReadByteArrayOp FloatVec 4 W64)
   -- , (VecReadByteArrayOp FloatVec 16 W32)
   -- , (VecReadByteArrayOp FloatVec 8 W64)
   -- , (VecWriteByteArrayOp IntVec 16 W8)
   -- , (VecWriteByteArrayOp IntVec 8 W16)
   -- , (VecWriteByteArrayOp IntVec 4 W32)
   -- , (VecWriteByteArrayOp IntVec 2 W64)
   -- , (VecWriteByteArrayOp IntVec 32 W8)
   -- , (VecWriteByteArrayOp IntVec 16 W16)
   -- , (VecWriteByteArrayOp IntVec 8 W32)
   -- , (VecWriteByteArrayOp IntVec 4 W64)
   -- , (VecWriteByteArrayOp IntVec 64 W8)
   -- , (VecWriteByteArrayOp IntVec 32 W16)
   -- , (VecWriteByteArrayOp IntVec 16 W32)
   -- , (VecWriteByteArrayOp IntVec 8 W64)
   -- , (VecWriteByteArrayOp WordVec 16 W8)
   -- , (VecWriteByteArrayOp WordVec 8 W16)
   -- , (VecWriteByteArrayOp WordVec 4 W32)
   -- , (VecWriteByteArrayOp WordVec 2 W64)
   -- , (VecWriteByteArrayOp WordVec 32 W8)
   -- , (VecWriteByteArrayOp WordVec 16 W16)
   -- , (VecWriteByteArrayOp WordVec 8 W32)
   -- , (VecWriteByteArrayOp WordVec 4 W64)
   -- , (VecWriteByteArrayOp WordVec 64 W8)
   -- , (VecWriteByteArrayOp WordVec 32 W16)
   -- , (VecWriteByteArrayOp WordVec 16 W32)
   -- , (VecWriteByteArrayOp WordVec 8 W64)
   -- , (VecWriteByteArrayOp FloatVec 4 W32)
   -- , (VecWriteByteArrayOp FloatVec 2 W64)
   -- , (VecWriteByteArrayOp FloatVec 8 W32)
   -- , (VecWriteByteArrayOp FloatVec 4 W64)
   -- , (VecWriteByteArrayOp FloatVec 16 W32)
   -- , (VecWriteByteArrayOp FloatVec 8 W64)
   -- , (VecIndexOffAddrOp IntVec 16 W8)
   -- , (VecIndexOffAddrOp IntVec 8 W16)
   -- , (VecIndexOffAddrOp IntVec 4 W32)
   -- , (VecIndexOffAddrOp IntVec 2 W64)
   -- , (VecIndexOffAddrOp IntVec 32 W8)
   -- , (VecIndexOffAddrOp IntVec 16 W16)
   -- , (VecIndexOffAddrOp IntVec 8 W32)
   -- , (VecIndexOffAddrOp IntVec 4 W64)
   -- , (VecIndexOffAddrOp IntVec 64 W8)
   -- , (VecIndexOffAddrOp IntVec 32 W16)
   -- , (VecIndexOffAddrOp IntVec 16 W32)
   -- , (VecIndexOffAddrOp IntVec 8 W64)
   -- , (VecIndexOffAddrOp WordVec 16 W8)
   -- , (VecIndexOffAddrOp WordVec 8 W16)
   -- , (VecIndexOffAddrOp WordVec 4 W32)
   -- , (VecIndexOffAddrOp WordVec 2 W64)
   -- , (VecIndexOffAddrOp WordVec 32 W8)
   -- , (VecIndexOffAddrOp WordVec 16 W16)
   -- , (VecIndexOffAddrOp WordVec 8 W32)
   -- , (VecIndexOffAddrOp WordVec 4 W64)
   -- , (VecIndexOffAddrOp WordVec 64 W8)
   -- , (VecIndexOffAddrOp WordVec 32 W16)
   -- , (VecIndexOffAddrOp WordVec 16 W32)
   -- , (VecIndexOffAddrOp WordVec 8 W64)
   -- , (VecIndexOffAddrOp FloatVec 4 W32)
   -- , (VecIndexOffAddrOp FloatVec 2 W64)
   -- , (VecIndexOffAddrOp FloatVec 8 W32)
   -- , (VecIndexOffAddrOp FloatVec 4 W64)
   -- , (VecIndexOffAddrOp FloatVec 16 W32)
   -- , (VecIndexOffAddrOp FloatVec 8 W64)
   -- , (VecReadOffAddrOp IntVec 16 W8)
   -- , (VecReadOffAddrOp IntVec 8 W16)
   -- , (VecReadOffAddrOp IntVec 4 W32)
   -- , (VecReadOffAddrOp IntVec 2 W64)
   -- , (VecReadOffAddrOp IntVec 32 W8)
   -- , (VecReadOffAddrOp IntVec 16 W16)
   -- , (VecReadOffAddrOp IntVec 8 W32)
   -- , (VecReadOffAddrOp IntVec 4 W64)
   -- , (VecReadOffAddrOp IntVec 64 W8)
   -- , (VecReadOffAddrOp IntVec 32 W16)
   -- , (VecReadOffAddrOp IntVec 16 W32)
   -- , (VecReadOffAddrOp IntVec 8 W64)
   -- , (VecReadOffAddrOp WordVec 16 W8)
   -- , (VecReadOffAddrOp WordVec 8 W16)
   -- , (VecReadOffAddrOp WordVec 4 W32)
   -- , (VecReadOffAddrOp WordVec 2 W64)
   -- , (VecReadOffAddrOp WordVec 32 W8)
   -- , (VecReadOffAddrOp WordVec 16 W16)
   -- , (VecReadOffAddrOp WordVec 8 W32)
   -- , (VecReadOffAddrOp WordVec 4 W64)
   -- , (VecReadOffAddrOp WordVec 64 W8)
   -- , (VecReadOffAddrOp WordVec 32 W16)
   -- , (VecReadOffAddrOp WordVec 16 W32)
   -- , (VecReadOffAddrOp WordVec 8 W64)
   -- , (VecReadOffAddrOp FloatVec 4 W32)
   -- , (VecReadOffAddrOp FloatVec 2 W64)
   -- , (VecReadOffAddrOp FloatVec 8 W32)
   -- , (VecReadOffAddrOp FloatVec 4 W64)
   -- , (VecReadOffAddrOp FloatVec 16 W32)
   -- , (VecReadOffAddrOp FloatVec 8 W64)
   -- , (VecWriteOffAddrOp IntVec 16 W8)
   -- , (VecWriteOffAddrOp IntVec 8 W16)
   -- , (VecWriteOffAddrOp IntVec 4 W32)
   -- , (VecWriteOffAddrOp IntVec 2 W64)
   -- , (VecWriteOffAddrOp IntVec 32 W8)
   -- , (VecWriteOffAddrOp IntVec 16 W16)
   -- , (VecWriteOffAddrOp IntVec 8 W32)
   -- , (VecWriteOffAddrOp IntVec 4 W64)
   -- , (VecWriteOffAddrOp IntVec 64 W8)
   -- , (VecWriteOffAddrOp IntVec 32 W16)
   -- , (VecWriteOffAddrOp IntVec 16 W32)
   -- , (VecWriteOffAddrOp IntVec 8 W64)
   -- , (VecWriteOffAddrOp WordVec 16 W8)
   -- , (VecWriteOffAddrOp WordVec 8 W16)
   -- , (VecWriteOffAddrOp WordVec 4 W32)
   -- , (VecWriteOffAddrOp WordVec 2 W64)
   -- , (VecWriteOffAddrOp WordVec 32 W8)
   -- , (VecWriteOffAddrOp WordVec 16 W16)
   -- , (VecWriteOffAddrOp WordVec 8 W32)
   -- , (VecWriteOffAddrOp WordVec 4 W64)
   -- , (VecWriteOffAddrOp WordVec 64 W8)
   -- , (VecWriteOffAddrOp WordVec 32 W16)
   -- , (VecWriteOffAddrOp WordVec 16 W32)
   -- , (VecWriteOffAddrOp WordVec 8 W64)
   -- , (VecWriteOffAddrOp FloatVec 4 W32)
   -- , (VecWriteOffAddrOp FloatVec 2 W64)
   -- , (VecWriteOffAddrOp FloatVec 8 W32)
   -- , (VecWriteOffAddrOp FloatVec 4 W64)
   -- , (VecWriteOffAddrOp FloatVec 16 W32)
   -- , (VecWriteOffAddrOp FloatVec 8 W64)
   -- , (VecIndexScalarByteArrayOp IntVec 16 W8)
   -- , (VecIndexScalarByteArrayOp IntVec 8 W16)
   -- , (VecIndexScalarByteArrayOp IntVec 4 W32)
   -- , (VecIndexScalarByteArrayOp IntVec 2 W64)
   -- , (VecIndexScalarByteArrayOp IntVec 32 W8)
   -- , (VecIndexScalarByteArrayOp IntVec 16 W16)
   -- , (VecIndexScalarByteArrayOp IntVec 8 W32)
   -- , (VecIndexScalarByteArrayOp IntVec 4 W64)
   -- , (VecIndexScalarByteArrayOp IntVec 64 W8)
   -- , (VecIndexScalarByteArrayOp IntVec 32 W16)
   -- , (VecIndexScalarByteArrayOp IntVec 16 W32)
   -- , (VecIndexScalarByteArrayOp IntVec 8 W64)
   -- , (VecIndexScalarByteArrayOp WordVec 16 W8)
   -- , (VecIndexScalarByteArrayOp WordVec 8 W16)
   -- , (VecIndexScalarByteArrayOp WordVec 4 W32)
   -- , (VecIndexScalarByteArrayOp WordVec 2 W64)
   -- , (VecIndexScalarByteArrayOp WordVec 32 W8)
   -- , (VecIndexScalarByteArrayOp WordVec 16 W16)
   -- , (VecIndexScalarByteArrayOp WordVec 8 W32)
   -- , (VecIndexScalarByteArrayOp WordVec 4 W64)
   -- , (VecIndexScalarByteArrayOp WordVec 64 W8)
   -- , (VecIndexScalarByteArrayOp WordVec 32 W16)
   -- , (VecIndexScalarByteArrayOp WordVec 16 W32)
   -- , (VecIndexScalarByteArrayOp WordVec 8 W64)
   -- , (VecIndexScalarByteArrayOp FloatVec 4 W32)
   -- , (VecIndexScalarByteArrayOp FloatVec 2 W64)
   -- , (VecIndexScalarByteArrayOp FloatVec 8 W32)
   -- , (VecIndexScalarByteArrayOp FloatVec 4 W64)
   -- , (VecIndexScalarByteArrayOp FloatVec 16 W32)
   -- , (VecIndexScalarByteArrayOp FloatVec 8 W64)
   -- , (VecReadScalarByteArrayOp IntVec 16 W8)
   -- , (VecReadScalarByteArrayOp IntVec 8 W16)
   -- , (VecReadScalarByteArrayOp IntVec 4 W32)
   -- , (VecReadScalarByteArrayOp IntVec 2 W64)
   -- , (VecReadScalarByteArrayOp IntVec 32 W8)
   -- , (VecReadScalarByteArrayOp IntVec 16 W16)
   -- , (VecReadScalarByteArrayOp IntVec 8 W32)
   -- , (VecReadScalarByteArrayOp IntVec 4 W64)
   -- , (VecReadScalarByteArrayOp IntVec 64 W8)
   -- , (VecReadScalarByteArrayOp IntVec 32 W16)
   -- , (VecReadScalarByteArrayOp IntVec 16 W32)
   -- , (VecReadScalarByteArrayOp IntVec 8 W64)
   -- , (VecReadScalarByteArrayOp WordVec 16 W8)
   -- , (VecReadScalarByteArrayOp WordVec 8 W16)
   -- , (VecReadScalarByteArrayOp WordVec 4 W32)
   -- , (VecReadScalarByteArrayOp WordVec 2 W64)
   -- , (VecReadScalarByteArrayOp WordVec 32 W8)
   -- , (VecReadScalarByteArrayOp WordVec 16 W16)
   -- , (VecReadScalarByteArrayOp WordVec 8 W32)
   -- , (VecReadScalarByteArrayOp WordVec 4 W64)
   -- , (VecReadScalarByteArrayOp WordVec 64 W8)
   -- , (VecReadScalarByteArrayOp WordVec 32 W16)
   -- , (VecReadScalarByteArrayOp WordVec 16 W32)
   -- , (VecReadScalarByteArrayOp WordVec 8 W64)
   -- , (VecReadScalarByteArrayOp FloatVec 4 W32)
   -- , (VecReadScalarByteArrayOp FloatVec 2 W64)
   -- , (VecReadScalarByteArrayOp FloatVec 8 W32)
   -- , (VecReadScalarByteArrayOp FloatVec 4 W64)
   -- , (VecReadScalarByteArrayOp FloatVec 16 W32)
   -- , (VecReadScalarByteArrayOp FloatVec 8 W64)
   -- , (VecWriteScalarByteArrayOp IntVec 16 W8)
   -- , (VecWriteScalarByteArrayOp IntVec 8 W16)
   -- , (VecWriteScalarByteArrayOp IntVec 4 W32)
   -- , (VecWriteScalarByteArrayOp IntVec 2 W64)
   -- , (VecWriteScalarByteArrayOp IntVec 32 W8)
   -- , (VecWriteScalarByteArrayOp IntVec 16 W16)
   -- , (VecWriteScalarByteArrayOp IntVec 8 W32)
   -- , (VecWriteScalarByteArrayOp IntVec 4 W64)
   -- , (VecWriteScalarByteArrayOp IntVec 64 W8)
   -- , (VecWriteScalarByteArrayOp IntVec 32 W16)
   -- , (VecWriteScalarByteArrayOp IntVec 16 W32)
   -- , (VecWriteScalarByteArrayOp IntVec 8 W64)
   -- , (VecWriteScalarByteArrayOp WordVec 16 W8)
   -- , (VecWriteScalarByteArrayOp WordVec 8 W16)
   -- , (VecWriteScalarByteArrayOp WordVec 4 W32)
   -- , (VecWriteScalarByteArrayOp WordVec 2 W64)
   -- , (VecWriteScalarByteArrayOp WordVec 32 W8)
   -- , (VecWriteScalarByteArrayOp WordVec 16 W16)
   -- , (VecWriteScalarByteArrayOp WordVec 8 W32)
   -- , (VecWriteScalarByteArrayOp WordVec 4 W64)
   -- , (VecWriteScalarByteArrayOp WordVec 64 W8)
   -- , (VecWriteScalarByteArrayOp WordVec 32 W16)
   -- , (VecWriteScalarByteArrayOp WordVec 16 W32)
   -- , (VecWriteScalarByteArrayOp WordVec 8 W64)
   -- , (VecWriteScalarByteArrayOp FloatVec 4 W32)
   -- , (VecWriteScalarByteArrayOp FloatVec 2 W64)
   -- , (VecWriteScalarByteArrayOp FloatVec 8 W32)
   -- , (VecWriteScalarByteArrayOp FloatVec 4 W64)
   -- , (VecWriteScalarByteArrayOp FloatVec 16 W32)
   -- , (VecWriteScalarByteArrayOp FloatVec 8 W64)
   -- , (VecIndexScalarOffAddrOp IntVec 16 W8)
   -- , (VecIndexScalarOffAddrOp IntVec 8 W16)
   -- , (VecIndexScalarOffAddrOp IntVec 4 W32)
   -- , (VecIndexScalarOffAddrOp IntVec 2 W64)
   -- , (VecIndexScalarOffAddrOp IntVec 32 W8)
   -- , (VecIndexScalarOffAddrOp IntVec 16 W16)
   -- , (VecIndexScalarOffAddrOp IntVec 8 W32)
   -- , (VecIndexScalarOffAddrOp IntVec 4 W64)
   -- , (VecIndexScalarOffAddrOp IntVec 64 W8)
   -- , (VecIndexScalarOffAddrOp IntVec 32 W16)
   -- , (VecIndexScalarOffAddrOp IntVec 16 W32)
   -- , (VecIndexScalarOffAddrOp IntVec 8 W64)
   -- , (VecIndexScalarOffAddrOp WordVec 16 W8)
   -- , (VecIndexScalarOffAddrOp WordVec 8 W16)
   -- , (VecIndexScalarOffAddrOp WordVec 4 W32)
   -- , (VecIndexScalarOffAddrOp WordVec 2 W64)
   -- , (VecIndexScalarOffAddrOp WordVec 32 W8)
   -- , (VecIndexScalarOffAddrOp WordVec 16 W16)
   -- , (VecIndexScalarOffAddrOp WordVec 8 W32)
   -- , (VecIndexScalarOffAddrOp WordVec 4 W64)
   -- , (VecIndexScalarOffAddrOp WordVec 64 W8)
   -- , (VecIndexScalarOffAddrOp WordVec 32 W16)
   -- , (VecIndexScalarOffAddrOp WordVec 16 W32)
   -- , (VecIndexScalarOffAddrOp WordVec 8 W64)
   -- , (VecIndexScalarOffAddrOp FloatVec 4 W32)
   -- , (VecIndexScalarOffAddrOp FloatVec 2 W64)
   -- , (VecIndexScalarOffAddrOp FloatVec 8 W32)
   -- , (VecIndexScalarOffAddrOp FloatVec 4 W64)
   -- , (VecIndexScalarOffAddrOp FloatVec 16 W32)
   -- , (VecIndexScalarOffAddrOp FloatVec 8 W64)
   -- , (VecReadScalarOffAddrOp IntVec 16 W8)
   -- , (VecReadScalarOffAddrOp IntVec 8 W16)
   -- , (VecReadScalarOffAddrOp IntVec 4 W32)
   -- , (VecReadScalarOffAddrOp IntVec 2 W64)
   -- , (VecReadScalarOffAddrOp IntVec 32 W8)
   -- , (VecReadScalarOffAddrOp IntVec 16 W16)
   -- , (VecReadScalarOffAddrOp IntVec 8 W32)
   -- , (VecReadScalarOffAddrOp IntVec 4 W64)
   -- , (VecReadScalarOffAddrOp IntVec 64 W8)
   -- , (VecReadScalarOffAddrOp IntVec 32 W16)
   -- , (VecReadScalarOffAddrOp IntVec 16 W32)
   -- , (VecReadScalarOffAddrOp IntVec 8 W64)
   -- , (VecReadScalarOffAddrOp WordVec 16 W8)
   -- , (VecReadScalarOffAddrOp WordVec 8 W16)
   -- , (VecReadScalarOffAddrOp WordVec 4 W32)
   -- , (VecReadScalarOffAddrOp WordVec 2 W64)
   -- , (VecReadScalarOffAddrOp WordVec 32 W8)
   -- , (VecReadScalarOffAddrOp WordVec 16 W16)
   -- , (VecReadScalarOffAddrOp WordVec 8 W32)
   -- , (VecReadScalarOffAddrOp WordVec 4 W64)
   -- , (VecReadScalarOffAddrOp WordVec 64 W8)
   -- , (VecReadScalarOffAddrOp WordVec 32 W16)
   -- , (VecReadScalarOffAddrOp WordVec 16 W32)
   -- , (VecReadScalarOffAddrOp WordVec 8 W64)
   -- , (VecReadScalarOffAddrOp FloatVec 4 W32)
   -- , (VecReadScalarOffAddrOp FloatVec 2 W64)
   -- , (VecReadScalarOffAddrOp FloatVec 8 W32)
   -- , (VecReadScalarOffAddrOp FloatVec 4 W64)
   -- , (VecReadScalarOffAddrOp FloatVec 16 W32)
   -- , (VecReadScalarOffAddrOp FloatVec 8 W64)
   -- , (VecWriteScalarOffAddrOp IntVec 16 W8)
   -- , (VecWriteScalarOffAddrOp IntVec 8 W16)
   -- , (VecWriteScalarOffAddrOp IntVec 4 W32)
   -- , (VecWriteScalarOffAddrOp IntVec 2 W64)
   -- , (VecWriteScalarOffAddrOp IntVec 32 W8)
   -- , (VecWriteScalarOffAddrOp IntVec 16 W16)
   -- , (VecWriteScalarOffAddrOp IntVec 8 W32)
   -- , (VecWriteScalarOffAddrOp IntVec 4 W64)
   -- , (VecWriteScalarOffAddrOp IntVec 64 W8)
   -- , (VecWriteScalarOffAddrOp IntVec 32 W16)
   -- , (VecWriteScalarOffAddrOp IntVec 16 W32)
   -- , (VecWriteScalarOffAddrOp IntVec 8 W64)
   -- , (VecWriteScalarOffAddrOp WordVec 16 W8)
   -- , (VecWriteScalarOffAddrOp WordVec 8 W16)
   -- , (VecWriteScalarOffAddrOp WordVec 4 W32)
   -- , (VecWriteScalarOffAddrOp WordVec 2 W64)
   -- , (VecWriteScalarOffAddrOp WordVec 32 W8)
   -- , (VecWriteScalarOffAddrOp WordVec 16 W16)
   -- , (VecWriteScalarOffAddrOp WordVec 8 W32)
   -- , (VecWriteScalarOffAddrOp WordVec 4 W64)
   -- , (VecWriteScalarOffAddrOp WordVec 64 W8)
   -- , (VecWriteScalarOffAddrOp WordVec 32 W16)
   -- , (VecWriteScalarOffAddrOp WordVec 16 W32)
   -- , (VecWriteScalarOffAddrOp WordVec 8 W64)
   -- , (VecWriteScalarOffAddrOp FloatVec 4 W32)
   -- , (VecWriteScalarOffAddrOp FloatVec 2 W64)
   -- , (VecWriteScalarOffAddrOp FloatVec 8 W32)
   -- , (VecWriteScalarOffAddrOp FloatVec 4 W64)
   -- , (VecWriteScalarOffAddrOp FloatVec 16 W32)
   -- , (VecWriteScalarOffAddrOp FloatVec 8 W64)
   , PrefetchByteArrayOp3
   , PrefetchMutableByteArrayOp3
   , PrefetchAddrOp3
   , PrefetchValueOp3
   , PrefetchByteArrayOp2
   , PrefetchMutableByteArrayOp2
   , PrefetchAddrOp2
   , PrefetchValueOp2
   , PrefetchByteArrayOp1
   , PrefetchMutableByteArrayOp1
   , PrefetchAddrOp1
   , PrefetchValueOp1
   , PrefetchByteArrayOp0
   , PrefetchMutableByteArrayOp0
   , PrefetchAddrOp0
   , PrefetchValueOp0
   ]

ghcvmPrimExports :: [IfaceExport]
ghcvmPrimExports
 = map (Avail . idName) ghcvmPrimIds ++
   map (Avail . idName . primOpId) ghcvmPrimOps ++
   [ AvailTC n [n]
   | tc <- funTyCon : ghcvmPrimTyCons, let n = tyConName tc  ]

-- TODO: Add custom ghcvm PrimIds here
ghcvmPrimIds :: [Id]
ghcvmPrimIds
  = [   -- These can't be defined in Haskell, but they have
        -- perfectly reasonable unfoldings in Core
    realWorldPrimId,
    voidPrimId,
    unsafeCoerceId,
    nullAddrId,
    seqId,
    magicDictId,
    coerceId,
    proxyHashId
    ]

ghcvmPrimTyCons :: [TyCon]
ghcvmPrimTyCons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , arrayArrayPrimTyCon
    , smallArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mutableArrayArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , voidPrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , anyTyCon
    , eqPrimTyCon
    , eqReprPrimTyCon
    , liftedTypeKindTyCon
    , unliftedTypeKindTyCon
    , openTypeKindTyCon
    , constraintKindTyCon
    , superKindTyCon
    , anyKindTyCon
    -- GHCVM custom TyCons start here
    , jcharPrimTyCon
    , jbytePrimTyCon
    , jbooleanPrimTyCon
    , jshortPrimTyCon
    , objectPrimTyCon ]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (ATyCon tycon)        -- Relevant TyCon
                  UserSyntax

pcPrimTyCon :: Name -> [Role] -> PrimRep -> TyCon
pcPrimTyCon name roles rep
  = mkPrimTyCon name kind roles rep
  where
    kind        = mkArrowKinds (map (const liftedTypeKind) roles) result_kind
    result_kind = unliftedTypeKind

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind [] rep
  where
    result_kind = unliftedTypeKind

-- TODO: Verify that the unique numbers don't clash with existing primTyConKeys
jcharPrimTy :: Type
jcharPrimTy = mkTyConTy jcharPrimTyCon
jcharPrimTyConName             = mkPrimTc (fsLit "JChar#") jcharPrimTyConKey jcharPrimTyCon
jcharPrimTyConKey                        = mkPreludeTyConUnique 77
jcharPrimTyCon = pcPrimTyCon0 jcharPrimTyConName VoidRep

jbooleanPrimTy :: Type
jbooleanPrimTy = mkTyConTy jbooleanPrimTyCon
jbooleanPrimTyConName             = mkPrimTc (fsLit "JBoolean#") jbooleanPrimTyConKey jbooleanPrimTyCon
jbooleanPrimTyConKey                        = mkPreludeTyConUnique 78
jbooleanPrimTyCon = pcPrimTyCon0 jbooleanPrimTyConName VoidRep

jbytePrimTy :: Type
jbytePrimTy = mkTyConTy jbytePrimTyCon
jbytePrimTyConName             = mkPrimTc (fsLit "JByte#") jbytePrimTyConKey jbytePrimTyCon
jbytePrimTyConKey                        = mkPreludeTyConUnique 79
jbytePrimTyCon = pcPrimTyCon0 jbytePrimTyConName VoidRep

jshortPrimTy :: Type
jshortPrimTy = mkTyConTy jshortPrimTyCon
jshortPrimTyConName             = mkPrimTc (fsLit "JShort#") jshortPrimTyConKey jshortPrimTyCon
jshortPrimTyConKey                        = mkPreludeTyConUnique 80
jshortPrimTyCon = pcPrimTyCon0 jshortPrimTyConName VoidRep

objectPrimTyConKey :: Unique
objectPrimTyConKey = mkPreludeTyConUnique 83
objectPrimTyCon :: TyCon
objectPrimTyCon   = pcPrimTyCon objectPrimTyConName [Nominal] VoidRep
objectPrimTyConName :: Name
objectPrimTyConName = mkPrimTc (fsLit "Object#") objectPrimTyConKey objectPrimTyCon

data JPrimRep = HPrimRep PrimRep
              | JRepBool
              | JRepChar
              | JRepByte
              | JRepShort
              | JRepObject Text

typeJPrimRep :: UnaryType -> JPrimRep
typeJPrimRep ty = case splitTyConApp_maybe ty of
  Just (tyCon, tys) -> if isUnboxedTupleTyCon tyCon
                          then pprPanic "typeJPrimRep: isUnboxedTypeTyCon" (ppr ty)
                          else case maybeJRep tyCon tys of
                                 Just primRep -> primRep
                                 Nothing -> HPrimRep $ tyConPrimRep tyCon
  Nothing -> pprPanic "typeJPrimRep: Unknown " (ppr ty)

maybeJRep :: TyCon -> [Type] -> Maybe JPrimRep
maybeJRep tyCon tys
  | tcUnique == jbooleanPrimTyConKey = Just JRepBool
  | tcUnique == jcharPrimTyConKey    = Just JRepChar
  | tcUnique == jbytePrimTyConKey    = Just JRepByte
  | tcUnique == jshortPrimTyConKey   = Just JRepShort
-- NOTE: A tag for a object MUST have an associated CType!
  | tcUnique == objectPrimTyConKey   = Just
                                     . JRepObject
                                     . fastStringToText
                                     . (\(CType _ _ fs) -> fs)
                                     . fromJust
                                     . tyConCType
                                     . fromJust
                                     . fmap fst
                                     . splitTyConApp_maybe
                                     $ head tys
  | otherwise                        = Nothing
  where tcUnique = tyConUnique tyCon

isVoidJRep :: JPrimRep -> Bool
isVoidJRep (HPrimRep VoidRep) = True
isVoidJRep _other  = False

idJPrimRep :: Id -> JPrimRep
idJPrimRep = typeJPrimRep . idType
