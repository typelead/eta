{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrimOp]{Primitive operations (machine-level)}
-}

{-# LANGUAGE CPP #-}

module Eta.Prelude.PrimOp (
        PrimOp(..), PrimOpVecCat(..), allThePrimOps,
        primOpType, primOpSig,
        primOpTag, maxPrimOpTag, primOpOcc,

        tagToEnumKey,

        primOpOutOfLine, primOpCodeSize,
        primOpOkForSpeculation, primOpOkForSideEffects,
        primOpIsCheap, primOpFixity,

        getPrimOpResultInfo,  PrimOpResultInfo(..),

        PrimCall(..)
    ) where

#include "HsVersions.h"

import Eta.Prelude.TysPrim
import Eta.Prelude.TysWiredIn

import Eta.BasicTypes.Demand
import Eta.BasicTypes.Var              ( TyVar )
import Eta.BasicTypes.OccName          ( OccName, pprOccName, mkVarOccFS )
import Eta.Types.TyCon            ( TyCon, isPrimTyCon, tyConPrimRep, PrimRep(..) )
import Eta.Types.Type             ( Type, mkForAllTys, mkFunTy, mkFunTys, tyConAppTyCon,
                          typePrimRep )
import Eta.BasicTypes.BasicTypes       ( Arity, Fixity(..), FixityDirection(..), TupleSort(..) )
import Eta.Prelude.ForeignCall      ( CLabelString )
import Eta.BasicTypes.Unique           ( Unique, mkPrimOpIdUnique )
import Eta.Utils.Outputable
import Eta.Utils.FastTypes
import Eta.Utils.FastString
import Eta.BasicTypes.Module           ( UnitId )

{-
************************************************************************
*                                                                      *
\subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}
*                                                                      *
************************************************************************

These are in \tr{state-interface.verb} order.
-}

data PrimOp
   = CharGtOp
   | CharGeOp
   | CharEqOp
   | CharNeOp
   | CharLtOp
   | CharLeOp
   | OrdOp
   | IntAddOp
   | IntSubOp
   | IntMulOp
   | IntMulMayOfloOp
   | IntQuotOp
   | IntRemOp
   | IntQuotRemOp
   | AndIOp
   | OrIOp
   | XorIOp
   | NotIOp
   | IntNegOp
   | IntAddCOp
   | IntSubCOp
   | IntGtOp
   | IntGeOp
   | IntEqOp
   | IntNeOp
   | IntLtOp
   | IntLeOp
   | ChrOp
   | Int2WordOp
   | Int2FloatOp
   | Int2DoubleOp
   | Word2FloatOp
   | Word2DoubleOp
   | ISllOp
   | ISraOp
   | ISrlOp
   | WordAddOp
   | WordAdd2Op
   | WordSubOp
   | WordMulOp
   | WordMul2Op
   | WordQuotOp
   | WordRemOp
   | WordQuotRemOp
   | WordQuotRem2Op
   | AndOp
   | OrOp
   | XorOp
   | NotOp
   | SllOp
   | SrlOp
   | Word2IntOp
   | WordGtOp
   | WordGeOp
   | WordEqOp
   | WordNeOp
   | WordLtOp
   | WordLeOp
   | PopCnt8Op
   | PopCnt16Op
   | PopCnt32Op
   | PopCnt64Op
   | PopCntOp
   | Clz8Op
   | Clz16Op
   | Clz32Op
   | Clz64Op
   | ClzOp
   | Ctz8Op
   | Ctz16Op
   | Ctz32Op
   | Ctz64Op
   | CtzOp
   | BSwap16Op
   | BSwap32Op
   | BSwap64Op
   | BSwapOp
   | Narrow8IntOp
   | Narrow16IntOp
   | Narrow32IntOp
   | Narrow8WordOp
   | Narrow16WordOp
   | Narrow32WordOp
   | DoubleGtOp
   | DoubleGeOp
   | DoubleEqOp
   | DoubleNeOp
   | DoubleLtOp
   | DoubleLeOp
   | DoubleAddOp
   | DoubleSubOp
   | DoubleMulOp
   | DoubleDivOp
   | DoubleNegOp
   | Double2IntOp
   | Double2FloatOp
   | DoubleExpOp
   | DoubleLogOp
   | DoubleSqrtOp
   | DoubleSinOp
   | DoubleCosOp
   | DoubleTanOp
   | DoubleAsinOp
   | DoubleAcosOp
   | DoubleAtanOp
   | DoubleSinhOp
   | DoubleCoshOp
   | DoubleTanhOp
   | DoublePowerOp
   | DoubleDecode_2IntOp
   | DoubleDecode_Int64Op
   | FloatGtOp
   | FloatGeOp
   | FloatEqOp
   | FloatNeOp
   | FloatLtOp
   | FloatLeOp
   | FloatAddOp
   | FloatSubOp
   | FloatMulOp
   | FloatDivOp
   | FloatNegOp
   | Float2IntOp
   | FloatExpOp
   | FloatLogOp
   | FloatSqrtOp
   | FloatSinOp
   | FloatCosOp
   | FloatTanOp
   | FloatAsinOp
   | FloatAcosOp
   | FloatAtanOp
   | FloatSinhOp
   | FloatCoshOp
   | FloatTanhOp
   | FloatPowerOp
   | Float2DoubleOp
   | FloatDecode_IntOp
   | NewArrayOp
   | SameMutableArrayOp
   | ReadArrayOp
   | WriteArrayOp
   | SizeofArrayOp
   | SizeofMutableArrayOp
   | IndexArrayOp
   | UnsafeFreezeArrayOp
   | UnsafeThawArrayOp
   | CopyArrayOp
   | CopyMutableArrayOp
   | CloneArrayOp
   | CloneMutableArrayOp
   | FreezeArrayOp
   | ThawArrayOp
   | CasArrayOp
   | NewSmallArrayOp
   | SameSmallMutableArrayOp
   | ReadSmallArrayOp
   | WriteSmallArrayOp
   | SizeofSmallArrayOp
   | SizeofSmallMutableArrayOp
   | IndexSmallArrayOp
   | UnsafeFreezeSmallArrayOp
   | UnsafeThawSmallArrayOp
   | CopySmallArrayOp
   | CopySmallMutableArrayOp
   | CloneSmallArrayOp
   | CloneSmallMutableArrayOp
   | FreezeSmallArrayOp
   | ThawSmallArrayOp
   | CasSmallArrayOp
   | NewByteArrayOp_Char
   | NewPinnedByteArrayOp_Char
   | NewAlignedPinnedByteArrayOp_Char
   | ByteArrayContents_Char
   | SameMutableByteArrayOp
   | ShrinkMutableByteArrayOp_Char
   | ResizeMutableByteArrayOp_Char
   | UnsafeFreezeByteArrayOp
   | SizeofByteArrayOp
   | SizeofMutableByteArrayOp
   | IndexByteArrayOp_Char
   | IndexByteArrayOp_WideChar
   | IndexByteArrayOp_Int
   | IndexByteArrayOp_Word
   | IndexByteArrayOp_Addr
   | IndexByteArrayOp_Float
   | IndexByteArrayOp_Double
   | IndexByteArrayOp_StablePtr
   | IndexByteArrayOp_Int8
   | IndexByteArrayOp_Int16
   | IndexByteArrayOp_Int32
   | IndexByteArrayOp_Int64
   | IndexByteArrayOp_Word8
   | IndexByteArrayOp_Word16
   | IndexByteArrayOp_Word32
   | IndexByteArrayOp_Word64
   | ReadByteArrayOp_Char
   | ReadByteArrayOp_WideChar
   | ReadByteArrayOp_Int
   | ReadByteArrayOp_Word
   | ReadByteArrayOp_Addr
   | ReadByteArrayOp_Float
   | ReadByteArrayOp_Double
   | ReadByteArrayOp_StablePtr
   | ReadByteArrayOp_Int8
   | ReadByteArrayOp_Int16
   | ReadByteArrayOp_Int32
   | ReadByteArrayOp_Int64
   | ReadByteArrayOp_Word8
   | ReadByteArrayOp_Word16
   | ReadByteArrayOp_Word32
   | ReadByteArrayOp_Word64
   | WriteByteArrayOp_Char
   | WriteByteArrayOp_WideChar
   | WriteByteArrayOp_Int
   | WriteByteArrayOp_Word
   | WriteByteArrayOp_Addr
   | WriteByteArrayOp_Float
   | WriteByteArrayOp_Double
   | WriteByteArrayOp_StablePtr
   | WriteByteArrayOp_Int8
   | WriteByteArrayOp_Int16
   | WriteByteArrayOp_Int32
   | WriteByteArrayOp_Int64
   | WriteByteArrayOp_Word8
   | WriteByteArrayOp_Word16
   | WriteByteArrayOp_Word32
   | WriteByteArrayOp_Word64
   | CopyByteArrayOp
   | CopyMutableByteArrayOp
   | CopyByteArrayToAddrOp
   | CopyMutableByteArrayToAddrOp
   | CopyAddrToByteArrayOp
   | SetByteArrayOp
   | AtomicReadByteArrayOp_Int
   | AtomicWriteByteArrayOp_Int
   | CasByteArrayOp_Int
   | FetchAddByteArrayOp_Int
   | FetchSubByteArrayOp_Int
   | FetchAndByteArrayOp_Int
   | FetchNandByteArrayOp_Int
   | FetchOrByteArrayOp_Int
   | FetchXorByteArrayOp_Int
   | NewArrayArrayOp
   | SameMutableArrayArrayOp
   | UnsafeFreezeArrayArrayOp
   | SizeofArrayArrayOp
   | SizeofMutableArrayArrayOp
   | GetSizeofMutableByteArrayOp
   | IndexArrayArrayOp_ByteArray
   | IndexArrayArrayOp_ArrayArray
   | ReadArrayArrayOp_ByteArray
   | ReadArrayArrayOp_MutableByteArray
   | ReadArrayArrayOp_ArrayArray
   | ReadArrayArrayOp_MutableArrayArray
   | WriteArrayArrayOp_ByteArray
   | WriteArrayArrayOp_MutableByteArray
   | WriteArrayArrayOp_ArrayArray
   | WriteArrayArrayOp_MutableArrayArray
   | CopyArrayArrayOp
   | CopyMutableArrayArrayOp
   | AddrAddOp
   | AddrSubOp
   | AddrRemOp
   | Addr2IntOp
   | Int2AddrOp
   | AddrGtOp
   | AddrGeOp
   | AddrEqOp
   | AddrNeOp
   | AddrLtOp
   | AddrLeOp
   | IndexOffAddrOp_Char
   | IndexOffAddrOp_WideChar
   | IndexOffAddrOp_Int
   | IndexOffAddrOp_Word
   | IndexOffAddrOp_Addr
   | IndexOffAddrOp_Float
   | IndexOffAddrOp_Double
   | IndexOffAddrOp_StablePtr
   | IndexOffAddrOp_Int8
   | IndexOffAddrOp_Int16
   | IndexOffAddrOp_Int32
   | IndexOffAddrOp_Int64
   | IndexOffAddrOp_Word8
   | IndexOffAddrOp_Word16
   | IndexOffAddrOp_Word32
   | IndexOffAddrOp_Word64
   | ReadOffAddrOp_Char
   | ReadOffAddrOp_WideChar
   | ReadOffAddrOp_Int
   | ReadOffAddrOp_Word
   | ReadOffAddrOp_Addr
   | ReadOffAddrOp_Float
   | ReadOffAddrOp_Double
   | ReadOffAddrOp_StablePtr
   | ReadOffAddrOp_Int8
   | ReadOffAddrOp_Int16
   | ReadOffAddrOp_Int32
   | ReadOffAddrOp_Int64
   | ReadOffAddrOp_Word8
   | ReadOffAddrOp_Word16
   | ReadOffAddrOp_Word32
   | ReadOffAddrOp_Word64
   | WriteOffAddrOp_Char
   | WriteOffAddrOp_WideChar
   | WriteOffAddrOp_Int
   | WriteOffAddrOp_Word
   | WriteOffAddrOp_Addr
   | WriteOffAddrOp_Float
   | WriteOffAddrOp_Double
   | WriteOffAddrOp_StablePtr
   | WriteOffAddrOp_Int8
   | WriteOffAddrOp_Int16
   | WriteOffAddrOp_Int32
   | WriteOffAddrOp_Int64
   | WriteOffAddrOp_Word8
   | WriteOffAddrOp_Word16
   | WriteOffAddrOp_Word32
   | WriteOffAddrOp_Word64
   | NewMutVarOp
   | ReadMutVarOp
   | WriteMutVarOp
   | SameMutVarOp
   | AtomicModifyMutVarOp
   | CasMutVarOp
   | CatchOp
   | RaiseOp
   | RaiseIOOp
   | MaskAsyncExceptionsOp
   | MaskUninterruptibleOp
   | UnmaskAsyncExceptionsOp
   | MaskStatus
   | AtomicallyOp
   | RetryOp
   | CatchRetryOp
   | CatchSTMOp
   | Check
   | NewTVarOp
   | ReadTVarOp
   | ReadTVarIOOp
   | WriteTVarOp
   | SameTVarOp
   | NewMVarOp
   | TakeMVarOp
   | TryTakeMVarOp
   | PutMVarOp
   | TryPutMVarOp
   | ReadMVarOp
   | TryReadMVarOp
   | SameMVarOp
   | IsEmptyMVarOp
   | DelayOp
   | WaitReadOp
   | WaitWriteOp
   | ForkOp
   | ForkOnOp
   | KillThreadOp
   | YieldOp
   | MyThreadIdOp
   | LabelThreadOp
   | IsCurrentThreadBoundOp
   | NoDuplicateOp
   | ThreadStatusOp
   | MkWeakOp
   | MkWeakNoFinalizerOp
   | AddCFinalizerToWeakOp
   | DeRefWeakOp
   | FinalizeWeakOp
   | TouchOp
   | MakeStablePtrOp
   | DeRefStablePtrOp
   | EqStablePtrOp
   | MakeStableNameOp
   | EqStableNameOp
   | StableNameToIntOp
   | ReallyUnsafePtrEqualityOp
   | ParOp
   | SparkOp
   | SeqOp
   | GetSparkOp
   | NumSparks
   | ParGlobalOp
   | ParLocalOp
   | ParAtOp
   | ParAtAbsOp
   | ParAtRelOp
   | ParAtForNowOp
   | DataToTagOp
   | TagToEnumOp
   | AddrToAnyOp
   | MkApUpd0_Op
   | NewBCOOp
   | UnpackClosureOp
   | GetApStackValOp
   | GetCCSOfOp
   | GetCurrentCCSOp
   | TraceEventOp
   | TraceMarkerOp
   -- | VecBroadcastOp PrimOpVecCat Length Width
   -- | VecPackOp PrimOpVecCat Length Width
   -- | VecUnpackOp PrimOpVecCat Length Width
   -- | VecInsertOp PrimOpVecCat Length Width
   -- | VecAddOp PrimOpVecCat Length Width
   -- | VecSubOp PrimOpVecCat Length Width
   -- | VecMulOp PrimOpVecCat Length Width
   -- | VecDivOp PrimOpVecCat Length Width
   -- | VecQuotOp PrimOpVecCat Length Width
   -- | VecRemOp PrimOpVecCat Length Width
   -- | VecNegOp PrimOpVecCat Length Width
   -- | VecIndexByteArrayOp PrimOpVecCat Length Width
   -- | VecReadByteArrayOp PrimOpVecCat Length Width
   -- | VecWriteByteArrayOp PrimOpVecCat Length Width
   -- | VecIndexOffAddrOp PrimOpVecCat Length Width
   -- | VecReadOffAddrOp PrimOpVecCat Length Width
   -- | VecWriteOffAddrOp PrimOpVecCat Length Width
   -- | VecIndexScalarByteArrayOp PrimOpVecCat Length Width
   -- | VecReadScalarByteArrayOp PrimOpVecCat Length Width
   -- | VecWriteScalarByteArrayOp PrimOpVecCat Length Width
   -- | VecIndexScalarOffAddrOp PrimOpVecCat Length Width
   -- | VecReadScalarOffAddrOp PrimOpVecCat Length Width
   -- | VecWriteScalarOffAddrOp PrimOpVecCat Length Width
   | PrefetchByteArrayOp3
   | PrefetchMutableByteArrayOp3
   | PrefetchAddrOp3
   | PrefetchValueOp3
   | PrefetchByteArrayOp2
   | PrefetchMutableByteArrayOp2
   | PrefetchAddrOp2
   | PrefetchValueOp2
   | PrefetchByteArrayOp1
   | PrefetchMutableByteArrayOp1
   | PrefetchAddrOp1
   | PrefetchValueOp1
   | PrefetchByteArrayOp0
   | PrefetchMutableByteArrayOp0
   | PrefetchAddrOp0
   | PrefetchValueOp0
   -- ETA-specific
   | Word64Eq
   | Word64Ne
   | Word64Lt
   | Word64Le
   | Word64Gt
   | Word64Ge
   | Word64Quot
   | Word64Rem
   | Word64And
   | Word64Or
   | Word64Xor
   | Word64Not
   | Word64SllOp
   | Word64SrlOp
   | Int64Eq
   | Int64Ne
   | Int64Lt
   | Int64Le
   | Int64Gt
   | Int64Ge
   | Int64Quot
   | Int64Rem
   | Int64Add
   | Int64Sub
   | Int64Mul
   | Int64Neg
   | Int64SllOp
   | Int64SraOp
   | Int64SrlOp
   | Int642Word64
   | Word642Int64
   | Int2Int64
   | Int642Int
   | Word2Word64
   | Word64ToWord
   | DecodeDoubleInteger
   | ObjectArrayAtOp
   | ObjectArraySetOp
   | IndexJByteArrayOp
   | ReadJByteArrayOp
   | WriteJByteArrayOp
   | JByte2CharOp
   | JBool2IntOp
   | StablePtr2AddrOp
   | Addr2StablePtrOp
   | JByte2IntOp
   | Int2JBoolOp
   | ClassCastOp
   | ObjectArrayNewOp
   | ArrayLengthOp
   | IsNullObjectOp
   | Int2JByteOp
   | JShort2IntOp
   | Int2JShortOp
   | JChar2WordOp
   | Word2JCharOp
   | NewJByteArrayOp
   | NewJBooleanArrayOp
   | ReadJBooleanArrayOp
   | WriteJBooleanArrayOp
   | NewJCharArrayOp
   | ReadJCharArrayOp
   | WriteJCharArrayOp
   | NewJShortArrayOp
   | ReadJShortArrayOp
   | WriteJShortArrayOp
   | NewJIntArrayOp
   | ReadJIntArrayOp
   | WriteJIntArrayOp
   | NewJLongArrayOp
   | ReadJLongArrayOp
   | WriteJLongArrayOp
   | NewJFloatArrayOp
   | ReadJFloatArrayOp
   | WriteJFloatArrayOp
   | NewJDoubleArrayOp
   | ReadJDoubleArrayOp
   | WriteJDoubleArrayOp
   | Addr2Int64Op
   | Int642AddrOp
   | WaitConnectOp
   | WaitAcceptOp
   | FreshStateTokenOp
   | FreshObjectTokenOp
   | FreshNullObjectTokenOp
   | FloatFabsOp
   | DoubleFabsOp

-- Used for the Ord instance
primOpTag :: PrimOp -> Int
primOpTag op = iBox (tagOf_PrimOp op)

maxPrimOpTag :: Int
maxPrimOpTag = 1145

tagOf_PrimOp :: PrimOp -> FastInt
tagOf_PrimOp CharGtOp = _ILIT(1)
tagOf_PrimOp CharGeOp = _ILIT(2)
tagOf_PrimOp CharEqOp = _ILIT(3)
tagOf_PrimOp CharNeOp = _ILIT(4)
tagOf_PrimOp CharLtOp = _ILIT(5)
tagOf_PrimOp CharLeOp = _ILIT(6)
tagOf_PrimOp OrdOp = _ILIT(7)
tagOf_PrimOp IntAddOp = _ILIT(8)
tagOf_PrimOp IntSubOp = _ILIT(9)
tagOf_PrimOp IntMulOp = _ILIT(10)
tagOf_PrimOp IntMulMayOfloOp = _ILIT(11)
tagOf_PrimOp IntQuotOp = _ILIT(12)
tagOf_PrimOp IntRemOp = _ILIT(13)
tagOf_PrimOp IntQuotRemOp = _ILIT(14)
tagOf_PrimOp AndIOp = _ILIT(15)
tagOf_PrimOp OrIOp = _ILIT(16)
tagOf_PrimOp XorIOp = _ILIT(17)
tagOf_PrimOp NotIOp = _ILIT(18)
tagOf_PrimOp IntNegOp = _ILIT(19)
tagOf_PrimOp IntAddCOp = _ILIT(20)
tagOf_PrimOp IntSubCOp = _ILIT(21)
tagOf_PrimOp IntGtOp = _ILIT(22)
tagOf_PrimOp IntGeOp = _ILIT(23)
tagOf_PrimOp IntEqOp = _ILIT(24)
tagOf_PrimOp IntNeOp = _ILIT(25)
tagOf_PrimOp IntLtOp = _ILIT(26)
tagOf_PrimOp IntLeOp = _ILIT(27)
tagOf_PrimOp ChrOp = _ILIT(28)
tagOf_PrimOp Int2WordOp = _ILIT(29)
tagOf_PrimOp Int2FloatOp = _ILIT(30)
tagOf_PrimOp Int2DoubleOp = _ILIT(31)
tagOf_PrimOp Word2FloatOp = _ILIT(32)
tagOf_PrimOp Word2DoubleOp = _ILIT(33)
tagOf_PrimOp ISllOp = _ILIT(34)
tagOf_PrimOp ISraOp = _ILIT(35)
tagOf_PrimOp ISrlOp = _ILIT(36)
tagOf_PrimOp WordAddOp = _ILIT(37)
tagOf_PrimOp WordAdd2Op = _ILIT(38)
tagOf_PrimOp WordSubOp = _ILIT(39)
tagOf_PrimOp WordMulOp = _ILIT(40)
tagOf_PrimOp WordMul2Op = _ILIT(41)
tagOf_PrimOp WordQuotOp = _ILIT(42)
tagOf_PrimOp WordRemOp = _ILIT(43)
tagOf_PrimOp WordQuotRemOp = _ILIT(44)
tagOf_PrimOp WordQuotRem2Op = _ILIT(45)
tagOf_PrimOp AndOp = _ILIT(46)
tagOf_PrimOp OrOp = _ILIT(47)
tagOf_PrimOp XorOp = _ILIT(48)
tagOf_PrimOp NotOp = _ILIT(49)
tagOf_PrimOp SllOp = _ILIT(50)
tagOf_PrimOp SrlOp = _ILIT(51)
tagOf_PrimOp Word2IntOp = _ILIT(52)
tagOf_PrimOp WordGtOp = _ILIT(53)
tagOf_PrimOp WordGeOp = _ILIT(54)
tagOf_PrimOp WordEqOp = _ILIT(55)
tagOf_PrimOp WordNeOp = _ILIT(56)
tagOf_PrimOp WordLtOp = _ILIT(57)
tagOf_PrimOp WordLeOp = _ILIT(58)
tagOf_PrimOp PopCnt8Op = _ILIT(59)
tagOf_PrimOp PopCnt16Op = _ILIT(60)
tagOf_PrimOp PopCnt32Op = _ILIT(61)
tagOf_PrimOp PopCnt64Op = _ILIT(62)
tagOf_PrimOp PopCntOp = _ILIT(63)
tagOf_PrimOp Clz8Op = _ILIT(64)
tagOf_PrimOp Clz16Op = _ILIT(65)
tagOf_PrimOp Clz32Op = _ILIT(66)
tagOf_PrimOp Clz64Op = _ILIT(67)
tagOf_PrimOp ClzOp = _ILIT(68)
tagOf_PrimOp Ctz8Op = _ILIT(69)
tagOf_PrimOp Ctz16Op = _ILIT(70)
tagOf_PrimOp Ctz32Op = _ILIT(71)
tagOf_PrimOp Ctz64Op = _ILIT(72)
tagOf_PrimOp CtzOp = _ILIT(73)
tagOf_PrimOp BSwap16Op = _ILIT(74)
tagOf_PrimOp BSwap32Op = _ILIT(75)
tagOf_PrimOp BSwap64Op = _ILIT(76)
tagOf_PrimOp BSwapOp = _ILIT(77)
tagOf_PrimOp Narrow8IntOp = _ILIT(78)
tagOf_PrimOp Narrow16IntOp = _ILIT(79)
tagOf_PrimOp Narrow32IntOp = _ILIT(80)
tagOf_PrimOp Narrow8WordOp = _ILIT(81)
tagOf_PrimOp Narrow16WordOp = _ILIT(82)
tagOf_PrimOp Narrow32WordOp = _ILIT(83)
tagOf_PrimOp DoubleGtOp = _ILIT(84)
tagOf_PrimOp DoubleGeOp = _ILIT(85)
tagOf_PrimOp DoubleEqOp = _ILIT(86)
tagOf_PrimOp DoubleNeOp = _ILIT(87)
tagOf_PrimOp DoubleLtOp = _ILIT(88)
tagOf_PrimOp DoubleLeOp = _ILIT(89)
tagOf_PrimOp DoubleAddOp = _ILIT(90)
tagOf_PrimOp DoubleSubOp = _ILIT(91)
tagOf_PrimOp DoubleMulOp = _ILIT(92)
tagOf_PrimOp DoubleDivOp = _ILIT(93)
tagOf_PrimOp DoubleNegOp = _ILIT(94)
tagOf_PrimOp Double2IntOp = _ILIT(95)
tagOf_PrimOp Double2FloatOp = _ILIT(96)
tagOf_PrimOp DoubleExpOp = _ILIT(97)
tagOf_PrimOp DoubleLogOp = _ILIT(98)
tagOf_PrimOp DoubleSqrtOp = _ILIT(99)
tagOf_PrimOp DoubleSinOp = _ILIT(100)
tagOf_PrimOp DoubleCosOp = _ILIT(101)
tagOf_PrimOp DoubleTanOp = _ILIT(102)
tagOf_PrimOp DoubleAsinOp = _ILIT(103)
tagOf_PrimOp DoubleAcosOp = _ILIT(104)
tagOf_PrimOp DoubleAtanOp = _ILIT(105)
tagOf_PrimOp DoubleSinhOp = _ILIT(106)
tagOf_PrimOp DoubleCoshOp = _ILIT(107)
tagOf_PrimOp DoubleTanhOp = _ILIT(108)
tagOf_PrimOp DoublePowerOp = _ILIT(109)
tagOf_PrimOp DoubleDecode_2IntOp = _ILIT(110)
tagOf_PrimOp DoubleDecode_Int64Op = _ILIT(111)
tagOf_PrimOp FloatGtOp = _ILIT(112)
tagOf_PrimOp FloatGeOp = _ILIT(113)
tagOf_PrimOp FloatEqOp = _ILIT(114)
tagOf_PrimOp FloatNeOp = _ILIT(115)
tagOf_PrimOp FloatLtOp = _ILIT(116)
tagOf_PrimOp FloatLeOp = _ILIT(117)
tagOf_PrimOp FloatAddOp = _ILIT(118)
tagOf_PrimOp FloatSubOp = _ILIT(119)
tagOf_PrimOp FloatMulOp = _ILIT(120)
tagOf_PrimOp FloatDivOp = _ILIT(121)
tagOf_PrimOp FloatNegOp = _ILIT(122)
tagOf_PrimOp Float2IntOp = _ILIT(123)
tagOf_PrimOp FloatExpOp = _ILIT(124)
tagOf_PrimOp FloatLogOp = _ILIT(125)
tagOf_PrimOp FloatSqrtOp = _ILIT(126)
tagOf_PrimOp FloatSinOp = _ILIT(127)
tagOf_PrimOp FloatCosOp = _ILIT(128)
tagOf_PrimOp FloatTanOp = _ILIT(129)
tagOf_PrimOp FloatAsinOp = _ILIT(130)
tagOf_PrimOp FloatAcosOp = _ILIT(131)
tagOf_PrimOp FloatAtanOp = _ILIT(132)
tagOf_PrimOp FloatSinhOp = _ILIT(133)
tagOf_PrimOp FloatCoshOp = _ILIT(134)
tagOf_PrimOp FloatTanhOp = _ILIT(135)
tagOf_PrimOp FloatPowerOp = _ILIT(136)
tagOf_PrimOp Float2DoubleOp = _ILIT(137)
tagOf_PrimOp FloatDecode_IntOp = _ILIT(138)
tagOf_PrimOp NewArrayOp = _ILIT(139)
tagOf_PrimOp SameMutableArrayOp = _ILIT(140)
tagOf_PrimOp ReadArrayOp = _ILIT(141)
tagOf_PrimOp WriteArrayOp = _ILIT(142)
tagOf_PrimOp SizeofArrayOp = _ILIT(143)
tagOf_PrimOp SizeofMutableArrayOp = _ILIT(144)
tagOf_PrimOp IndexArrayOp = _ILIT(145)
tagOf_PrimOp UnsafeFreezeArrayOp = _ILIT(146)
tagOf_PrimOp UnsafeThawArrayOp = _ILIT(147)
tagOf_PrimOp CopyArrayOp = _ILIT(148)
tagOf_PrimOp CopyMutableArrayOp = _ILIT(149)
tagOf_PrimOp CloneArrayOp = _ILIT(150)
tagOf_PrimOp CloneMutableArrayOp = _ILIT(151)
tagOf_PrimOp FreezeArrayOp = _ILIT(152)
tagOf_PrimOp ThawArrayOp = _ILIT(153)
tagOf_PrimOp CasArrayOp = _ILIT(154)
tagOf_PrimOp NewSmallArrayOp = _ILIT(155)
tagOf_PrimOp SameSmallMutableArrayOp = _ILIT(156)
tagOf_PrimOp ReadSmallArrayOp = _ILIT(157)
tagOf_PrimOp WriteSmallArrayOp = _ILIT(158)
tagOf_PrimOp SizeofSmallArrayOp = _ILIT(159)
tagOf_PrimOp SizeofSmallMutableArrayOp = _ILIT(160)
tagOf_PrimOp IndexSmallArrayOp = _ILIT(161)
tagOf_PrimOp UnsafeFreezeSmallArrayOp = _ILIT(162)
tagOf_PrimOp UnsafeThawSmallArrayOp = _ILIT(163)
tagOf_PrimOp CopySmallArrayOp = _ILIT(164)
tagOf_PrimOp CopySmallMutableArrayOp = _ILIT(165)
tagOf_PrimOp CloneSmallArrayOp = _ILIT(166)
tagOf_PrimOp CloneSmallMutableArrayOp = _ILIT(167)
tagOf_PrimOp FreezeSmallArrayOp = _ILIT(168)
tagOf_PrimOp ThawSmallArrayOp = _ILIT(169)
tagOf_PrimOp CasSmallArrayOp = _ILIT(170)
tagOf_PrimOp NewByteArrayOp_Char = _ILIT(171)
tagOf_PrimOp NewPinnedByteArrayOp_Char = _ILIT(172)
tagOf_PrimOp NewAlignedPinnedByteArrayOp_Char = _ILIT(173)
tagOf_PrimOp ByteArrayContents_Char = _ILIT(174)
tagOf_PrimOp SameMutableByteArrayOp = _ILIT(175)
tagOf_PrimOp ShrinkMutableByteArrayOp_Char = _ILIT(176)
tagOf_PrimOp ResizeMutableByteArrayOp_Char = _ILIT(177)
tagOf_PrimOp UnsafeFreezeByteArrayOp = _ILIT(178)
tagOf_PrimOp SizeofByteArrayOp = _ILIT(179)
tagOf_PrimOp SizeofMutableByteArrayOp = _ILIT(180)
tagOf_PrimOp IndexByteArrayOp_Char = _ILIT(181)
tagOf_PrimOp IndexByteArrayOp_WideChar = _ILIT(182)
tagOf_PrimOp IndexByteArrayOp_Int = _ILIT(183)
tagOf_PrimOp IndexByteArrayOp_Word = _ILIT(184)
tagOf_PrimOp IndexByteArrayOp_Addr = _ILIT(185)
tagOf_PrimOp IndexByteArrayOp_Float = _ILIT(186)
tagOf_PrimOp IndexByteArrayOp_Double = _ILIT(187)
tagOf_PrimOp IndexByteArrayOp_StablePtr = _ILIT(188)
tagOf_PrimOp IndexByteArrayOp_Int8 = _ILIT(189)
tagOf_PrimOp IndexByteArrayOp_Int16 = _ILIT(190)
tagOf_PrimOp IndexByteArrayOp_Int32 = _ILIT(191)
tagOf_PrimOp IndexByteArrayOp_Int64 = _ILIT(192)
tagOf_PrimOp IndexByteArrayOp_Word8 = _ILIT(193)
tagOf_PrimOp IndexByteArrayOp_Word16 = _ILIT(194)
tagOf_PrimOp IndexByteArrayOp_Word32 = _ILIT(195)
tagOf_PrimOp IndexByteArrayOp_Word64 = _ILIT(196)
tagOf_PrimOp ReadByteArrayOp_Char = _ILIT(197)
tagOf_PrimOp ReadByteArrayOp_WideChar = _ILIT(198)
tagOf_PrimOp ReadByteArrayOp_Int = _ILIT(199)
tagOf_PrimOp ReadByteArrayOp_Word = _ILIT(200)
tagOf_PrimOp ReadByteArrayOp_Addr = _ILIT(201)
tagOf_PrimOp ReadByteArrayOp_Float = _ILIT(202)
tagOf_PrimOp ReadByteArrayOp_Double = _ILIT(203)
tagOf_PrimOp ReadByteArrayOp_StablePtr = _ILIT(204)
tagOf_PrimOp ReadByteArrayOp_Int8 = _ILIT(205)
tagOf_PrimOp ReadByteArrayOp_Int16 = _ILIT(206)
tagOf_PrimOp ReadByteArrayOp_Int32 = _ILIT(207)
tagOf_PrimOp ReadByteArrayOp_Int64 = _ILIT(208)
tagOf_PrimOp ReadByteArrayOp_Word8 = _ILIT(209)
tagOf_PrimOp ReadByteArrayOp_Word16 = _ILIT(210)
tagOf_PrimOp ReadByteArrayOp_Word32 = _ILIT(211)
tagOf_PrimOp ReadByteArrayOp_Word64 = _ILIT(212)
tagOf_PrimOp WriteByteArrayOp_Char = _ILIT(213)
tagOf_PrimOp WriteByteArrayOp_WideChar = _ILIT(214)
tagOf_PrimOp WriteByteArrayOp_Int = _ILIT(215)
tagOf_PrimOp WriteByteArrayOp_Word = _ILIT(216)
tagOf_PrimOp WriteByteArrayOp_Addr = _ILIT(217)
tagOf_PrimOp WriteByteArrayOp_Float = _ILIT(218)
tagOf_PrimOp WriteByteArrayOp_Double = _ILIT(219)
tagOf_PrimOp WriteByteArrayOp_StablePtr = _ILIT(220)
tagOf_PrimOp WriteByteArrayOp_Int8 = _ILIT(221)
tagOf_PrimOp WriteByteArrayOp_Int16 = _ILIT(222)
tagOf_PrimOp WriteByteArrayOp_Int32 = _ILIT(223)
tagOf_PrimOp WriteByteArrayOp_Int64 = _ILIT(224)
tagOf_PrimOp WriteByteArrayOp_Word8 = _ILIT(225)
tagOf_PrimOp WriteByteArrayOp_Word16 = _ILIT(226)
tagOf_PrimOp WriteByteArrayOp_Word32 = _ILIT(227)
tagOf_PrimOp WriteByteArrayOp_Word64 = _ILIT(228)
tagOf_PrimOp CopyByteArrayOp = _ILIT(229)
tagOf_PrimOp CopyMutableByteArrayOp = _ILIT(230)
tagOf_PrimOp CopyByteArrayToAddrOp = _ILIT(231)
tagOf_PrimOp CopyMutableByteArrayToAddrOp = _ILIT(232)
tagOf_PrimOp CopyAddrToByteArrayOp = _ILIT(233)
tagOf_PrimOp SetByteArrayOp = _ILIT(234)
tagOf_PrimOp AtomicReadByteArrayOp_Int = _ILIT(235)
tagOf_PrimOp AtomicWriteByteArrayOp_Int = _ILIT(236)
tagOf_PrimOp CasByteArrayOp_Int = _ILIT(237)
tagOf_PrimOp FetchAddByteArrayOp_Int = _ILIT(238)
tagOf_PrimOp FetchSubByteArrayOp_Int = _ILIT(239)
tagOf_PrimOp FetchAndByteArrayOp_Int = _ILIT(240)
tagOf_PrimOp FetchNandByteArrayOp_Int = _ILIT(241)
tagOf_PrimOp FetchOrByteArrayOp_Int = _ILIT(242)
tagOf_PrimOp FetchXorByteArrayOp_Int = _ILIT(243)
tagOf_PrimOp NewArrayArrayOp = _ILIT(244)
tagOf_PrimOp SameMutableArrayArrayOp = _ILIT(245)
tagOf_PrimOp UnsafeFreezeArrayArrayOp = _ILIT(246)
tagOf_PrimOp SizeofArrayArrayOp = _ILIT(247)
tagOf_PrimOp SizeofMutableArrayArrayOp = _ILIT(248)
tagOf_PrimOp IndexArrayArrayOp_ByteArray = _ILIT(249)
tagOf_PrimOp IndexArrayArrayOp_ArrayArray = _ILIT(250)
tagOf_PrimOp ReadArrayArrayOp_ByteArray = _ILIT(251)
tagOf_PrimOp ReadArrayArrayOp_MutableByteArray = _ILIT(252)
tagOf_PrimOp ReadArrayArrayOp_ArrayArray = _ILIT(253)
tagOf_PrimOp ReadArrayArrayOp_MutableArrayArray = _ILIT(254)
tagOf_PrimOp WriteArrayArrayOp_ByteArray = _ILIT(255)
tagOf_PrimOp WriteArrayArrayOp_MutableByteArray = _ILIT(256)
tagOf_PrimOp WriteArrayArrayOp_ArrayArray = _ILIT(257)
tagOf_PrimOp WriteArrayArrayOp_MutableArrayArray = _ILIT(258)
tagOf_PrimOp CopyArrayArrayOp = _ILIT(259)
tagOf_PrimOp CopyMutableArrayArrayOp = _ILIT(260)
tagOf_PrimOp AddrAddOp = _ILIT(261)
tagOf_PrimOp AddrSubOp = _ILIT(262)
tagOf_PrimOp AddrRemOp = _ILIT(263)
tagOf_PrimOp Addr2IntOp = _ILIT(264)
tagOf_PrimOp Int2AddrOp = _ILIT(265)
tagOf_PrimOp AddrGtOp = _ILIT(266)
tagOf_PrimOp AddrGeOp = _ILIT(267)
tagOf_PrimOp AddrEqOp = _ILIT(268)
tagOf_PrimOp AddrNeOp = _ILIT(269)
tagOf_PrimOp AddrLtOp = _ILIT(270)
tagOf_PrimOp AddrLeOp = _ILIT(271)
tagOf_PrimOp IndexOffAddrOp_Char = _ILIT(272)
tagOf_PrimOp IndexOffAddrOp_WideChar = _ILIT(273)
tagOf_PrimOp IndexOffAddrOp_Int = _ILIT(274)
tagOf_PrimOp IndexOffAddrOp_Word = _ILIT(275)
tagOf_PrimOp IndexOffAddrOp_Addr = _ILIT(276)
tagOf_PrimOp IndexOffAddrOp_Float = _ILIT(277)
tagOf_PrimOp IndexOffAddrOp_Double = _ILIT(278)
tagOf_PrimOp IndexOffAddrOp_StablePtr = _ILIT(279)
tagOf_PrimOp IndexOffAddrOp_Int8 = _ILIT(280)
tagOf_PrimOp IndexOffAddrOp_Int16 = _ILIT(281)
tagOf_PrimOp IndexOffAddrOp_Int32 = _ILIT(282)
tagOf_PrimOp IndexOffAddrOp_Int64 = _ILIT(283)
tagOf_PrimOp IndexOffAddrOp_Word8 = _ILIT(284)
tagOf_PrimOp IndexOffAddrOp_Word16 = _ILIT(285)
tagOf_PrimOp IndexOffAddrOp_Word32 = _ILIT(286)
tagOf_PrimOp IndexOffAddrOp_Word64 = _ILIT(287)
tagOf_PrimOp ReadOffAddrOp_Char = _ILIT(288)
tagOf_PrimOp ReadOffAddrOp_WideChar = _ILIT(289)
tagOf_PrimOp ReadOffAddrOp_Int = _ILIT(290)
tagOf_PrimOp ReadOffAddrOp_Word = _ILIT(291)
tagOf_PrimOp ReadOffAddrOp_Addr = _ILIT(292)
tagOf_PrimOp ReadOffAddrOp_Float = _ILIT(293)
tagOf_PrimOp ReadOffAddrOp_Double = _ILIT(294)
tagOf_PrimOp ReadOffAddrOp_StablePtr = _ILIT(295)
tagOf_PrimOp ReadOffAddrOp_Int8 = _ILIT(296)
tagOf_PrimOp ReadOffAddrOp_Int16 = _ILIT(297)
tagOf_PrimOp ReadOffAddrOp_Int32 = _ILIT(298)
tagOf_PrimOp ReadOffAddrOp_Int64 = _ILIT(299)
tagOf_PrimOp ReadOffAddrOp_Word8 = _ILIT(300)
tagOf_PrimOp ReadOffAddrOp_Word16 = _ILIT(301)
tagOf_PrimOp ReadOffAddrOp_Word32 = _ILIT(302)
tagOf_PrimOp ReadOffAddrOp_Word64 = _ILIT(303)
tagOf_PrimOp WriteOffAddrOp_Char = _ILIT(304)
tagOf_PrimOp WriteOffAddrOp_WideChar = _ILIT(305)
tagOf_PrimOp WriteOffAddrOp_Int = _ILIT(306)
tagOf_PrimOp WriteOffAddrOp_Word = _ILIT(307)
tagOf_PrimOp WriteOffAddrOp_Addr = _ILIT(308)
tagOf_PrimOp WriteOffAddrOp_Float = _ILIT(309)
tagOf_PrimOp WriteOffAddrOp_Double = _ILIT(310)
tagOf_PrimOp WriteOffAddrOp_StablePtr = _ILIT(311)
tagOf_PrimOp WriteOffAddrOp_Int8 = _ILIT(312)
tagOf_PrimOp WriteOffAddrOp_Int16 = _ILIT(313)
tagOf_PrimOp WriteOffAddrOp_Int32 = _ILIT(314)
tagOf_PrimOp WriteOffAddrOp_Int64 = _ILIT(315)
tagOf_PrimOp WriteOffAddrOp_Word8 = _ILIT(316)
tagOf_PrimOp WriteOffAddrOp_Word16 = _ILIT(317)
tagOf_PrimOp WriteOffAddrOp_Word32 = _ILIT(318)
tagOf_PrimOp WriteOffAddrOp_Word64 = _ILIT(319)
tagOf_PrimOp NewMutVarOp = _ILIT(320)
tagOf_PrimOp ReadMutVarOp = _ILIT(321)
tagOf_PrimOp WriteMutVarOp = _ILIT(322)
tagOf_PrimOp SameMutVarOp = _ILIT(323)
tagOf_PrimOp AtomicModifyMutVarOp = _ILIT(324)
tagOf_PrimOp CasMutVarOp = _ILIT(325)
tagOf_PrimOp CatchOp = _ILIT(326)
tagOf_PrimOp RaiseOp = _ILIT(327)
tagOf_PrimOp RaiseIOOp = _ILIT(328)
tagOf_PrimOp MaskAsyncExceptionsOp = _ILIT(329)
tagOf_PrimOp MaskUninterruptibleOp = _ILIT(330)
tagOf_PrimOp UnmaskAsyncExceptionsOp = _ILIT(331)
tagOf_PrimOp MaskStatus = _ILIT(332)
tagOf_PrimOp AtomicallyOp = _ILIT(333)
tagOf_PrimOp RetryOp = _ILIT(334)
tagOf_PrimOp CatchRetryOp = _ILIT(335)
tagOf_PrimOp CatchSTMOp = _ILIT(336)
tagOf_PrimOp Check = _ILIT(337)
tagOf_PrimOp NewTVarOp = _ILIT(338)
tagOf_PrimOp ReadTVarOp = _ILIT(339)
tagOf_PrimOp ReadTVarIOOp = _ILIT(340)
tagOf_PrimOp WriteTVarOp = _ILIT(341)
tagOf_PrimOp SameTVarOp = _ILIT(342)
tagOf_PrimOp NewMVarOp = _ILIT(343)
tagOf_PrimOp TakeMVarOp = _ILIT(344)
tagOf_PrimOp TryTakeMVarOp = _ILIT(345)
tagOf_PrimOp PutMVarOp = _ILIT(346)
tagOf_PrimOp TryPutMVarOp = _ILIT(347)
tagOf_PrimOp ReadMVarOp = _ILIT(348)
tagOf_PrimOp TryReadMVarOp = _ILIT(349)
tagOf_PrimOp SameMVarOp = _ILIT(350)
tagOf_PrimOp IsEmptyMVarOp = _ILIT(351)
tagOf_PrimOp DelayOp = _ILIT(352)
tagOf_PrimOp WaitReadOp = _ILIT(353)
tagOf_PrimOp WaitWriteOp = _ILIT(354)
tagOf_PrimOp ForkOp = _ILIT(355)
tagOf_PrimOp ForkOnOp = _ILIT(356)
tagOf_PrimOp KillThreadOp = _ILIT(357)
tagOf_PrimOp YieldOp = _ILIT(358)
tagOf_PrimOp MyThreadIdOp = _ILIT(359)
tagOf_PrimOp LabelThreadOp = _ILIT(360)
tagOf_PrimOp IsCurrentThreadBoundOp = _ILIT(361)
tagOf_PrimOp NoDuplicateOp = _ILIT(362)
tagOf_PrimOp ThreadStatusOp = _ILIT(363)
tagOf_PrimOp MkWeakOp = _ILIT(364)
tagOf_PrimOp MkWeakNoFinalizerOp = _ILIT(365)
tagOf_PrimOp AddCFinalizerToWeakOp = _ILIT(366)
tagOf_PrimOp DeRefWeakOp = _ILIT(367)
tagOf_PrimOp FinalizeWeakOp = _ILIT(368)
tagOf_PrimOp TouchOp = _ILIT(369)
tagOf_PrimOp MakeStablePtrOp = _ILIT(370)
tagOf_PrimOp DeRefStablePtrOp = _ILIT(371)
tagOf_PrimOp EqStablePtrOp = _ILIT(372)
tagOf_PrimOp MakeStableNameOp = _ILIT(373)
tagOf_PrimOp EqStableNameOp = _ILIT(374)
tagOf_PrimOp StableNameToIntOp = _ILIT(375)
tagOf_PrimOp ReallyUnsafePtrEqualityOp = _ILIT(376)
tagOf_PrimOp ParOp = _ILIT(377)
tagOf_PrimOp SparkOp = _ILIT(378)
tagOf_PrimOp SeqOp = _ILIT(379)
tagOf_PrimOp GetSparkOp = _ILIT(380)
tagOf_PrimOp NumSparks = _ILIT(381)
tagOf_PrimOp ParGlobalOp = _ILIT(382)
tagOf_PrimOp ParLocalOp = _ILIT(383)
tagOf_PrimOp ParAtOp = _ILIT(384)
tagOf_PrimOp ParAtAbsOp = _ILIT(385)
tagOf_PrimOp ParAtRelOp = _ILIT(386)
tagOf_PrimOp ParAtForNowOp = _ILIT(387)
tagOf_PrimOp DataToTagOp = _ILIT(388)
tagOf_PrimOp TagToEnumOp = _ILIT(389)
tagOf_PrimOp AddrToAnyOp = _ILIT(390)
tagOf_PrimOp MkApUpd0_Op = _ILIT(391)
tagOf_PrimOp NewBCOOp = _ILIT(392)
tagOf_PrimOp UnpackClosureOp = _ILIT(393)
tagOf_PrimOp GetApStackValOp = _ILIT(394)
tagOf_PrimOp GetCCSOfOp = _ILIT(395)
tagOf_PrimOp GetCurrentCCSOp = _ILIT(396)
tagOf_PrimOp TraceEventOp = _ILIT(397)
tagOf_PrimOp TraceMarkerOp = _ILIT(398)
-- tagOf_PrimOp (VecBroadcastOp IntVec 16 W8) = _ILIT(399)
-- tagOf_PrimOp (VecBroadcastOp IntVec 8 W16) = _ILIT(400)
-- tagOf_PrimOp (VecBroadcastOp IntVec 4 W32) = _ILIT(401)
-- tagOf_PrimOp (VecBroadcastOp IntVec 2 W64) = _ILIT(402)
-- tagOf_PrimOp (VecBroadcastOp IntVec 32 W8) = _ILIT(403)
-- tagOf_PrimOp (VecBroadcastOp IntVec 16 W16) = _ILIT(404)
-- tagOf_PrimOp (VecBroadcastOp IntVec 8 W32) = _ILIT(405)
-- tagOf_PrimOp (VecBroadcastOp IntVec 4 W64) = _ILIT(406)
-- tagOf_PrimOp (VecBroadcastOp IntVec 64 W8) = _ILIT(407)
-- tagOf_PrimOp (VecBroadcastOp IntVec 32 W16) = _ILIT(408)
-- tagOf_PrimOp (VecBroadcastOp IntVec 16 W32) = _ILIT(409)
-- tagOf_PrimOp (VecBroadcastOp IntVec 8 W64) = _ILIT(410)
-- tagOf_PrimOp (VecBroadcastOp WordVec 16 W8) = _ILIT(411)
-- tagOf_PrimOp (VecBroadcastOp WordVec 8 W16) = _ILIT(412)
-- tagOf_PrimOp (VecBroadcastOp WordVec 4 W32) = _ILIT(413)
-- tagOf_PrimOp (VecBroadcastOp WordVec 2 W64) = _ILIT(414)
-- tagOf_PrimOp (VecBroadcastOp WordVec 32 W8) = _ILIT(415)
-- tagOf_PrimOp (VecBroadcastOp WordVec 16 W16) = _ILIT(416)
-- tagOf_PrimOp (VecBroadcastOp WordVec 8 W32) = _ILIT(417)
-- tagOf_PrimOp (VecBroadcastOp WordVec 4 W64) = _ILIT(418)
-- tagOf_PrimOp (VecBroadcastOp WordVec 64 W8) = _ILIT(419)
-- tagOf_PrimOp (VecBroadcastOp WordVec 32 W16) = _ILIT(420)
-- tagOf_PrimOp (VecBroadcastOp WordVec 16 W32) = _ILIT(421)
-- tagOf_PrimOp (VecBroadcastOp WordVec 8 W64) = _ILIT(422)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 4 W32) = _ILIT(423)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 2 W64) = _ILIT(424)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 8 W32) = _ILIT(425)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 4 W64) = _ILIT(426)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 16 W32) = _ILIT(427)
-- tagOf_PrimOp (VecBroadcastOp FloatVec 8 W64) = _ILIT(428)
-- tagOf_PrimOp (VecPackOp IntVec 16 W8) = _ILIT(429)
-- tagOf_PrimOp (VecPackOp IntVec 8 W16) = _ILIT(430)
-- tagOf_PrimOp (VecPackOp IntVec 4 W32) = _ILIT(431)
-- tagOf_PrimOp (VecPackOp IntVec 2 W64) = _ILIT(432)
-- tagOf_PrimOp (VecPackOp IntVec 32 W8) = _ILIT(433)
-- tagOf_PrimOp (VecPackOp IntVec 16 W16) = _ILIT(434)
-- tagOf_PrimOp (VecPackOp IntVec 8 W32) = _ILIT(435)
-- tagOf_PrimOp (VecPackOp IntVec 4 W64) = _ILIT(436)
-- tagOf_PrimOp (VecPackOp IntVec 64 W8) = _ILIT(437)
-- tagOf_PrimOp (VecPackOp IntVec 32 W16) = _ILIT(438)
-- tagOf_PrimOp (VecPackOp IntVec 16 W32) = _ILIT(439)
-- tagOf_PrimOp (VecPackOp IntVec 8 W64) = _ILIT(440)
-- tagOf_PrimOp (VecPackOp WordVec 16 W8) = _ILIT(441)
-- tagOf_PrimOp (VecPackOp WordVec 8 W16) = _ILIT(442)
-- tagOf_PrimOp (VecPackOp WordVec 4 W32) = _ILIT(443)
-- tagOf_PrimOp (VecPackOp WordVec 2 W64) = _ILIT(444)
-- tagOf_PrimOp (VecPackOp WordVec 32 W8) = _ILIT(445)
-- tagOf_PrimOp (VecPackOp WordVec 16 W16) = _ILIT(446)
-- tagOf_PrimOp (VecPackOp WordVec 8 W32) = _ILIT(447)
-- tagOf_PrimOp (VecPackOp WordVec 4 W64) = _ILIT(448)
-- tagOf_PrimOp (VecPackOp WordVec 64 W8) = _ILIT(449)
-- tagOf_PrimOp (VecPackOp WordVec 32 W16) = _ILIT(450)
-- tagOf_PrimOp (VecPackOp WordVec 16 W32) = _ILIT(451)
-- tagOf_PrimOp (VecPackOp WordVec 8 W64) = _ILIT(452)
-- tagOf_PrimOp (VecPackOp FloatVec 4 W32) = _ILIT(453)
-- tagOf_PrimOp (VecPackOp FloatVec 2 W64) = _ILIT(454)
-- tagOf_PrimOp (VecPackOp FloatVec 8 W32) = _ILIT(455)
-- tagOf_PrimOp (VecPackOp FloatVec 4 W64) = _ILIT(456)
-- tagOf_PrimOp (VecPackOp FloatVec 16 W32) = _ILIT(457)
-- tagOf_PrimOp (VecPackOp FloatVec 8 W64) = _ILIT(458)
-- tagOf_PrimOp (VecUnpackOp IntVec 16 W8) = _ILIT(459)
-- tagOf_PrimOp (VecUnpackOp IntVec 8 W16) = _ILIT(460)
-- tagOf_PrimOp (VecUnpackOp IntVec 4 W32) = _ILIT(461)
-- tagOf_PrimOp (VecUnpackOp IntVec 2 W64) = _ILIT(462)
-- tagOf_PrimOp (VecUnpackOp IntVec 32 W8) = _ILIT(463)
-- tagOf_PrimOp (VecUnpackOp IntVec 16 W16) = _ILIT(464)
-- tagOf_PrimOp (VecUnpackOp IntVec 8 W32) = _ILIT(465)
-- tagOf_PrimOp (VecUnpackOp IntVec 4 W64) = _ILIT(466)
-- tagOf_PrimOp (VecUnpackOp IntVec 64 W8) = _ILIT(467)
-- tagOf_PrimOp (VecUnpackOp IntVec 32 W16) = _ILIT(468)
-- tagOf_PrimOp (VecUnpackOp IntVec 16 W32) = _ILIT(469)
-- tagOf_PrimOp (VecUnpackOp IntVec 8 W64) = _ILIT(470)
-- tagOf_PrimOp (VecUnpackOp WordVec 16 W8) = _ILIT(471)
-- tagOf_PrimOp (VecUnpackOp WordVec 8 W16) = _ILIT(472)
-- tagOf_PrimOp (VecUnpackOp WordVec 4 W32) = _ILIT(473)
-- tagOf_PrimOp (VecUnpackOp WordVec 2 W64) = _ILIT(474)
-- tagOf_PrimOp (VecUnpackOp WordVec 32 W8) = _ILIT(475)
-- tagOf_PrimOp (VecUnpackOp WordVec 16 W16) = _ILIT(476)
-- tagOf_PrimOp (VecUnpackOp WordVec 8 W32) = _ILIT(477)
-- tagOf_PrimOp (VecUnpackOp WordVec 4 W64) = _ILIT(478)
-- tagOf_PrimOp (VecUnpackOp WordVec 64 W8) = _ILIT(479)
-- tagOf_PrimOp (VecUnpackOp WordVec 32 W16) = _ILIT(480)
-- tagOf_PrimOp (VecUnpackOp WordVec 16 W32) = _ILIT(481)
-- tagOf_PrimOp (VecUnpackOp WordVec 8 W64) = _ILIT(482)
-- tagOf_PrimOp (VecUnpackOp FloatVec 4 W32) = _ILIT(483)
-- tagOf_PrimOp (VecUnpackOp FloatVec 2 W64) = _ILIT(484)
-- tagOf_PrimOp (VecUnpackOp FloatVec 8 W32) = _ILIT(485)
-- tagOf_PrimOp (VecUnpackOp FloatVec 4 W64) = _ILIT(486)
-- tagOf_PrimOp (VecUnpackOp FloatVec 16 W32) = _ILIT(487)
-- tagOf_PrimOp (VecUnpackOp FloatVec 8 W64) = _ILIT(488)
-- tagOf_PrimOp (VecInsertOp IntVec 16 W8) = _ILIT(489)
-- tagOf_PrimOp (VecInsertOp IntVec 8 W16) = _ILIT(490)
-- tagOf_PrimOp (VecInsertOp IntVec 4 W32) = _ILIT(491)
-- tagOf_PrimOp (VecInsertOp IntVec 2 W64) = _ILIT(492)
-- tagOf_PrimOp (VecInsertOp IntVec 32 W8) = _ILIT(493)
-- tagOf_PrimOp (VecInsertOp IntVec 16 W16) = _ILIT(494)
-- tagOf_PrimOp (VecInsertOp IntVec 8 W32) = _ILIT(495)
-- tagOf_PrimOp (VecInsertOp IntVec 4 W64) = _ILIT(496)
-- tagOf_PrimOp (VecInsertOp IntVec 64 W8) = _ILIT(497)
-- tagOf_PrimOp (VecInsertOp IntVec 32 W16) = _ILIT(498)
-- tagOf_PrimOp (VecInsertOp IntVec 16 W32) = _ILIT(499)
-- tagOf_PrimOp (VecInsertOp IntVec 8 W64) = _ILIT(500)
-- tagOf_PrimOp (VecInsertOp WordVec 16 W8) = _ILIT(501)
-- tagOf_PrimOp (VecInsertOp WordVec 8 W16) = _ILIT(502)
-- tagOf_PrimOp (VecInsertOp WordVec 4 W32) = _ILIT(503)
-- tagOf_PrimOp (VecInsertOp WordVec 2 W64) = _ILIT(504)
-- tagOf_PrimOp (VecInsertOp WordVec 32 W8) = _ILIT(505)
-- tagOf_PrimOp (VecInsertOp WordVec 16 W16) = _ILIT(506)
-- tagOf_PrimOp (VecInsertOp WordVec 8 W32) = _ILIT(507)
-- tagOf_PrimOp (VecInsertOp WordVec 4 W64) = _ILIT(508)
-- tagOf_PrimOp (VecInsertOp WordVec 64 W8) = _ILIT(509)
-- tagOf_PrimOp (VecInsertOp WordVec 32 W16) = _ILIT(510)
-- tagOf_PrimOp (VecInsertOp WordVec 16 W32) = _ILIT(511)
-- tagOf_PrimOp (VecInsertOp WordVec 8 W64) = _ILIT(512)
-- tagOf_PrimOp (VecInsertOp FloatVec 4 W32) = _ILIT(513)
-- tagOf_PrimOp (VecInsertOp FloatVec 2 W64) = _ILIT(514)
-- tagOf_PrimOp (VecInsertOp FloatVec 8 W32) = _ILIT(515)
-- tagOf_PrimOp (VecInsertOp FloatVec 4 W64) = _ILIT(516)
-- tagOf_PrimOp (VecInsertOp FloatVec 16 W32) = _ILIT(517)
-- tagOf_PrimOp (VecInsertOp FloatVec 8 W64) = _ILIT(518)
-- tagOf_PrimOp (VecAddOp IntVec 16 W8) = _ILIT(519)
-- tagOf_PrimOp (VecAddOp IntVec 8 W16) = _ILIT(520)
-- tagOf_PrimOp (VecAddOp IntVec 4 W32) = _ILIT(521)
-- tagOf_PrimOp (VecAddOp IntVec 2 W64) = _ILIT(522)
-- tagOf_PrimOp (VecAddOp IntVec 32 W8) = _ILIT(523)
-- tagOf_PrimOp (VecAddOp IntVec 16 W16) = _ILIT(524)
-- tagOf_PrimOp (VecAddOp IntVec 8 W32) = _ILIT(525)
-- tagOf_PrimOp (VecAddOp IntVec 4 W64) = _ILIT(526)
-- tagOf_PrimOp (VecAddOp IntVec 64 W8) = _ILIT(527)
-- tagOf_PrimOp (VecAddOp IntVec 32 W16) = _ILIT(528)
-- tagOf_PrimOp (VecAddOp IntVec 16 W32) = _ILIT(529)
-- tagOf_PrimOp (VecAddOp IntVec 8 W64) = _ILIT(530)
-- tagOf_PrimOp (VecAddOp WordVec 16 W8) = _ILIT(531)
-- tagOf_PrimOp (VecAddOp WordVec 8 W16) = _ILIT(532)
-- tagOf_PrimOp (VecAddOp WordVec 4 W32) = _ILIT(533)
-- tagOf_PrimOp (VecAddOp WordVec 2 W64) = _ILIT(534)
-- tagOf_PrimOp (VecAddOp WordVec 32 W8) = _ILIT(535)
-- tagOf_PrimOp (VecAddOp WordVec 16 W16) = _ILIT(536)
-- tagOf_PrimOp (VecAddOp WordVec 8 W32) = _ILIT(537)
-- tagOf_PrimOp (VecAddOp WordVec 4 W64) = _ILIT(538)
-- tagOf_PrimOp (VecAddOp WordVec 64 W8) = _ILIT(539)
-- tagOf_PrimOp (VecAddOp WordVec 32 W16) = _ILIT(540)
-- tagOf_PrimOp (VecAddOp WordVec 16 W32) = _ILIT(541)
-- tagOf_PrimOp (VecAddOp WordVec 8 W64) = _ILIT(542)
-- tagOf_PrimOp (VecAddOp FloatVec 4 W32) = _ILIT(543)
-- tagOf_PrimOp (VecAddOp FloatVec 2 W64) = _ILIT(544)
-- tagOf_PrimOp (VecAddOp FloatVec 8 W32) = _ILIT(545)
-- tagOf_PrimOp (VecAddOp FloatVec 4 W64) = _ILIT(546)
-- tagOf_PrimOp (VecAddOp FloatVec 16 W32) = _ILIT(547)
-- tagOf_PrimOp (VecAddOp FloatVec 8 W64) = _ILIT(548)
-- tagOf_PrimOp (VecSubOp IntVec 16 W8) = _ILIT(549)
-- tagOf_PrimOp (VecSubOp IntVec 8 W16) = _ILIT(550)
-- tagOf_PrimOp (VecSubOp IntVec 4 W32) = _ILIT(551)
-- tagOf_PrimOp (VecSubOp IntVec 2 W64) = _ILIT(552)
-- tagOf_PrimOp (VecSubOp IntVec 32 W8) = _ILIT(553)
-- tagOf_PrimOp (VecSubOp IntVec 16 W16) = _ILIT(554)
-- tagOf_PrimOp (VecSubOp IntVec 8 W32) = _ILIT(555)
-- tagOf_PrimOp (VecSubOp IntVec 4 W64) = _ILIT(556)
-- tagOf_PrimOp (VecSubOp IntVec 64 W8) = _ILIT(557)
-- tagOf_PrimOp (VecSubOp IntVec 32 W16) = _ILIT(558)
-- tagOf_PrimOp (VecSubOp IntVec 16 W32) = _ILIT(559)
-- tagOf_PrimOp (VecSubOp IntVec 8 W64) = _ILIT(560)
-- tagOf_PrimOp (VecSubOp WordVec 16 W8) = _ILIT(561)
-- tagOf_PrimOp (VecSubOp WordVec 8 W16) = _ILIT(562)
-- tagOf_PrimOp (VecSubOp WordVec 4 W32) = _ILIT(563)
-- tagOf_PrimOp (VecSubOp WordVec 2 W64) = _ILIT(564)
-- tagOf_PrimOp (VecSubOp WordVec 32 W8) = _ILIT(565)
-- tagOf_PrimOp (VecSubOp WordVec 16 W16) = _ILIT(566)
-- tagOf_PrimOp (VecSubOp WordVec 8 W32) = _ILIT(567)
-- tagOf_PrimOp (VecSubOp WordVec 4 W64) = _ILIT(568)
-- tagOf_PrimOp (VecSubOp WordVec 64 W8) = _ILIT(569)
-- tagOf_PrimOp (VecSubOp WordVec 32 W16) = _ILIT(570)
-- tagOf_PrimOp (VecSubOp WordVec 16 W32) = _ILIT(571)
-- tagOf_PrimOp (VecSubOp WordVec 8 W64) = _ILIT(572)
-- tagOf_PrimOp (VecSubOp FloatVec 4 W32) = _ILIT(573)
-- tagOf_PrimOp (VecSubOp FloatVec 2 W64) = _ILIT(574)
-- tagOf_PrimOp (VecSubOp FloatVec 8 W32) = _ILIT(575)
-- tagOf_PrimOp (VecSubOp FloatVec 4 W64) = _ILIT(576)
-- tagOf_PrimOp (VecSubOp FloatVec 16 W32) = _ILIT(577)
-- tagOf_PrimOp (VecSubOp FloatVec 8 W64) = _ILIT(578)
-- tagOf_PrimOp (VecMulOp IntVec 16 W8) = _ILIT(579)
-- tagOf_PrimOp (VecMulOp IntVec 8 W16) = _ILIT(580)
-- tagOf_PrimOp (VecMulOp IntVec 4 W32) = _ILIT(581)
-- tagOf_PrimOp (VecMulOp IntVec 2 W64) = _ILIT(582)
-- tagOf_PrimOp (VecMulOp IntVec 32 W8) = _ILIT(583)
-- tagOf_PrimOp (VecMulOp IntVec 16 W16) = _ILIT(584)
-- tagOf_PrimOp (VecMulOp IntVec 8 W32) = _ILIT(585)
-- tagOf_PrimOp (VecMulOp IntVec 4 W64) = _ILIT(586)
-- tagOf_PrimOp (VecMulOp IntVec 64 W8) = _ILIT(587)
-- tagOf_PrimOp (VecMulOp IntVec 32 W16) = _ILIT(588)
-- tagOf_PrimOp (VecMulOp IntVec 16 W32) = _ILIT(589)
-- tagOf_PrimOp (VecMulOp IntVec 8 W64) = _ILIT(590)
-- tagOf_PrimOp (VecMulOp WordVec 16 W8) = _ILIT(591)
-- tagOf_PrimOp (VecMulOp WordVec 8 W16) = _ILIT(592)
-- tagOf_PrimOp (VecMulOp WordVec 4 W32) = _ILIT(593)
-- tagOf_PrimOp (VecMulOp WordVec 2 W64) = _ILIT(594)
-- tagOf_PrimOp (VecMulOp WordVec 32 W8) = _ILIT(595)
-- tagOf_PrimOp (VecMulOp WordVec 16 W16) = _ILIT(596)
-- tagOf_PrimOp (VecMulOp WordVec 8 W32) = _ILIT(597)
-- tagOf_PrimOp (VecMulOp WordVec 4 W64) = _ILIT(598)
-- tagOf_PrimOp (VecMulOp WordVec 64 W8) = _ILIT(599)
-- tagOf_PrimOp (VecMulOp WordVec 32 W16) = _ILIT(600)
-- tagOf_PrimOp (VecMulOp WordVec 16 W32) = _ILIT(601)
-- tagOf_PrimOp (VecMulOp WordVec 8 W64) = _ILIT(602)
-- tagOf_PrimOp (VecMulOp FloatVec 4 W32) = _ILIT(603)
-- tagOf_PrimOp (VecMulOp FloatVec 2 W64) = _ILIT(604)
-- tagOf_PrimOp (VecMulOp FloatVec 8 W32) = _ILIT(605)
-- tagOf_PrimOp (VecMulOp FloatVec 4 W64) = _ILIT(606)
-- tagOf_PrimOp (VecMulOp FloatVec 16 W32) = _ILIT(607)
-- tagOf_PrimOp (VecMulOp FloatVec 8 W64) = _ILIT(608)
-- tagOf_PrimOp (VecDivOp FloatVec 4 W32) = _ILIT(609)
-- tagOf_PrimOp (VecDivOp FloatVec 2 W64) = _ILIT(610)
-- tagOf_PrimOp (VecDivOp FloatVec 8 W32) = _ILIT(611)
-- tagOf_PrimOp (VecDivOp FloatVec 4 W64) = _ILIT(612)
-- tagOf_PrimOp (VecDivOp FloatVec 16 W32) = _ILIT(613)
-- tagOf_PrimOp (VecDivOp FloatVec 8 W64) = _ILIT(614)
-- tagOf_PrimOp (VecQuotOp IntVec 16 W8) = _ILIT(615)
-- tagOf_PrimOp (VecQuotOp IntVec 8 W16) = _ILIT(616)
-- tagOf_PrimOp (VecQuotOp IntVec 4 W32) = _ILIT(617)
-- tagOf_PrimOp (VecQuotOp IntVec 2 W64) = _ILIT(618)
-- tagOf_PrimOp (VecQuotOp IntVec 32 W8) = _ILIT(619)
-- tagOf_PrimOp (VecQuotOp IntVec 16 W16) = _ILIT(620)
-- tagOf_PrimOp (VecQuotOp IntVec 8 W32) = _ILIT(621)
-- tagOf_PrimOp (VecQuotOp IntVec 4 W64) = _ILIT(622)
-- tagOf_PrimOp (VecQuotOp IntVec 64 W8) = _ILIT(623)
-- tagOf_PrimOp (VecQuotOp IntVec 32 W16) = _ILIT(624)
-- tagOf_PrimOp (VecQuotOp IntVec 16 W32) = _ILIT(625)
-- tagOf_PrimOp (VecQuotOp IntVec 8 W64) = _ILIT(626)
-- tagOf_PrimOp (VecQuotOp WordVec 16 W8) = _ILIT(627)
-- tagOf_PrimOp (VecQuotOp WordVec 8 W16) = _ILIT(628)
-- tagOf_PrimOp (VecQuotOp WordVec 4 W32) = _ILIT(629)
-- tagOf_PrimOp (VecQuotOp WordVec 2 W64) = _ILIT(630)
-- tagOf_PrimOp (VecQuotOp WordVec 32 W8) = _ILIT(631)
-- tagOf_PrimOp (VecQuotOp WordVec 16 W16) = _ILIT(632)
-- tagOf_PrimOp (VecQuotOp WordVec 8 W32) = _ILIT(633)
-- tagOf_PrimOp (VecQuotOp WordVec 4 W64) = _ILIT(634)
-- tagOf_PrimOp (VecQuotOp WordVec 64 W8) = _ILIT(635)
-- tagOf_PrimOp (VecQuotOp WordVec 32 W16) = _ILIT(636)
-- tagOf_PrimOp (VecQuotOp WordVec 16 W32) = _ILIT(637)
-- tagOf_PrimOp (VecQuotOp WordVec 8 W64) = _ILIT(638)
-- tagOf_PrimOp (VecRemOp IntVec 16 W8) = _ILIT(639)
-- tagOf_PrimOp (VecRemOp IntVec 8 W16) = _ILIT(640)
-- tagOf_PrimOp (VecRemOp IntVec 4 W32) = _ILIT(641)
-- tagOf_PrimOp (VecRemOp IntVec 2 W64) = _ILIT(642)
-- tagOf_PrimOp (VecRemOp IntVec 32 W8) = _ILIT(643)
-- tagOf_PrimOp (VecRemOp IntVec 16 W16) = _ILIT(644)
-- tagOf_PrimOp (VecRemOp IntVec 8 W32) = _ILIT(645)
-- tagOf_PrimOp (VecRemOp IntVec 4 W64) = _ILIT(646)
-- tagOf_PrimOp (VecRemOp IntVec 64 W8) = _ILIT(647)
-- tagOf_PrimOp (VecRemOp IntVec 32 W16) = _ILIT(648)
-- tagOf_PrimOp (VecRemOp IntVec 16 W32) = _ILIT(649)
-- tagOf_PrimOp (VecRemOp IntVec 8 W64) = _ILIT(650)
-- tagOf_PrimOp (VecRemOp WordVec 16 W8) = _ILIT(651)
-- tagOf_PrimOp (VecRemOp WordVec 8 W16) = _ILIT(652)
-- tagOf_PrimOp (VecRemOp WordVec 4 W32) = _ILIT(653)
-- tagOf_PrimOp (VecRemOp WordVec 2 W64) = _ILIT(654)
-- tagOf_PrimOp (VecRemOp WordVec 32 W8) = _ILIT(655)
-- tagOf_PrimOp (VecRemOp WordVec 16 W16) = _ILIT(656)
-- tagOf_PrimOp (VecRemOp WordVec 8 W32) = _ILIT(657)
-- tagOf_PrimOp (VecRemOp WordVec 4 W64) = _ILIT(658)
-- tagOf_PrimOp (VecRemOp WordVec 64 W8) = _ILIT(659)
-- tagOf_PrimOp (VecRemOp WordVec 32 W16) = _ILIT(660)
-- tagOf_PrimOp (VecRemOp WordVec 16 W32) = _ILIT(661)
-- tagOf_PrimOp (VecRemOp WordVec 8 W64) = _ILIT(662)
-- tagOf_PrimOp (VecNegOp IntVec 16 W8) = _ILIT(663)
-- tagOf_PrimOp (VecNegOp IntVec 8 W16) = _ILIT(664)
-- tagOf_PrimOp (VecNegOp IntVec 4 W32) = _ILIT(665)
-- tagOf_PrimOp (VecNegOp IntVec 2 W64) = _ILIT(666)
-- tagOf_PrimOp (VecNegOp IntVec 32 W8) = _ILIT(667)
-- tagOf_PrimOp (VecNegOp IntVec 16 W16) = _ILIT(668)
-- tagOf_PrimOp (VecNegOp IntVec 8 W32) = _ILIT(669)
-- tagOf_PrimOp (VecNegOp IntVec 4 W64) = _ILIT(670)
-- tagOf_PrimOp (VecNegOp IntVec 64 W8) = _ILIT(671)
-- tagOf_PrimOp (VecNegOp IntVec 32 W16) = _ILIT(672)
-- tagOf_PrimOp (VecNegOp IntVec 16 W32) = _ILIT(673)
-- tagOf_PrimOp (VecNegOp IntVec 8 W64) = _ILIT(674)
-- tagOf_PrimOp (VecNegOp FloatVec 4 W32) = _ILIT(675)
-- tagOf_PrimOp (VecNegOp FloatVec 2 W64) = _ILIT(676)
-- tagOf_PrimOp (VecNegOp FloatVec 8 W32) = _ILIT(677)
-- tagOf_PrimOp (VecNegOp FloatVec 4 W64) = _ILIT(678)
-- tagOf_PrimOp (VecNegOp FloatVec 16 W32) = _ILIT(679)
-- tagOf_PrimOp (VecNegOp FloatVec 8 W64) = _ILIT(680)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 16 W8) = _ILIT(681)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 8 W16) = _ILIT(682)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 4 W32) = _ILIT(683)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 2 W64) = _ILIT(684)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 32 W8) = _ILIT(685)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 16 W16) = _ILIT(686)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 8 W32) = _ILIT(687)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 4 W64) = _ILIT(688)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 64 W8) = _ILIT(689)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 32 W16) = _ILIT(690)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 16 W32) = _ILIT(691)
-- tagOf_PrimOp (VecIndexByteArrayOp IntVec 8 W64) = _ILIT(692)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 16 W8) = _ILIT(693)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 8 W16) = _ILIT(694)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 4 W32) = _ILIT(695)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 2 W64) = _ILIT(696)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 32 W8) = _ILIT(697)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 16 W16) = _ILIT(698)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 8 W32) = _ILIT(699)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 4 W64) = _ILIT(700)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 64 W8) = _ILIT(701)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 32 W16) = _ILIT(702)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 16 W32) = _ILIT(703)
-- tagOf_PrimOp (VecIndexByteArrayOp WordVec 8 W64) = _ILIT(704)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 4 W32) = _ILIT(705)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 2 W64) = _ILIT(706)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 8 W32) = _ILIT(707)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 4 W64) = _ILIT(708)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 16 W32) = _ILIT(709)
-- tagOf_PrimOp (VecIndexByteArrayOp FloatVec 8 W64) = _ILIT(710)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 16 W8) = _ILIT(711)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 8 W16) = _ILIT(712)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 4 W32) = _ILIT(713)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 2 W64) = _ILIT(714)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 32 W8) = _ILIT(715)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 16 W16) = _ILIT(716)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 8 W32) = _ILIT(717)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 4 W64) = _ILIT(718)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 64 W8) = _ILIT(719)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 32 W16) = _ILIT(720)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 16 W32) = _ILIT(721)
-- tagOf_PrimOp (VecReadByteArrayOp IntVec 8 W64) = _ILIT(722)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 16 W8) = _ILIT(723)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 8 W16) = _ILIT(724)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 4 W32) = _ILIT(725)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 2 W64) = _ILIT(726)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 32 W8) = _ILIT(727)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 16 W16) = _ILIT(728)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 8 W32) = _ILIT(729)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 4 W64) = _ILIT(730)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 64 W8) = _ILIT(731)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 32 W16) = _ILIT(732)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 16 W32) = _ILIT(733)
-- tagOf_PrimOp (VecReadByteArrayOp WordVec 8 W64) = _ILIT(734)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 4 W32) = _ILIT(735)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 2 W64) = _ILIT(736)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 8 W32) = _ILIT(737)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 4 W64) = _ILIT(738)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 16 W32) = _ILIT(739)
-- tagOf_PrimOp (VecReadByteArrayOp FloatVec 8 W64) = _ILIT(740)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 16 W8) = _ILIT(741)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 8 W16) = _ILIT(742)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 4 W32) = _ILIT(743)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 2 W64) = _ILIT(744)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 32 W8) = _ILIT(745)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 16 W16) = _ILIT(746)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 8 W32) = _ILIT(747)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 4 W64) = _ILIT(748)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 64 W8) = _ILIT(749)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 32 W16) = _ILIT(750)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 16 W32) = _ILIT(751)
-- tagOf_PrimOp (VecWriteByteArrayOp IntVec 8 W64) = _ILIT(752)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 16 W8) = _ILIT(753)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 8 W16) = _ILIT(754)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 4 W32) = _ILIT(755)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 2 W64) = _ILIT(756)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 32 W8) = _ILIT(757)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 16 W16) = _ILIT(758)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 8 W32) = _ILIT(759)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 4 W64) = _ILIT(760)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 64 W8) = _ILIT(761)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 32 W16) = _ILIT(762)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 16 W32) = _ILIT(763)
-- tagOf_PrimOp (VecWriteByteArrayOp WordVec 8 W64) = _ILIT(764)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 4 W32) = _ILIT(765)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 2 W64) = _ILIT(766)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 8 W32) = _ILIT(767)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 4 W64) = _ILIT(768)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 16 W32) = _ILIT(769)
-- tagOf_PrimOp (VecWriteByteArrayOp FloatVec 8 W64) = _ILIT(770)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 16 W8) = _ILIT(771)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 8 W16) = _ILIT(772)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 4 W32) = _ILIT(773)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 2 W64) = _ILIT(774)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 32 W8) = _ILIT(775)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 16 W16) = _ILIT(776)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 8 W32) = _ILIT(777)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 4 W64) = _ILIT(778)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 64 W8) = _ILIT(779)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 32 W16) = _ILIT(780)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 16 W32) = _ILIT(781)
-- tagOf_PrimOp (VecIndexOffAddrOp IntVec 8 W64) = _ILIT(782)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 16 W8) = _ILIT(783)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 8 W16) = _ILIT(784)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 4 W32) = _ILIT(785)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 2 W64) = _ILIT(786)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 32 W8) = _ILIT(787)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 16 W16) = _ILIT(788)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 8 W32) = _ILIT(789)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 4 W64) = _ILIT(790)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 64 W8) = _ILIT(791)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 32 W16) = _ILIT(792)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 16 W32) = _ILIT(793)
-- tagOf_PrimOp (VecIndexOffAddrOp WordVec 8 W64) = _ILIT(794)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 4 W32) = _ILIT(795)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 2 W64) = _ILIT(796)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 8 W32) = _ILIT(797)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 4 W64) = _ILIT(798)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 16 W32) = _ILIT(799)
-- tagOf_PrimOp (VecIndexOffAddrOp FloatVec 8 W64) = _ILIT(800)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 16 W8) = _ILIT(801)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 8 W16) = _ILIT(802)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 4 W32) = _ILIT(803)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 2 W64) = _ILIT(804)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 32 W8) = _ILIT(805)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 16 W16) = _ILIT(806)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 8 W32) = _ILIT(807)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 4 W64) = _ILIT(808)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 64 W8) = _ILIT(809)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 32 W16) = _ILIT(810)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 16 W32) = _ILIT(811)
-- tagOf_PrimOp (VecReadOffAddrOp IntVec 8 W64) = _ILIT(812)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 16 W8) = _ILIT(813)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 8 W16) = _ILIT(814)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 4 W32) = _ILIT(815)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 2 W64) = _ILIT(816)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 32 W8) = _ILIT(817)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 16 W16) = _ILIT(818)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 8 W32) = _ILIT(819)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 4 W64) = _ILIT(820)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 64 W8) = _ILIT(821)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 32 W16) = _ILIT(822)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 16 W32) = _ILIT(823)
-- tagOf_PrimOp (VecReadOffAddrOp WordVec 8 W64) = _ILIT(824)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 4 W32) = _ILIT(825)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 2 W64) = _ILIT(826)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 8 W32) = _ILIT(827)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 4 W64) = _ILIT(828)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 16 W32) = _ILIT(829)
-- tagOf_PrimOp (VecReadOffAddrOp FloatVec 8 W64) = _ILIT(830)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 16 W8) = _ILIT(831)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 8 W16) = _ILIT(832)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 4 W32) = _ILIT(833)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 2 W64) = _ILIT(834)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 32 W8) = _ILIT(835)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 16 W16) = _ILIT(836)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 8 W32) = _ILIT(837)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 4 W64) = _ILIT(838)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 64 W8) = _ILIT(839)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 32 W16) = _ILIT(840)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 16 W32) = _ILIT(841)
-- tagOf_PrimOp (VecWriteOffAddrOp IntVec 8 W64) = _ILIT(842)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 16 W8) = _ILIT(843)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 8 W16) = _ILIT(844)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 4 W32) = _ILIT(845)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 2 W64) = _ILIT(846)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 32 W8) = _ILIT(847)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 16 W16) = _ILIT(848)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 8 W32) = _ILIT(849)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 4 W64) = _ILIT(850)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 64 W8) = _ILIT(851)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 32 W16) = _ILIT(852)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 16 W32) = _ILIT(853)
-- tagOf_PrimOp (VecWriteOffAddrOp WordVec 8 W64) = _ILIT(854)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 4 W32) = _ILIT(855)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 2 W64) = _ILIT(856)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 8 W32) = _ILIT(857)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 4 W64) = _ILIT(858)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 16 W32) = _ILIT(859)
-- tagOf_PrimOp (VecWriteOffAddrOp FloatVec 8 W64) = _ILIT(860)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 16 W8) = _ILIT(861)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 8 W16) = _ILIT(862)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 4 W32) = _ILIT(863)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 2 W64) = _ILIT(864)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 32 W8) = _ILIT(865)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 16 W16) = _ILIT(866)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 8 W32) = _ILIT(867)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 4 W64) = _ILIT(868)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 64 W8) = _ILIT(869)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 32 W16) = _ILIT(870)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 16 W32) = _ILIT(871)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp IntVec 8 W64) = _ILIT(872)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 16 W8) = _ILIT(873)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 8 W16) = _ILIT(874)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 4 W32) = _ILIT(875)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 2 W64) = _ILIT(876)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 32 W8) = _ILIT(877)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 16 W16) = _ILIT(878)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 8 W32) = _ILIT(879)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 4 W64) = _ILIT(880)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 64 W8) = _ILIT(881)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 32 W16) = _ILIT(882)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 16 W32) = _ILIT(883)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp WordVec 8 W64) = _ILIT(884)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 4 W32) = _ILIT(885)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 2 W64) = _ILIT(886)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 8 W32) = _ILIT(887)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 4 W64) = _ILIT(888)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 16 W32) = _ILIT(889)
-- tagOf_PrimOp (VecIndexScalarByteArrayOp FloatVec 8 W64) = _ILIT(890)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 16 W8) = _ILIT(891)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 8 W16) = _ILIT(892)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 4 W32) = _ILIT(893)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 2 W64) = _ILIT(894)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 32 W8) = _ILIT(895)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 16 W16) = _ILIT(896)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 8 W32) = _ILIT(897)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 4 W64) = _ILIT(898)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 64 W8) = _ILIT(899)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 32 W16) = _ILIT(900)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 16 W32) = _ILIT(901)
-- tagOf_PrimOp (VecReadScalarByteArrayOp IntVec 8 W64) = _ILIT(902)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 16 W8) = _ILIT(903)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 8 W16) = _ILIT(904)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 4 W32) = _ILIT(905)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 2 W64) = _ILIT(906)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 32 W8) = _ILIT(907)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 16 W16) = _ILIT(908)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 8 W32) = _ILIT(909)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 4 W64) = _ILIT(910)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 64 W8) = _ILIT(911)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 32 W16) = _ILIT(912)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 16 W32) = _ILIT(913)
-- tagOf_PrimOp (VecReadScalarByteArrayOp WordVec 8 W64) = _ILIT(914)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 4 W32) = _ILIT(915)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 2 W64) = _ILIT(916)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 8 W32) = _ILIT(917)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 4 W64) = _ILIT(918)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 16 W32) = _ILIT(919)
-- tagOf_PrimOp (VecReadScalarByteArrayOp FloatVec 8 W64) = _ILIT(920)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 16 W8) = _ILIT(921)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 8 W16) = _ILIT(922)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 4 W32) = _ILIT(923)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 2 W64) = _ILIT(924)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 32 W8) = _ILIT(925)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 16 W16) = _ILIT(926)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 8 W32) = _ILIT(927)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 4 W64) = _ILIT(928)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 64 W8) = _ILIT(929)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 32 W16) = _ILIT(930)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 16 W32) = _ILIT(931)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp IntVec 8 W64) = _ILIT(932)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 16 W8) = _ILIT(933)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 8 W16) = _ILIT(934)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 4 W32) = _ILIT(935)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 2 W64) = _ILIT(936)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 32 W8) = _ILIT(937)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 16 W16) = _ILIT(938)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 8 W32) = _ILIT(939)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 4 W64) = _ILIT(940)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 64 W8) = _ILIT(941)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 32 W16) = _ILIT(942)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 16 W32) = _ILIT(943)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp WordVec 8 W64) = _ILIT(944)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 4 W32) = _ILIT(945)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 2 W64) = _ILIT(946)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 8 W32) = _ILIT(947)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 4 W64) = _ILIT(948)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 16 W32) = _ILIT(949)
-- tagOf_PrimOp (VecWriteScalarByteArrayOp FloatVec 8 W64) = _ILIT(950)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 16 W8) = _ILIT(951)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 8 W16) = _ILIT(952)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 4 W32) = _ILIT(953)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 2 W64) = _ILIT(954)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 32 W8) = _ILIT(955)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 16 W16) = _ILIT(956)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 8 W32) = _ILIT(957)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 4 W64) = _ILIT(958)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 64 W8) = _ILIT(959)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 32 W16) = _ILIT(960)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 16 W32) = _ILIT(961)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp IntVec 8 W64) = _ILIT(962)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 16 W8) = _ILIT(963)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 8 W16) = _ILIT(964)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 4 W32) = _ILIT(965)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 2 W64) = _ILIT(966)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 32 W8) = _ILIT(967)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 16 W16) = _ILIT(968)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 8 W32) = _ILIT(969)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 4 W64) = _ILIT(970)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 64 W8) = _ILIT(971)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 32 W16) = _ILIT(972)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 16 W32) = _ILIT(973)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp WordVec 8 W64) = _ILIT(974)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 4 W32) = _ILIT(975)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 2 W64) = _ILIT(976)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 8 W32) = _ILIT(977)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 4 W64) = _ILIT(978)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 16 W32) = _ILIT(979)
-- tagOf_PrimOp (VecIndexScalarOffAddrOp FloatVec 8 W64) = _ILIT(980)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 16 W8) = _ILIT(981)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 8 W16) = _ILIT(982)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 4 W32) = _ILIT(983)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 2 W64) = _ILIT(984)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 32 W8) = _ILIT(985)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 16 W16) = _ILIT(986)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 8 W32) = _ILIT(987)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 4 W64) = _ILIT(988)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 64 W8) = _ILIT(989)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 32 W16) = _ILIT(990)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 16 W32) = _ILIT(991)
-- tagOf_PrimOp (VecReadScalarOffAddrOp IntVec 8 W64) = _ILIT(992)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 16 W8) = _ILIT(993)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 8 W16) = _ILIT(994)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 4 W32) = _ILIT(995)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 2 W64) = _ILIT(996)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 32 W8) = _ILIT(997)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 16 W16) = _ILIT(998)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 8 W32) = _ILIT(999)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 4 W64) = _ILIT(1000)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 64 W8) = _ILIT(1001)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 32 W16) = _ILIT(1002)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 16 W32) = _ILIT(1003)
-- tagOf_PrimOp (VecReadScalarOffAddrOp WordVec 8 W64) = _ILIT(1004)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 4 W32) = _ILIT(1005)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 2 W64) = _ILIT(1006)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 8 W32) = _ILIT(1007)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 4 W64) = _ILIT(1008)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 16 W32) = _ILIT(1009)
-- tagOf_PrimOp (VecReadScalarOffAddrOp FloatVec 8 W64) = _ILIT(1010)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 16 W8) = _ILIT(1011)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 8 W16) = _ILIT(1012)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 4 W32) = _ILIT(1013)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 2 W64) = _ILIT(1014)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 32 W8) = _ILIT(1015)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 16 W16) = _ILIT(1016)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 8 W32) = _ILIT(1017)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 4 W64) = _ILIT(1018)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 64 W8) = _ILIT(1019)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 32 W16) = _ILIT(1020)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 16 W32) = _ILIT(1021)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp IntVec 8 W64) = _ILIT(1022)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 16 W8) = _ILIT(1023)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 8 W16) = _ILIT(1024)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 4 W32) = _ILIT(1025)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 2 W64) = _ILIT(1026)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 32 W8) = _ILIT(1027)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 16 W16) = _ILIT(1028)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 8 W32) = _ILIT(1029)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 4 W64) = _ILIT(1030)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 64 W8) = _ILIT(1031)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 32 W16) = _ILIT(1032)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 16 W32) = _ILIT(1033)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp WordVec 8 W64) = _ILIT(1034)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 4 W32) = _ILIT(1035)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 2 W64) = _ILIT(1036)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 8 W32) = _ILIT(1037)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 4 W64) = _ILIT(1038)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 16 W32) = _ILIT(1039)
-- tagOf_PrimOp (VecWriteScalarOffAddrOp FloatVec 8 W64) = _ILIT(1040)
tagOf_PrimOp PrefetchByteArrayOp3 = _ILIT(1041)
tagOf_PrimOp PrefetchMutableByteArrayOp3 = _ILIT(1042)
tagOf_PrimOp PrefetchAddrOp3 = _ILIT(1043)
tagOf_PrimOp PrefetchValueOp3 = _ILIT(1044)
tagOf_PrimOp PrefetchByteArrayOp2 = _ILIT(1045)
tagOf_PrimOp PrefetchMutableByteArrayOp2 = _ILIT(1046)
tagOf_PrimOp PrefetchAddrOp2 = _ILIT(1047)
tagOf_PrimOp PrefetchValueOp2 = _ILIT(1048)
tagOf_PrimOp PrefetchByteArrayOp1 = _ILIT(1049)
tagOf_PrimOp PrefetchMutableByteArrayOp1 = _ILIT(1050)
tagOf_PrimOp PrefetchAddrOp1 = _ILIT(1051)
tagOf_PrimOp PrefetchValueOp1 = _ILIT(1052)
tagOf_PrimOp PrefetchByteArrayOp0 = _ILIT(1053)
tagOf_PrimOp PrefetchMutableByteArrayOp0 = _ILIT(1054)
tagOf_PrimOp PrefetchAddrOp0 = _ILIT(1055)
tagOf_PrimOp PrefetchValueOp0 = _ILIT(1056)
--ETA-specific
tagOf_PrimOp Word64Eq = _ILIT(1057)
tagOf_PrimOp Word64Ne = _ILIT(1058)
tagOf_PrimOp Word64Lt = _ILIT(1059)
tagOf_PrimOp Word64Le = _ILIT(1060)
tagOf_PrimOp Word64Gt = _ILIT(1061)
tagOf_PrimOp Word64Ge = _ILIT(1062)
tagOf_PrimOp Word64Quot = _ILIT(1063)
tagOf_PrimOp Word64Rem = _ILIT(1064)
tagOf_PrimOp Word64And = _ILIT(1065)
tagOf_PrimOp Word64Or = _ILIT(1066)
tagOf_PrimOp Word64Xor = _ILIT(1067)
tagOf_PrimOp Word64Not = _ILIT(1068)
tagOf_PrimOp Word64SllOp = _ILIT(1069)
tagOf_PrimOp Word64SrlOp = _ILIT(1070)
tagOf_PrimOp Int64Eq = _ILIT(1071)
tagOf_PrimOp Int64Ne = _ILIT(1072)
tagOf_PrimOp Int64Lt = _ILIT(1073)
tagOf_PrimOp Int64Le = _ILIT(1074)
tagOf_PrimOp Int64Gt = _ILIT(1075)
tagOf_PrimOp Int64Ge = _ILIT(1076)
tagOf_PrimOp Int64Quot = _ILIT(1077)
tagOf_PrimOp Int64Rem = _ILIT(1078)
tagOf_PrimOp Int64Add = _ILIT(1079)
tagOf_PrimOp Int64Sub = _ILIT(1080)
tagOf_PrimOp Int64Mul = _ILIT(1081)
tagOf_PrimOp Int64Neg = _ILIT(1082)
tagOf_PrimOp Int64SllOp = _ILIT(1083)
tagOf_PrimOp Int64SraOp = _ILIT(1084)
tagOf_PrimOp Int64SrlOp = _ILIT(1085)
tagOf_PrimOp Int642Word64 = _ILIT(1086)
tagOf_PrimOp Word642Int64 = _ILIT(1087)
tagOf_PrimOp Int2Int64 = _ILIT(1088)
tagOf_PrimOp Int642Int = _ILIT(1089)
tagOf_PrimOp Word2Word64 = _ILIT(1090)
tagOf_PrimOp Word64ToWord = _ILIT(1091)
tagOf_PrimOp DecodeDoubleInteger = _ILIT(1092)
tagOf_PrimOp ObjectArrayAtOp = _ILIT(1093)
tagOf_PrimOp ObjectArraySetOp = _ILIT(1094)
tagOf_PrimOp IndexJByteArrayOp = _ILIT(1095)
tagOf_PrimOp ReadJByteArrayOp = _ILIT(1096)
tagOf_PrimOp WriteJByteArrayOp = _ILIT(1097)
tagOf_PrimOp JByte2CharOp = _ILIT(1098)
tagOf_PrimOp JBool2IntOp = _ILIT(1099)
tagOf_PrimOp StablePtr2AddrOp = _ILIT(1100)
tagOf_PrimOp Addr2StablePtrOp = _ILIT(1101)
tagOf_PrimOp JByte2IntOp = _ILIT(1102)
tagOf_PrimOp Int2JBoolOp = _ILIT(1103)
tagOf_PrimOp ClassCastOp = _ILIT(1104)
tagOf_PrimOp ObjectArrayNewOp = _ILIT(1105)
tagOf_PrimOp ArrayLengthOp = _ILIT(1106)
-- 1107 is FREE
tagOf_PrimOp IsNullObjectOp = _ILIT(1108)
tagOf_PrimOp GetSizeofMutableByteArrayOp = _ILIT(1109)
tagOf_PrimOp Int2JByteOp = _ILIT(1110)
tagOf_PrimOp JShort2IntOp = _ILIT(1111)
tagOf_PrimOp Int2JShortOp = _ILIT(1112)
tagOf_PrimOp JChar2WordOp = _ILIT(1113)
tagOf_PrimOp Word2JCharOp = _ILIT(1114)
tagOf_PrimOp NewJByteArrayOp = _ILIT(1115)
tagOf_PrimOp NewJBooleanArrayOp = _ILIT(1116)
tagOf_PrimOp ReadJBooleanArrayOp = _ILIT(1117)
tagOf_PrimOp WriteJBooleanArrayOp = _ILIT(1118)
tagOf_PrimOp NewJCharArrayOp = _ILIT(1119)
tagOf_PrimOp ReadJCharArrayOp = _ILIT(1120)
tagOf_PrimOp WriteJCharArrayOp = _ILIT(1121)
tagOf_PrimOp NewJShortArrayOp = _ILIT(1122)
tagOf_PrimOp ReadJShortArrayOp = _ILIT(1123)
tagOf_PrimOp WriteJShortArrayOp = _ILIT(1124)
tagOf_PrimOp NewJIntArrayOp = _ILIT(1125)
tagOf_PrimOp ReadJIntArrayOp = _ILIT(1126)
tagOf_PrimOp WriteJIntArrayOp = _ILIT(1127)
tagOf_PrimOp NewJLongArrayOp = _ILIT(1128)
tagOf_PrimOp ReadJLongArrayOp = _ILIT(1129)
tagOf_PrimOp WriteJLongArrayOp = _ILIT(1130)
tagOf_PrimOp NewJFloatArrayOp = _ILIT(1131)
tagOf_PrimOp ReadJFloatArrayOp = _ILIT(1132)
tagOf_PrimOp WriteJFloatArrayOp = _ILIT(1133)
tagOf_PrimOp NewJDoubleArrayOp = _ILIT(1134)
tagOf_PrimOp ReadJDoubleArrayOp = _ILIT(1135)
tagOf_PrimOp WriteJDoubleArrayOp = _ILIT(1136)
tagOf_PrimOp Addr2Int64Op = _ILIT(1137)
tagOf_PrimOp Int642AddrOp = _ILIT(1138)
tagOf_PrimOp WaitConnectOp = _ILIT(1139)
tagOf_PrimOp WaitAcceptOp = _ILIT(1140)
tagOf_PrimOp FreshStateTokenOp = _ILIT(1141)
tagOf_PrimOp FreshObjectTokenOp = _ILIT(1142)
tagOf_PrimOp FreshNullObjectTokenOp = _ILIT(1143)
tagOf_PrimOp FloatFabsOp = _ILIT(1144)
tagOf_PrimOp DoubleFabsOp = _ILIT(1145)

instance Eq PrimOp where
    op1 == op2 = tagOf_PrimOp op1 ==# tagOf_PrimOp op2

instance Ord PrimOp where
    op1 <  op2 =  tagOf_PrimOp op1 <# tagOf_PrimOp op2
    op1 <= op2 =  tagOf_PrimOp op1 <=# tagOf_PrimOp op2
    op1 >= op2 =  tagOf_PrimOp op1 >=# tagOf_PrimOp op2
    op1 >  op2 =  tagOf_PrimOp op1 ># tagOf_PrimOp op2
    op1 `compare` op2 | op1 < op2  = LT
                      | op1 == op2 = EQ
                      | otherwise  = GT

instance Outputable PrimOp where
    ppr op = pprPrimOp op

data PrimOpVecCat = IntVec
                  | WordVec
                  | FloatVec

-- An @Enum@-derived list would be better; meanwhile... (ToDo)

allThePrimOps :: [PrimOp]
allThePrimOps =
   [ CharGtOp
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
   , GetSizeofMutableByteArrayOp
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
   , ParGlobalOp
   , ParLocalOp
   , ParAtOp
   , ParAtAbsOp
   , ParAtRelOp
   , ParAtForNowOp
   , DataToTagOp
   , TagToEnumOp
   , AddrToAnyOp
   , MkApUpd0_Op
   , NewBCOOp
   , UnpackClosureOp
   , GetApStackValOp
   , GetCCSOfOp
   , GetCurrentCCSOp
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
   -- ETA-Specific
   , Word64Eq
   , Word64Ne
   , Word64Lt
   , Word64Le
   , Word64Gt
   , Word64Ge
   , Word64Quot
   , Word64Rem
   , Word64And
   , Word64Or
   , Word64Xor
   , Word64Not
   , Word64SllOp
   , Word64SrlOp
   , Int64Eq
   , Int64Ne
   , Int64Lt
   , Int64Le
   , Int64Gt
   , Int64Ge
   , Int64Quot
   , Int64Rem
   , Int64Add
   , Int64Sub
   , Int64Mul
   , Int64Neg
   , Int64SllOp
   , Int64SraOp
   , Int64SrlOp
   , Int642Word64
   , Word642Int64
   , Int2Int64
   , Int642Int
   , Word2Word64
   , Word64ToWord
   , DecodeDoubleInteger
   , ObjectArrayAtOp
   , ObjectArraySetOp
   , IndexJByteArrayOp
   , ReadJByteArrayOp
   , WriteJByteArrayOp
   , JByte2CharOp
   , JBool2IntOp
   , StablePtr2AddrOp
   , Addr2StablePtrOp
   , JByte2IntOp
   , Int2JBoolOp
   , ClassCastOp
   , ObjectArrayNewOp
   , ArrayLengthOp
   , IsNullObjectOp
   , Int2JByteOp
   , JShort2IntOp
   , Int2JShortOp
   , JChar2WordOp
   , Word2JCharOp
   , NewJByteArrayOp
   , NewJBooleanArrayOp
   , ReadJBooleanArrayOp
   , WriteJBooleanArrayOp
   , NewJCharArrayOp
   , ReadJCharArrayOp
   , WriteJCharArrayOp
   , NewJShortArrayOp
   , ReadJShortArrayOp
   , WriteJShortArrayOp
   , NewJIntArrayOp
   , ReadJIntArrayOp
   , WriteJIntArrayOp
   , NewJLongArrayOp
   , ReadJLongArrayOp
   , WriteJLongArrayOp
   , NewJFloatArrayOp
   , ReadJFloatArrayOp
   , WriteJFloatArrayOp
   , NewJDoubleArrayOp
   , ReadJDoubleArrayOp
   , WriteJDoubleArrayOp
   , Addr2Int64Op
   , Int642AddrOp
   , WaitConnectOp
   , WaitAcceptOp
   , FreshStateTokenOp
   , FreshObjectTokenOp
   , FreshNullObjectTokenOp
   , FloatFabsOp
   , DoubleFabsOp
   ]

tagToEnumKey :: Unique
tagToEnumKey = mkPrimOpIdUnique (primOpTag TagToEnumOp)

{-
************************************************************************
*                                                                      *
\subsection[PrimOp-info]{The essential info about each @PrimOp@}
*                                                                      *
************************************************************************

The @String@ in the @PrimOpInfos@ is the ``base name'' by which the user may
refer to the primitive operation.  The conventional \tr{#}-for-
unboxed ops is added on later.

The reason for the funny characters in the names is so we do not
interfere with the programmer's Haskell name spaces.

We use @PrimKinds@ for the ``type'' information, because they're
(slightly) more convenient to use than @TyCons@.
-}

data PrimOpInfo
  = Dyadic      OccName         -- string :: T -> T -> T
                Type
  | Monadic     OccName         -- string :: T -> T
                Type
  | Compare     OccName         -- string :: T -> T -> Int#
                Type
  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
                [TyVar]
                [Type]
                Type

mkDyadic, mkMonadic, mkCompare :: FastString -> Type -> PrimOpInfo
mkDyadic str  ty = Dyadic  (mkVarOccFS str) ty
mkMonadic str ty = Monadic (mkVarOccFS str) ty
mkCompare str ty = Compare (mkVarOccFS str) ty

mkGenPrimOp :: FastString -> [TyVar] -> [Type] -> Type -> PrimOpInfo
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty

{-
************************************************************************
*                                                                      *
\subsubsection{Strictness}
*                                                                      *
************************************************************************

Not all primops are strict!
-}

primOpStrictness :: PrimOp -> Arity -> StrictSig
        -- See Demand.StrictnessInfo for discussion of what the results
        -- The arity should be the arity of the primop; that's why
        -- this function isn't exported.
primOpStrictness CatchOp =  \ _arity -> mkClosedStrictSig [lazyApply1Dmd,lazyApply2Dmd,topDmd] topRes
primOpStrictness RaiseOp =  \ _arity -> mkClosedStrictSig [topDmd] botRes
primOpStrictness RaiseIOOp =  \ _arity -> mkClosedStrictSig [topDmd, topDmd] botRes
primOpStrictness MaskAsyncExceptionsOp =  \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topRes
primOpStrictness MaskUninterruptibleOp =  \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topRes
primOpStrictness UnmaskAsyncExceptionsOp =  \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topRes
primOpStrictness AtomicallyOp =  \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topRes
primOpStrictness RetryOp =  \ _arity -> mkClosedStrictSig [topDmd] botRes
primOpStrictness CatchRetryOp =  \ _arity -> mkClosedStrictSig [catchArgDmd,lazyApply1Dmd,topDmd] topRes
primOpStrictness CatchSTMOp =  \ _arity -> mkClosedStrictSig [lazyApply1Dmd,lazyApply2Dmd,topDmd] topRes
primOpStrictness DataToTagOp =  \ _arity -> mkClosedStrictSig [evalDmd] topRes
primOpStrictness PrefetchValueOp3 =  \ _arity -> mkClosedStrictSig [botDmd, topDmd] topRes
primOpStrictness PrefetchValueOp2 =  \ _arity -> mkClosedStrictSig [botDmd, topDmd] topRes
primOpStrictness PrefetchValueOp1 =  \ _arity -> mkClosedStrictSig [botDmd, topDmd] topRes
primOpStrictness PrefetchValueOp0 =  \ _arity -> mkClosedStrictSig [botDmd, topDmd] topRes
primOpStrictness FreshStateTokenOp =  \ _arity -> mkClosedStrictSig [seqDmd] topRes
primOpStrictness FreshObjectTokenOp =  \ _arity -> mkClosedStrictSig [seqDmd, seqDmd] topRes
primOpStrictness FreshNullObjectTokenOp =  \ _arity -> mkClosedStrictSig [seqDmd] topRes
primOpStrictness _ =  \ arity -> mkClosedStrictSig (replicate arity topDmd) topRes

{-
************************************************************************
*                                                                      *
\subsubsection{Fixity}
*                                                                      *
************************************************************************
-}

primOpFixity :: PrimOp -> Maybe Fixity
primOpFixity IntAddOp = Just (Fixity 6 InfixL)
primOpFixity IntSubOp = Just (Fixity 6 InfixL)
primOpFixity IntMulOp = Just (Fixity 7 InfixL)
primOpFixity IntGtOp = Just (Fixity 4 InfixN)
primOpFixity IntGeOp = Just (Fixity 4 InfixN)
primOpFixity IntEqOp = Just (Fixity 4 InfixN)
primOpFixity IntNeOp = Just (Fixity 4 InfixN)
primOpFixity IntLtOp = Just (Fixity 4 InfixN)
primOpFixity IntLeOp = Just (Fixity 4 InfixN)
primOpFixity DoubleGtOp = Just (Fixity 4 InfixN)
primOpFixity DoubleGeOp = Just (Fixity 4 InfixN)
primOpFixity DoubleEqOp = Just (Fixity 4 InfixN)
primOpFixity DoubleNeOp = Just (Fixity 4 InfixN)
primOpFixity DoubleLtOp = Just (Fixity 4 InfixN)
primOpFixity DoubleLeOp = Just (Fixity 4 InfixN)
primOpFixity DoubleAddOp = Just (Fixity 6 InfixL)
primOpFixity DoubleSubOp = Just (Fixity 6 InfixL)
primOpFixity DoubleMulOp = Just (Fixity 7 InfixL)
primOpFixity DoubleDivOp = Just (Fixity 7 InfixL)
primOpFixity _ = Nothing

{-
************************************************************************
*                                                                      *
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
*                                                                      *
************************************************************************

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.
-}

primOpInfo :: PrimOp -> PrimOpInfo
primOpInfo CharGtOp = mkCompare (fsLit "gtChar#") charPrimTy
primOpInfo CharGeOp = mkCompare (fsLit "geChar#") charPrimTy
primOpInfo CharEqOp = mkCompare (fsLit "eqChar#") charPrimTy
primOpInfo CharNeOp = mkCompare (fsLit "neChar#") charPrimTy
primOpInfo CharLtOp = mkCompare (fsLit "ltChar#") charPrimTy
primOpInfo CharLeOp = mkCompare (fsLit "leChar#") charPrimTy
primOpInfo OrdOp = mkGenPrimOp (fsLit "ord#")  [] [charPrimTy] (intPrimTy)
primOpInfo IntAddOp = mkDyadic (fsLit "+#") intPrimTy
primOpInfo IntSubOp = mkDyadic (fsLit "-#") intPrimTy
primOpInfo IntMulOp = mkDyadic (fsLit "*#") intPrimTy
primOpInfo IntMulMayOfloOp = mkDyadic (fsLit "mulIntMayOflo#") intPrimTy
primOpInfo IntQuotOp = mkDyadic (fsLit "quotInt#") intPrimTy
primOpInfo IntRemOp = mkDyadic (fsLit "remInt#") intPrimTy
primOpInfo IntQuotRemOp = mkGenPrimOp (fsLit "quotRemInt#")  [] [intPrimTy, intPrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy]))
primOpInfo AndIOp = mkDyadic (fsLit "andI#") intPrimTy
primOpInfo OrIOp = mkDyadic (fsLit "orI#") intPrimTy
primOpInfo XorIOp = mkDyadic (fsLit "xorI#") intPrimTy
primOpInfo NotIOp = mkMonadic (fsLit "notI#") intPrimTy
primOpInfo IntNegOp = mkMonadic (fsLit "negateInt#") intPrimTy
primOpInfo IntAddCOp = mkGenPrimOp (fsLit "addIntC#")  [] [intPrimTy, intPrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy]))
primOpInfo IntSubCOp = mkGenPrimOp (fsLit "subIntC#")  [] [intPrimTy, intPrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy]))
primOpInfo IntGtOp = mkCompare (fsLit ">#") intPrimTy
primOpInfo IntGeOp = mkCompare (fsLit ">=#") intPrimTy
primOpInfo IntEqOp = mkCompare (fsLit "==#") intPrimTy
primOpInfo IntNeOp = mkCompare (fsLit "/=#") intPrimTy
primOpInfo IntLtOp = mkCompare (fsLit "<#") intPrimTy
primOpInfo IntLeOp = mkCompare (fsLit "<=#") intPrimTy
primOpInfo ChrOp = mkGenPrimOp (fsLit "chr#")  [] [intPrimTy] (charPrimTy)
primOpInfo Int2WordOp = mkGenPrimOp (fsLit "int2Word#")  [] [intPrimTy] (wordPrimTy)
primOpInfo Int2FloatOp = mkGenPrimOp (fsLit "int2Float#")  [] [intPrimTy] (floatPrimTy)
primOpInfo Int2DoubleOp = mkGenPrimOp (fsLit "int2Double#")  [] [intPrimTy] (doublePrimTy)
primOpInfo Word2FloatOp = mkGenPrimOp (fsLit "word2Float#")  [] [wordPrimTy] (floatPrimTy)
primOpInfo Word2DoubleOp = mkGenPrimOp (fsLit "word2Double#")  [] [wordPrimTy] (doublePrimTy)
primOpInfo ISllOp = mkGenPrimOp (fsLit "uncheckedIShiftL#")  [] [intPrimTy, intPrimTy] (intPrimTy)
primOpInfo ISraOp = mkGenPrimOp (fsLit "uncheckedIShiftRA#")  [] [intPrimTy, intPrimTy] (intPrimTy)
primOpInfo ISrlOp = mkGenPrimOp (fsLit "uncheckedIShiftRL#")  [] [intPrimTy, intPrimTy] (intPrimTy)
primOpInfo WordAddOp = mkDyadic (fsLit "plusWord#") wordPrimTy
primOpInfo WordAdd2Op = mkGenPrimOp (fsLit "plusWord2#")  [] [wordPrimTy, wordPrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy]))
primOpInfo WordSubOp = mkDyadic (fsLit "minusWord#") wordPrimTy
primOpInfo WordMulOp = mkDyadic (fsLit "timesWord#") wordPrimTy
primOpInfo WordMul2Op = mkGenPrimOp (fsLit "timesWord2#")  [] [wordPrimTy, wordPrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy]))
primOpInfo WordQuotOp = mkDyadic (fsLit "quotWord#") wordPrimTy
primOpInfo WordRemOp = mkDyadic (fsLit "remWord#") wordPrimTy
primOpInfo WordQuotRemOp = mkGenPrimOp (fsLit "quotRemWord#")  [] [wordPrimTy, wordPrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy]))
primOpInfo WordQuotRem2Op = mkGenPrimOp (fsLit "quotRemWord2#")  [] [wordPrimTy, wordPrimTy, wordPrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy]))
primOpInfo AndOp = mkDyadic (fsLit "and#") wordPrimTy
primOpInfo OrOp = mkDyadic (fsLit "or#") wordPrimTy
primOpInfo XorOp = mkDyadic (fsLit "xor#") wordPrimTy
primOpInfo NotOp = mkMonadic (fsLit "not#") wordPrimTy
primOpInfo SllOp = mkGenPrimOp (fsLit "uncheckedShiftL#")  [] [wordPrimTy, intPrimTy] (wordPrimTy)
primOpInfo SrlOp = mkGenPrimOp (fsLit "uncheckedShiftRL#")  [] [wordPrimTy, intPrimTy] (wordPrimTy)
primOpInfo Word2IntOp = mkGenPrimOp (fsLit "word2Int#")  [] [wordPrimTy] (intPrimTy)
primOpInfo WordGtOp = mkCompare (fsLit "gtWord#") wordPrimTy
primOpInfo WordGeOp = mkCompare (fsLit "geWord#") wordPrimTy
primOpInfo WordEqOp = mkCompare (fsLit "eqWord#") wordPrimTy
primOpInfo WordNeOp = mkCompare (fsLit "neWord#") wordPrimTy
primOpInfo WordLtOp = mkCompare (fsLit "ltWord#") wordPrimTy
primOpInfo WordLeOp = mkCompare (fsLit "leWord#") wordPrimTy
primOpInfo PopCnt8Op = mkMonadic (fsLit "popCnt8#") wordPrimTy
primOpInfo PopCnt16Op = mkMonadic (fsLit "popCnt16#") wordPrimTy
primOpInfo PopCnt32Op = mkMonadic (fsLit "popCnt32#") wordPrimTy
primOpInfo PopCnt64Op = mkGenPrimOp (fsLit "popCnt64#")  [] [word64PrimTy] (wordPrimTy)
primOpInfo PopCntOp = mkMonadic (fsLit "popCnt#") wordPrimTy
primOpInfo Clz8Op = mkMonadic (fsLit "clz8#") wordPrimTy
primOpInfo Clz16Op = mkMonadic (fsLit "clz16#") wordPrimTy
primOpInfo Clz32Op = mkMonadic (fsLit "clz32#") wordPrimTy
primOpInfo Clz64Op = mkGenPrimOp (fsLit "clz64#")  [] [word64PrimTy] (wordPrimTy)
primOpInfo ClzOp = mkMonadic (fsLit "clz#") wordPrimTy
primOpInfo Ctz8Op = mkMonadic (fsLit "ctz8#") wordPrimTy
primOpInfo Ctz16Op = mkMonadic (fsLit "ctz16#") wordPrimTy
primOpInfo Ctz32Op = mkMonadic (fsLit "ctz32#") wordPrimTy
primOpInfo Ctz64Op = mkGenPrimOp (fsLit "ctz64#")  [] [word64PrimTy] (wordPrimTy)
primOpInfo CtzOp = mkMonadic (fsLit "ctz#") wordPrimTy
primOpInfo BSwap16Op = mkMonadic (fsLit "byteSwap16#") wordPrimTy
primOpInfo BSwap32Op = mkMonadic (fsLit "byteSwap32#") wordPrimTy
primOpInfo BSwap64Op = mkMonadic (fsLit "byteSwap64#") word64PrimTy
primOpInfo BSwapOp = mkMonadic (fsLit "byteSwap#") wordPrimTy
primOpInfo Narrow8IntOp = mkMonadic (fsLit "narrow8Int#") intPrimTy
primOpInfo Narrow16IntOp = mkMonadic (fsLit "narrow16Int#") intPrimTy
primOpInfo Narrow32IntOp = mkMonadic (fsLit "narrow32Int#") intPrimTy
primOpInfo Narrow8WordOp = mkMonadic (fsLit "narrow8Word#") wordPrimTy
primOpInfo Narrow16WordOp = mkMonadic (fsLit "narrow16Word#") wordPrimTy
primOpInfo Narrow32WordOp = mkMonadic (fsLit "narrow32Word#") wordPrimTy
primOpInfo DoubleGtOp = mkCompare (fsLit ">##") doublePrimTy
primOpInfo DoubleGeOp = mkCompare (fsLit ">=##") doublePrimTy
primOpInfo DoubleEqOp = mkCompare (fsLit "==##") doublePrimTy
primOpInfo DoubleNeOp = mkCompare (fsLit "/=##") doublePrimTy
primOpInfo DoubleLtOp = mkCompare (fsLit "<##") doublePrimTy
primOpInfo DoubleLeOp = mkCompare (fsLit "<=##") doublePrimTy
primOpInfo DoubleAddOp = mkDyadic (fsLit "+##") doublePrimTy
primOpInfo DoubleSubOp = mkDyadic (fsLit "-##") doublePrimTy
primOpInfo DoubleMulOp = mkDyadic (fsLit "*##") doublePrimTy
primOpInfo DoubleDivOp = mkDyadic (fsLit "/##") doublePrimTy
primOpInfo DoubleNegOp = mkMonadic (fsLit "negateDouble#") doublePrimTy
primOpInfo Double2IntOp = mkGenPrimOp (fsLit "double2Int#")  [] [doublePrimTy] (intPrimTy)
primOpInfo Double2FloatOp = mkGenPrimOp (fsLit "double2Float#")  [] [doublePrimTy] (floatPrimTy)
primOpInfo DoubleExpOp = mkMonadic (fsLit "expDouble#") doublePrimTy
primOpInfo DoubleLogOp = mkMonadic (fsLit "logDouble#") doublePrimTy
primOpInfo DoubleSqrtOp = mkMonadic (fsLit "sqrtDouble#") doublePrimTy
primOpInfo DoubleSinOp = mkMonadic (fsLit "sinDouble#") doublePrimTy
primOpInfo DoubleCosOp = mkMonadic (fsLit "cosDouble#") doublePrimTy
primOpInfo DoubleTanOp = mkMonadic (fsLit "tanDouble#") doublePrimTy
primOpInfo DoubleAsinOp = mkMonadic (fsLit "asinDouble#") doublePrimTy
primOpInfo DoubleAcosOp = mkMonadic (fsLit "acosDouble#") doublePrimTy
primOpInfo DoubleAtanOp = mkMonadic (fsLit "atanDouble#") doublePrimTy
primOpInfo DoubleSinhOp = mkMonadic (fsLit "sinhDouble#") doublePrimTy
primOpInfo DoubleCoshOp = mkMonadic (fsLit "coshDouble#") doublePrimTy
primOpInfo DoubleTanhOp = mkMonadic (fsLit "tanhDouble#") doublePrimTy
primOpInfo DoublePowerOp = mkDyadic (fsLit "**##") doublePrimTy
primOpInfo DoubleDecode_2IntOp = mkGenPrimOp (fsLit "decodeDouble_2Int#")  [] [doublePrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, wordPrimTy, wordPrimTy, intPrimTy]))
primOpInfo DoubleDecode_Int64Op = mkGenPrimOp (fsLit "decodeDouble_Int64#")  [] [doublePrimTy] ((mkTupleTy UnboxedTuple [int64PrimTy, intPrimTy]))
primOpInfo FloatGtOp = mkCompare (fsLit "gtFloat#") floatPrimTy
primOpInfo FloatGeOp = mkCompare (fsLit "geFloat#") floatPrimTy
primOpInfo FloatEqOp = mkCompare (fsLit "eqFloat#") floatPrimTy
primOpInfo FloatNeOp = mkCompare (fsLit "neFloat#") floatPrimTy
primOpInfo FloatLtOp = mkCompare (fsLit "ltFloat#") floatPrimTy
primOpInfo FloatLeOp = mkCompare (fsLit "leFloat#") floatPrimTy
primOpInfo FloatAddOp = mkDyadic (fsLit "plusFloat#") floatPrimTy
primOpInfo FloatSubOp = mkDyadic (fsLit "minusFloat#") floatPrimTy
primOpInfo FloatMulOp = mkDyadic (fsLit "timesFloat#") floatPrimTy
primOpInfo FloatDivOp = mkDyadic (fsLit "divideFloat#") floatPrimTy
primOpInfo FloatNegOp = mkMonadic (fsLit "negateFloat#") floatPrimTy
primOpInfo Float2IntOp = mkGenPrimOp (fsLit "float2Int#")  [] [floatPrimTy] (intPrimTy)
primOpInfo FloatExpOp = mkMonadic (fsLit "expFloat#") floatPrimTy
primOpInfo FloatLogOp = mkMonadic (fsLit "logFloat#") floatPrimTy
primOpInfo FloatSqrtOp = mkMonadic (fsLit "sqrtFloat#") floatPrimTy
primOpInfo FloatSinOp = mkMonadic (fsLit "sinFloat#") floatPrimTy
primOpInfo FloatCosOp = mkMonadic (fsLit "cosFloat#") floatPrimTy
primOpInfo FloatTanOp = mkMonadic (fsLit "tanFloat#") floatPrimTy
primOpInfo FloatAsinOp = mkMonadic (fsLit "asinFloat#") floatPrimTy
primOpInfo FloatAcosOp = mkMonadic (fsLit "acosFloat#") floatPrimTy
primOpInfo FloatAtanOp = mkMonadic (fsLit "atanFloat#") floatPrimTy
primOpInfo FloatSinhOp = mkMonadic (fsLit "sinhFloat#") floatPrimTy
primOpInfo FloatCoshOp = mkMonadic (fsLit "coshFloat#") floatPrimTy
primOpInfo FloatTanhOp = mkMonadic (fsLit "tanhFloat#") floatPrimTy
primOpInfo FloatPowerOp = mkDyadic (fsLit "powerFloat#") floatPrimTy
primOpInfo Float2DoubleOp = mkGenPrimOp (fsLit "float2Double#")  [] [floatPrimTy] (doublePrimTy)
primOpInfo FloatDecode_IntOp = mkGenPrimOp (fsLit "decodeFloat_Int#")  [] [floatPrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy]))
primOpInfo NewArrayOp = mkGenPrimOp (fsLit "newArray#")  [alphaTyVar, deltaTyVar] [intPrimTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo SameMutableArrayOp = mkGenPrimOp (fsLit "sameMutableArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, mkMutableArrayPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo ReadArrayOp = mkGenPrimOp (fsLit "readArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo WriteArrayOp = mkGenPrimOp (fsLit "writeArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo SizeofArrayOp = mkGenPrimOp (fsLit "sizeofArray#")  [alphaTyVar] [mkArrayPrimTy alphaTy] (intPrimTy)
primOpInfo SizeofMutableArrayOp = mkGenPrimOp (fsLit "sizeofMutableArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo IndexArrayOp = mkGenPrimOp (fsLit "indexArray#")  [alphaTyVar] [mkArrayPrimTy alphaTy, intPrimTy] ((mkTupleTy UnboxedTuple [alphaTy]))
primOpInfo UnsafeFreezeArrayOp = mkGenPrimOp (fsLit "unsafeFreezeArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkArrayPrimTy alphaTy]))
primOpInfo UnsafeThawArrayOp = mkGenPrimOp (fsLit "unsafeThawArray#")  [alphaTyVar, deltaTyVar] [mkArrayPrimTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo CopyArrayOp = mkGenPrimOp (fsLit "copyArray#")  [alphaTyVar, deltaTyVar] [mkArrayPrimTy alphaTy, intPrimTy, mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyMutableArrayOp = mkGenPrimOp (fsLit "copyMutableArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CloneArrayOp = mkGenPrimOp (fsLit "cloneArray#")  [alphaTyVar] [mkArrayPrimTy alphaTy, intPrimTy, intPrimTy] (mkArrayPrimTy alphaTy)
primOpInfo CloneMutableArrayOp = mkGenPrimOp (fsLit "cloneMutableArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo FreezeArrayOp = mkGenPrimOp (fsLit "freezeArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkArrayPrimTy alphaTy]))
primOpInfo ThawArrayOp = mkGenPrimOp (fsLit "thawArray#")  [alphaTyVar, deltaTyVar] [mkArrayPrimTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo CasArrayOp = mkGenPrimOp (fsLit "casArray#")  [deltaTyVar, alphaTyVar] [mkMutableArrayPrimTy deltaTy alphaTy, intPrimTy, alphaTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo NewSmallArrayOp = mkGenPrimOp (fsLit "newSmallArray#")  [alphaTyVar, deltaTyVar] [intPrimTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo SameSmallMutableArrayOp = mkGenPrimOp (fsLit "sameSmallMutableArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, mkSmallMutableArrayPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo ReadSmallArrayOp = mkGenPrimOp (fsLit "readSmallArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo WriteSmallArrayOp = mkGenPrimOp (fsLit "writeSmallArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo SizeofSmallArrayOp = mkGenPrimOp (fsLit "sizeofSmallArray#")  [alphaTyVar] [mkSmallArrayPrimTy alphaTy] (intPrimTy)
primOpInfo SizeofSmallMutableArrayOp = mkGenPrimOp (fsLit "sizeofSmallMutableArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo IndexSmallArrayOp = mkGenPrimOp (fsLit "indexSmallArray#")  [alphaTyVar] [mkSmallArrayPrimTy alphaTy, intPrimTy] ((mkTupleTy UnboxedTuple [alphaTy]))
primOpInfo UnsafeFreezeSmallArrayOp = mkGenPrimOp (fsLit "unsafeFreezeSmallArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallArrayPrimTy alphaTy]))
primOpInfo UnsafeThawSmallArrayOp = mkGenPrimOp (fsLit "unsafeThawSmallArray#")  [alphaTyVar, deltaTyVar] [mkSmallArrayPrimTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo CopySmallArrayOp = mkGenPrimOp (fsLit "copySmallArray#")  [alphaTyVar, deltaTyVar] [mkSmallArrayPrimTy alphaTy, intPrimTy, mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopySmallMutableArrayOp = mkGenPrimOp (fsLit "copySmallMutableArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CloneSmallArrayOp = mkGenPrimOp (fsLit "cloneSmallArray#")  [alphaTyVar] [mkSmallArrayPrimTy alphaTy, intPrimTy, intPrimTy] (mkSmallArrayPrimTy alphaTy)
primOpInfo CloneSmallMutableArrayOp = mkGenPrimOp (fsLit "cloneSmallMutableArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo FreezeSmallArrayOp = mkGenPrimOp (fsLit "freezeSmallArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallArrayPrimTy alphaTy]))
primOpInfo ThawSmallArrayOp = mkGenPrimOp (fsLit "thawSmallArray#")  [alphaTyVar, deltaTyVar] [mkSmallArrayPrimTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkSmallMutableArrayPrimTy deltaTy alphaTy]))
primOpInfo CasSmallArrayOp = mkGenPrimOp (fsLit "casSmallArray#")  [deltaTyVar, alphaTyVar] [mkSmallMutableArrayPrimTy deltaTy alphaTy, intPrimTy, alphaTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo NewByteArrayOp_Char = mkGenPrimOp (fsLit "newByteArray#")  [deltaTyVar] [intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy]))
primOpInfo NewPinnedByteArrayOp_Char = mkGenPrimOp (fsLit "newPinnedByteArray#")  [deltaTyVar] [intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy]))
primOpInfo NewAlignedPinnedByteArrayOp_Char = mkGenPrimOp (fsLit "newAlignedPinnedByteArray#")  [deltaTyVar] [intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy]))
primOpInfo ByteArrayContents_Char = mkGenPrimOp (fsLit "byteArrayContents#")  [] [byteArrayPrimTy] (addrPrimTy)
primOpInfo SameMutableByteArrayOp = mkGenPrimOp (fsLit "sameMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy] (intPrimTy)
primOpInfo ShrinkMutableByteArrayOp_Char = mkGenPrimOp (fsLit "shrinkMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo ResizeMutableByteArrayOp_Char = mkGenPrimOp (fsLit "resizeMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy]))
primOpInfo UnsafeFreezeByteArrayOp = mkGenPrimOp (fsLit "unsafeFreezeByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, byteArrayPrimTy]))
primOpInfo SizeofByteArrayOp = mkGenPrimOp (fsLit "sizeofByteArray#")  [] [byteArrayPrimTy] (intPrimTy)
primOpInfo SizeofMutableByteArrayOp = mkGenPrimOp (fsLit "sizeofMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy] (intPrimTy)
primOpInfo GetSizeofMutableByteArrayOp = mkGenPrimOp (fsLit "getSizeofMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, mkStatePrimTy deltaTy] (mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy])
primOpInfo IndexByteArrayOp_Char = mkGenPrimOp (fsLit "indexCharArray#")  [] [byteArrayPrimTy, intPrimTy] (charPrimTy)
primOpInfo IndexByteArrayOp_WideChar = mkGenPrimOp (fsLit "indexWideCharArray#")  [] [byteArrayPrimTy, intPrimTy] (charPrimTy)
primOpInfo IndexByteArrayOp_Int = mkGenPrimOp (fsLit "indexIntArray#")  [] [byteArrayPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexByteArrayOp_Word = mkGenPrimOp (fsLit "indexWordArray#")  [] [byteArrayPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexByteArrayOp_Addr = mkGenPrimOp (fsLit "indexAddrArray#")  [] [byteArrayPrimTy, intPrimTy] (addrPrimTy)
primOpInfo IndexByteArrayOp_Float = mkGenPrimOp (fsLit "indexFloatArray#")  [] [byteArrayPrimTy, intPrimTy] (floatPrimTy)
primOpInfo IndexByteArrayOp_Double = mkGenPrimOp (fsLit "indexDoubleArray#")  [] [byteArrayPrimTy, intPrimTy] (doublePrimTy)
primOpInfo IndexByteArrayOp_StablePtr = mkGenPrimOp (fsLit "indexStablePtrArray#")  [alphaTyVar] [byteArrayPrimTy, intPrimTy] (mkStablePtrPrimTy alphaTy)
primOpInfo IndexByteArrayOp_Int8 = mkGenPrimOp (fsLit "indexInt8Array#")  [] [byteArrayPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexByteArrayOp_Int16 = mkGenPrimOp (fsLit "indexInt16Array#")  [] [byteArrayPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexByteArrayOp_Int32 = mkGenPrimOp (fsLit "indexInt32Array#")  [] [byteArrayPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexByteArrayOp_Int64 = mkGenPrimOp (fsLit "indexInt64Array#")  [] [byteArrayPrimTy, intPrimTy] (int64PrimTy)
primOpInfo IndexByteArrayOp_Word8 = mkGenPrimOp (fsLit "indexWord8Array#")  [] [byteArrayPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexByteArrayOp_Word16 = mkGenPrimOp (fsLit "indexWord16Array#")  [] [byteArrayPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexByteArrayOp_Word32 = mkGenPrimOp (fsLit "indexWord32Array#")  [] [byteArrayPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexByteArrayOp_Word64 = mkGenPrimOp (fsLit "indexWord64Array#")  [] [byteArrayPrimTy, intPrimTy] (word64PrimTy)
primOpInfo ReadByteArrayOp_Char = mkGenPrimOp (fsLit "readCharArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, charPrimTy]))
primOpInfo ReadByteArrayOp_WideChar = mkGenPrimOp (fsLit "readWideCharArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, charPrimTy]))
primOpInfo ReadByteArrayOp_Int = mkGenPrimOp (fsLit "readIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadByteArrayOp_Word = mkGenPrimOp (fsLit "readWordArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadByteArrayOp_Addr = mkGenPrimOp (fsLit "readAddrArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, addrPrimTy]))
primOpInfo ReadByteArrayOp_Float = mkGenPrimOp (fsLit "readFloatArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatPrimTy]))
primOpInfo ReadByteArrayOp_Double = mkGenPrimOp (fsLit "readDoubleArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doublePrimTy]))
primOpInfo ReadByteArrayOp_StablePtr = mkGenPrimOp (fsLit "readStablePtrArray#")  [deltaTyVar, alphaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkStablePtrPrimTy alphaTy]))
primOpInfo ReadByteArrayOp_Int8 = mkGenPrimOp (fsLit "readInt8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadByteArrayOp_Int16 = mkGenPrimOp (fsLit "readInt16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadByteArrayOp_Int32 = mkGenPrimOp (fsLit "readInt32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadByteArrayOp_Int64 = mkGenPrimOp (fsLit "readInt64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64PrimTy]))
primOpInfo ReadByteArrayOp_Word8 = mkGenPrimOp (fsLit "readWord8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadByteArrayOp_Word16 = mkGenPrimOp (fsLit "readWord16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadByteArrayOp_Word32 = mkGenPrimOp (fsLit "readWord32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadByteArrayOp_Word64 = mkGenPrimOp (fsLit "readWord64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64PrimTy]))
primOpInfo WriteByteArrayOp_Char = mkGenPrimOp (fsLit "writeCharArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, charPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_WideChar = mkGenPrimOp (fsLit "writeWideCharArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, charPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Int = mkGenPrimOp (fsLit "writeIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Word = mkGenPrimOp (fsLit "writeWordArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Addr = mkGenPrimOp (fsLit "writeAddrArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, addrPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Float = mkGenPrimOp (fsLit "writeFloatArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Double = mkGenPrimOp (fsLit "writeDoubleArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doublePrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_StablePtr = mkGenPrimOp (fsLit "writeStablePtrArray#")  [deltaTyVar, alphaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStablePtrPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Int8 = mkGenPrimOp (fsLit "writeInt8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Int16 = mkGenPrimOp (fsLit "writeInt16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Int32 = mkGenPrimOp (fsLit "writeInt32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Int64 = mkGenPrimOp (fsLit "writeInt64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Word8 = mkGenPrimOp (fsLit "writeWord8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Word16 = mkGenPrimOp (fsLit "writeWord16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Word32 = mkGenPrimOp (fsLit "writeWord32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteByteArrayOp_Word64 = mkGenPrimOp (fsLit "writeWord64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyByteArrayOp = mkGenPrimOp (fsLit "copyByteArray#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyMutableByteArrayOp = mkGenPrimOp (fsLit "copyMutableByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyByteArrayToAddrOp = mkGenPrimOp (fsLit "copyByteArrayToAddr#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyMutableByteArrayToAddrOp = mkGenPrimOp (fsLit "copyMutableByteArrayToAddr#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyAddrToByteArrayOp = mkGenPrimOp (fsLit "copyAddrToByteArray#")  [deltaTyVar] [addrPrimTy, mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo SetByteArrayOp = mkGenPrimOp (fsLit "setByteArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo AtomicReadByteArrayOp_Int = mkGenPrimOp (fsLit "atomicReadIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo AtomicWriteByteArrayOp_Int = mkGenPrimOp (fsLit "atomicWriteIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CasByteArrayOp_Int = mkGenPrimOp (fsLit "casIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchAddByteArrayOp_Int = mkGenPrimOp (fsLit "fetchAddIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchSubByteArrayOp_Int = mkGenPrimOp (fsLit "fetchSubIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchAndByteArrayOp_Int = mkGenPrimOp (fsLit "fetchAndIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchNandByteArrayOp_Int = mkGenPrimOp (fsLit "fetchNandIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchOrByteArrayOp_Int = mkGenPrimOp (fsLit "fetchOrIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo FetchXorByteArrayOp_Int = mkGenPrimOp (fsLit "fetchXorIntArray#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo NewArrayArrayOp = mkGenPrimOp (fsLit "newArrayArray#")  [deltaTyVar] [intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayArrayPrimTy deltaTy]))
primOpInfo SameMutableArrayArrayOp = mkGenPrimOp (fsLit "sameMutableArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, mkMutableArrayArrayPrimTy deltaTy] (intPrimTy)
primOpInfo UnsafeFreezeArrayArrayOp = mkGenPrimOp (fsLit "unsafeFreezeArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkArrayArrayPrimTy]))
primOpInfo SizeofArrayArrayOp = mkGenPrimOp (fsLit "sizeofArrayArray#")  [] [mkArrayArrayPrimTy] (intPrimTy)
primOpInfo SizeofMutableArrayArrayOp = mkGenPrimOp (fsLit "sizeofMutableArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy] (intPrimTy)
primOpInfo IndexArrayArrayOp_ByteArray = mkGenPrimOp (fsLit "indexByteArrayArray#")  [] [mkArrayArrayPrimTy, intPrimTy] (byteArrayPrimTy)
primOpInfo IndexArrayArrayOp_ArrayArray = mkGenPrimOp (fsLit "indexArrayArrayArray#")  [] [mkArrayArrayPrimTy, intPrimTy] (mkArrayArrayPrimTy)
primOpInfo ReadArrayArrayOp_ByteArray = mkGenPrimOp (fsLit "readByteArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, byteArrayPrimTy]))
primOpInfo ReadArrayArrayOp_MutableByteArray = mkGenPrimOp (fsLit "readMutableByteArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableByteArrayPrimTy deltaTy]))
primOpInfo ReadArrayArrayOp_ArrayArray = mkGenPrimOp (fsLit "readArrayArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkArrayArrayPrimTy]))
primOpInfo ReadArrayArrayOp_MutableArrayArray = mkGenPrimOp (fsLit "readMutableArrayArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutableArrayArrayPrimTy deltaTy]))
primOpInfo WriteArrayArrayOp_ByteArray = mkGenPrimOp (fsLit "writeByteArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, byteArrayPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteArrayArrayOp_MutableByteArray = mkGenPrimOp (fsLit "writeMutableByteArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkMutableByteArrayPrimTy deltaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteArrayArrayOp_ArrayArray = mkGenPrimOp (fsLit "writeArrayArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkArrayArrayPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteArrayArrayOp_MutableArrayArray = mkGenPrimOp (fsLit "writeMutableArrayArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkMutableArrayArrayPrimTy deltaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyArrayArrayOp = mkGenPrimOp (fsLit "copyArrayArray#")  [deltaTyVar] [mkArrayArrayPrimTy, intPrimTy, mkMutableArrayArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo CopyMutableArrayArrayOp = mkGenPrimOp (fsLit "copyMutableArrayArray#")  [deltaTyVar] [mkMutableArrayArrayPrimTy deltaTy, intPrimTy, mkMutableArrayArrayPrimTy deltaTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo AddrAddOp = mkGenPrimOp (fsLit "plusAddr#")  [] [addrPrimTy, intPrimTy] (addrPrimTy)
primOpInfo AddrSubOp = mkGenPrimOp (fsLit "minusAddr#")  [] [addrPrimTy, addrPrimTy] (intPrimTy)
primOpInfo AddrRemOp = mkGenPrimOp (fsLit "remAddr#")  [] [addrPrimTy, intPrimTy] (intPrimTy)
primOpInfo Addr2IntOp = mkGenPrimOp (fsLit "addr2Int#")  [] [addrPrimTy] (intPrimTy)
primOpInfo Int2AddrOp = mkGenPrimOp (fsLit "int2Addr#")  [] [intPrimTy] (addrPrimTy)
primOpInfo AddrGtOp = mkCompare (fsLit "gtAddr#") addrPrimTy
primOpInfo AddrGeOp = mkCompare (fsLit "geAddr#") addrPrimTy
primOpInfo AddrEqOp = mkCompare (fsLit "eqAddr#") addrPrimTy
primOpInfo AddrNeOp = mkCompare (fsLit "neAddr#") addrPrimTy
primOpInfo AddrLtOp = mkCompare (fsLit "ltAddr#") addrPrimTy
primOpInfo AddrLeOp = mkCompare (fsLit "leAddr#") addrPrimTy
primOpInfo IndexOffAddrOp_Char = mkGenPrimOp (fsLit "indexCharOffAddr#")  [] [addrPrimTy, intPrimTy] (charPrimTy)
primOpInfo IndexOffAddrOp_WideChar = mkGenPrimOp (fsLit "indexWideCharOffAddr#")  [] [addrPrimTy, intPrimTy] (charPrimTy)
primOpInfo IndexOffAddrOp_Int = mkGenPrimOp (fsLit "indexIntOffAddr#")  [] [addrPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexOffAddrOp_Word = mkGenPrimOp (fsLit "indexWordOffAddr#")  [] [addrPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexOffAddrOp_Addr = mkGenPrimOp (fsLit "indexAddrOffAddr#")  [] [addrPrimTy, intPrimTy] (addrPrimTy)
primOpInfo IndexOffAddrOp_Float = mkGenPrimOp (fsLit "indexFloatOffAddr#")  [] [addrPrimTy, intPrimTy] (floatPrimTy)
primOpInfo IndexOffAddrOp_Double = mkGenPrimOp (fsLit "indexDoubleOffAddr#")  [] [addrPrimTy, intPrimTy] (doublePrimTy)
primOpInfo IndexOffAddrOp_StablePtr = mkGenPrimOp (fsLit "indexStablePtrOffAddr#")  [alphaTyVar] [addrPrimTy, intPrimTy] (mkStablePtrPrimTy alphaTy)
primOpInfo IndexOffAddrOp_Int8 = mkGenPrimOp (fsLit "indexInt8OffAddr#")  [] [addrPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexOffAddrOp_Int16 = mkGenPrimOp (fsLit "indexInt16OffAddr#")  [] [addrPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexOffAddrOp_Int32 = mkGenPrimOp (fsLit "indexInt32OffAddr#")  [] [addrPrimTy, intPrimTy] (intPrimTy)
primOpInfo IndexOffAddrOp_Int64 = mkGenPrimOp (fsLit "indexInt64OffAddr#")  [] [addrPrimTy, intPrimTy] (int64PrimTy)
primOpInfo IndexOffAddrOp_Word8 = mkGenPrimOp (fsLit "indexWord8OffAddr#")  [] [addrPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexOffAddrOp_Word16 = mkGenPrimOp (fsLit "indexWord16OffAddr#")  [] [addrPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexOffAddrOp_Word32 = mkGenPrimOp (fsLit "indexWord32OffAddr#")  [] [addrPrimTy, intPrimTy] (wordPrimTy)
primOpInfo IndexOffAddrOp_Word64 = mkGenPrimOp (fsLit "indexWord64OffAddr#")  [] [addrPrimTy, intPrimTy] (word64PrimTy)
primOpInfo ReadOffAddrOp_Char = mkGenPrimOp (fsLit "readCharOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, charPrimTy]))
primOpInfo ReadOffAddrOp_WideChar = mkGenPrimOp (fsLit "readWideCharOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, charPrimTy]))
primOpInfo ReadOffAddrOp_Int = mkGenPrimOp (fsLit "readIntOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadOffAddrOp_Word = mkGenPrimOp (fsLit "readWordOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadOffAddrOp_Addr = mkGenPrimOp (fsLit "readAddrOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, addrPrimTy]))
primOpInfo ReadOffAddrOp_Float = mkGenPrimOp (fsLit "readFloatOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatPrimTy]))
primOpInfo ReadOffAddrOp_Double = mkGenPrimOp (fsLit "readDoubleOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doublePrimTy]))
primOpInfo ReadOffAddrOp_StablePtr = mkGenPrimOp (fsLit "readStablePtrOffAddr#")  [deltaTyVar, alphaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkStablePtrPrimTy alphaTy]))
primOpInfo ReadOffAddrOp_Int8 = mkGenPrimOp (fsLit "readInt8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadOffAddrOp_Int16 = mkGenPrimOp (fsLit "readInt16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadOffAddrOp_Int32 = mkGenPrimOp (fsLit "readInt32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadOffAddrOp_Int64 = mkGenPrimOp (fsLit "readInt64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64PrimTy]))
primOpInfo ReadOffAddrOp_Word8 = mkGenPrimOp (fsLit "readWord8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadOffAddrOp_Word16 = mkGenPrimOp (fsLit "readWord16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadOffAddrOp_Word32 = mkGenPrimOp (fsLit "readWord32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, wordPrimTy]))
primOpInfo ReadOffAddrOp_Word64 = mkGenPrimOp (fsLit "readWord64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64PrimTy]))
primOpInfo WriteOffAddrOp_Char = mkGenPrimOp (fsLit "writeCharOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, charPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_WideChar = mkGenPrimOp (fsLit "writeWideCharOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, charPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Int = mkGenPrimOp (fsLit "writeIntOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Word = mkGenPrimOp (fsLit "writeWordOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Addr = mkGenPrimOp (fsLit "writeAddrOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, addrPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Float = mkGenPrimOp (fsLit "writeFloatOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Double = mkGenPrimOp (fsLit "writeDoubleOffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, doublePrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_StablePtr = mkGenPrimOp (fsLit "writeStablePtrOffAddr#")  [alphaTyVar, deltaTyVar] [addrPrimTy, intPrimTy, mkStablePtrPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Int8 = mkGenPrimOp (fsLit "writeInt8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Int16 = mkGenPrimOp (fsLit "writeInt16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Int32 = mkGenPrimOp (fsLit "writeInt32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Int64 = mkGenPrimOp (fsLit "writeInt64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Word8 = mkGenPrimOp (fsLit "writeWord8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Word16 = mkGenPrimOp (fsLit "writeWord16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Word32 = mkGenPrimOp (fsLit "writeWord32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, wordPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WriteOffAddrOp_Word64 = mkGenPrimOp (fsLit "writeWord64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo NewMutVarOp = mkGenPrimOp (fsLit "newMutVar#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMutVarPrimTy deltaTy alphaTy]))
primOpInfo ReadMutVarOp = mkGenPrimOp (fsLit "readMutVar#")  [deltaTyVar, alphaTyVar] [mkMutVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo WriteMutVarOp = mkGenPrimOp (fsLit "writeMutVar#")  [deltaTyVar, alphaTyVar] [mkMutVarPrimTy deltaTy alphaTy, alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo SameMutVarOp = mkGenPrimOp (fsLit "sameMutVar#")  [deltaTyVar, alphaTyVar] [mkMutVarPrimTy deltaTy alphaTy, mkMutVarPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo AtomicModifyMutVarOp = mkGenPrimOp (fsLit "atomicModifyMutVar#")  [deltaTyVar, alphaTyVar, betaTyVar, gammaTyVar] [mkMutVarPrimTy deltaTy alphaTy, (mkFunTy (alphaTy) (betaTy)), mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, gammaTy]))
primOpInfo CasMutVarOp = mkGenPrimOp (fsLit "casMutVar#")  [deltaTyVar, alphaTyVar] [mkMutVarPrimTy deltaTy alphaTy, alphaTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo CatchOp = mkGenPrimOp (fsLit "catch#")  [alphaTyVar, betaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), (mkFunTy (betaTy) ((mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo RaiseOp = mkGenPrimOp (fsLit "raise#")  [alphaTyVar, betaTyVar] [alphaTy] (betaTy)
primOpInfo RaiseIOOp = mkGenPrimOp (fsLit "raiseIO#")  [alphaTyVar, betaTyVar] [alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, betaTy]))
primOpInfo MaskAsyncExceptionsOp = mkGenPrimOp (fsLit "maskAsyncExceptions#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo MaskUninterruptibleOp = mkGenPrimOp (fsLit "maskUninterruptible#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo UnmaskAsyncExceptionsOp = mkGenPrimOp (fsLit "unmaskAsyncExceptions#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo MaskStatus = mkGenPrimOp (fsLit "getMaskingState#")  [] [mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy]))
primOpInfo AtomicallyOp = mkGenPrimOp (fsLit "atomically#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo RetryOp = mkGenPrimOp (fsLit "retry#")  [alphaTyVar] [mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo CatchRetryOp = mkGenPrimOp (fsLit "catchRetry#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), (mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo CatchSTMOp = mkGenPrimOp (fsLit "catchSTM#")  [alphaTyVar, betaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), (mkFunTy (betaTy) ((mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo Check = mkGenPrimOp (fsLit "check#")  [alphaTyVar] [(mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))), mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, unitTy]))
primOpInfo NewTVarOp = mkGenPrimOp (fsLit "newTVar#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkTVarPrimTy deltaTy alphaTy]))
primOpInfo ReadTVarOp = mkGenPrimOp (fsLit "readTVar#")  [deltaTyVar, alphaTyVar] [mkTVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo ReadTVarIOOp = mkGenPrimOp (fsLit "readTVarIO#")  [deltaTyVar, alphaTyVar] [mkTVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo WriteTVarOp = mkGenPrimOp (fsLit "writeTVar#")  [deltaTyVar, alphaTyVar] [mkTVarPrimTy deltaTy alphaTy, alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo SameTVarOp = mkGenPrimOp (fsLit "sameTVar#")  [deltaTyVar, alphaTyVar] [mkTVarPrimTy deltaTy alphaTy, mkTVarPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo NewMVarOp = mkGenPrimOp (fsLit "newMVar#")  [deltaTyVar, alphaTyVar] [mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, mkMVarPrimTy deltaTy alphaTy]))
primOpInfo TakeMVarOp = mkGenPrimOp (fsLit "takeMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo TryTakeMVarOp = mkGenPrimOp (fsLit "tryTakeMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo PutMVarOp = mkGenPrimOp (fsLit "putMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo TryPutMVarOp = mkGenPrimOp (fsLit "tryPutMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ReadMVarOp = mkGenPrimOp (fsLit "readMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo TryReadMVarOp = mkGenPrimOp (fsLit "tryReadMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo SameMVarOp = mkGenPrimOp (fsLit "sameMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkMVarPrimTy deltaTy alphaTy] (intPrimTy)
primOpInfo IsEmptyMVarOp = mkGenPrimOp (fsLit "isEmptyMVar#")  [deltaTyVar, alphaTyVar] [mkMVarPrimTy deltaTy alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo DelayOp = mkGenPrimOp (fsLit "delay#")  [deltaTyVar] [intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WaitReadOp = mkGenPrimOp (fsLit "waitRead#")  [alphaTyVar, deltaTyVar] [mkObjectPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WaitWriteOp = mkGenPrimOp (fsLit "waitWrite#")  [alphaTyVar, deltaTyVar] [mkObjectPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo ForkOp = mkGenPrimOp (fsLit "fork#")  [alphaTyVar] [alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, threadIdPrimTy]))
primOpInfo ForkOnOp = mkGenPrimOp (fsLit "forkOn#")  [alphaTyVar] [intPrimTy, alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, threadIdPrimTy]))
primOpInfo KillThreadOp = mkGenPrimOp (fsLit "killThread#")  [alphaTyVar] [threadIdPrimTy, alphaTy, mkStatePrimTy realWorldTy] (mkStatePrimTy realWorldTy)
primOpInfo YieldOp = mkGenPrimOp (fsLit "yield#")  [] [mkStatePrimTy realWorldTy] (mkStatePrimTy realWorldTy)
primOpInfo MyThreadIdOp = mkGenPrimOp (fsLit "myThreadId#")  [] [mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, threadIdPrimTy]))
primOpInfo LabelThreadOp = mkGenPrimOp (fsLit "labelThread#")  [] [threadIdPrimTy, addrPrimTy, mkStatePrimTy realWorldTy] (mkStatePrimTy realWorldTy)
primOpInfo IsCurrentThreadBoundOp = mkGenPrimOp (fsLit "isCurrentThreadBound#")  [] [mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy]))
primOpInfo NoDuplicateOp = mkGenPrimOp (fsLit "noDuplicate#")  [alphaTyVar] [mkStatePrimTy alphaTy] (mkStatePrimTy alphaTy)
primOpInfo ThreadStatusOp = mkGenPrimOp (fsLit "threadStatus#")  [] [threadIdPrimTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy, intPrimTy, intPrimTy]))
primOpInfo MkWeakOp = mkGenPrimOp (fsLit "mkWeak#")  [openAlphaTyVar, betaTyVar, gammaTyVar] [openAlphaTy, betaTy, gammaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, mkWeakPrimTy betaTy]))
primOpInfo MkWeakNoFinalizerOp = mkGenPrimOp (fsLit "mkWeakNoFinalizer#")  [openAlphaTyVar, betaTyVar] [openAlphaTy, betaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, mkWeakPrimTy betaTy]))
primOpInfo AddCFinalizerToWeakOp = mkGenPrimOp (fsLit "addCFinalizerToWeak#")  [betaTyVar] [addrPrimTy, addrPrimTy, intPrimTy, addrPrimTy, mkWeakPrimTy betaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy]))
primOpInfo DeRefWeakOp = mkGenPrimOp (fsLit "deRefWeak#")  [alphaTyVar] [mkWeakPrimTy alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy, alphaTy]))
primOpInfo FinalizeWeakOp = mkGenPrimOp (fsLit "finalizeWeak#")  [alphaTyVar] [mkWeakPrimTy alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, intPrimTy, (mkFunTy (mkStatePrimTy realWorldTy) ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, unitTy])))]))
primOpInfo TouchOp = mkGenPrimOp (fsLit "touch#")  [openAlphaTyVar] [openAlphaTy, mkStatePrimTy realWorldTy] (mkStatePrimTy realWorldTy)
primOpInfo MakeStablePtrOp = mkGenPrimOp (fsLit "makeStablePtr#")  [alphaTyVar] [alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, mkStablePtrPrimTy alphaTy]))
primOpInfo DeRefStablePtrOp = mkGenPrimOp (fsLit "deRefStablePtr#")  [alphaTyVar] [mkStablePtrPrimTy alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, alphaTy]))
primOpInfo EqStablePtrOp = mkGenPrimOp (fsLit "eqStablePtr#")  [alphaTyVar] [mkStablePtrPrimTy alphaTy, mkStablePtrPrimTy alphaTy] (intPrimTy)
primOpInfo MakeStableNameOp = mkGenPrimOp (fsLit "makeStableName#")  [alphaTyVar] [alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy realWorldTy, mkStableNamePrimTy alphaTy]))
primOpInfo EqStableNameOp = mkGenPrimOp (fsLit "eqStableName#")  [alphaTyVar, betaTyVar] [mkStableNamePrimTy alphaTy, mkStableNamePrimTy betaTy] (intPrimTy)
primOpInfo StableNameToIntOp = mkGenPrimOp (fsLit "stableNameToInt#")  [alphaTyVar] [mkStableNamePrimTy alphaTy] (intPrimTy)
primOpInfo ReallyUnsafePtrEqualityOp = mkGenPrimOp (fsLit "reallyUnsafePtrEquality#")  [alphaTyVar] [alphaTy, alphaTy] (intPrimTy)
primOpInfo ParOp = mkGenPrimOp (fsLit "par#")  [alphaTyVar] [alphaTy] (intPrimTy)
primOpInfo SparkOp = mkGenPrimOp (fsLit "spark#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo SeqOp = mkGenPrimOp (fsLit "seq#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, alphaTy]))
primOpInfo GetSparkOp = mkGenPrimOp (fsLit "getSpark#")  [deltaTyVar, alphaTyVar] [mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy, alphaTy]))
primOpInfo NumSparks = mkGenPrimOp (fsLit "numSparks#")  [deltaTyVar] [mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, intPrimTy]))
primOpInfo ParGlobalOp = mkGenPrimOp (fsLit "parGlobal#")  [alphaTyVar, betaTyVar] [alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, betaTy] (intPrimTy)
primOpInfo ParLocalOp = mkGenPrimOp (fsLit "parLocal#")  [alphaTyVar, betaTyVar] [alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, betaTy] (intPrimTy)
primOpInfo ParAtOp = mkGenPrimOp (fsLit "parAt#")  [betaTyVar, alphaTyVar, gammaTyVar] [betaTy, alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, gammaTy] (intPrimTy)
primOpInfo ParAtAbsOp = mkGenPrimOp (fsLit "parAtAbs#")  [alphaTyVar, betaTyVar] [alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, betaTy] (intPrimTy)
primOpInfo ParAtRelOp = mkGenPrimOp (fsLit "parAtRel#")  [alphaTyVar, betaTyVar] [alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, betaTy] (intPrimTy)
primOpInfo ParAtForNowOp = mkGenPrimOp (fsLit "parAtForNow#")  [betaTyVar, alphaTyVar, gammaTyVar] [betaTy, alphaTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, gammaTy] (intPrimTy)
primOpInfo DataToTagOp = mkGenPrimOp (fsLit "dataToTag#")  [alphaTyVar] [alphaTy] (intPrimTy)
primOpInfo TagToEnumOp = mkGenPrimOp (fsLit "tagToEnum#")  [alphaTyVar] [intPrimTy] (alphaTy)
primOpInfo AddrToAnyOp = mkGenPrimOp (fsLit "addrToAny#")  [alphaTyVar] [addrPrimTy] ((mkTupleTy UnboxedTuple [alphaTy]))
primOpInfo MkApUpd0_Op = mkGenPrimOp (fsLit "mkApUpd0#")  [alphaTyVar] [bcoPrimTy] ((mkTupleTy UnboxedTuple [alphaTy]))
primOpInfo NewBCOOp = mkGenPrimOp (fsLit "newBCO#")  [alphaTyVar, deltaTyVar] [byteArrayPrimTy, byteArrayPrimTy, mkArrayPrimTy alphaTy, intPrimTy, byteArrayPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, bcoPrimTy]))
primOpInfo UnpackClosureOp = mkGenPrimOp (fsLit "unpackClosure#")  [alphaTyVar, betaTyVar] [alphaTy] ((mkTupleTy UnboxedTuple [addrPrimTy, mkArrayPrimTy betaTy, byteArrayPrimTy]))
primOpInfo GetApStackValOp = mkGenPrimOp (fsLit "getApStackVal#")  [alphaTyVar, betaTyVar] [alphaTy, intPrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, betaTy]))
primOpInfo GetCCSOfOp = mkGenPrimOp (fsLit "getCCSOf#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, addrPrimTy]))
primOpInfo GetCurrentCCSOp = mkGenPrimOp (fsLit "getCurrentCCS#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, addrPrimTy]))
primOpInfo TraceEventOp = mkGenPrimOp (fsLit "traceEvent#")  [deltaTyVar] [addrPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo TraceMarkerOp = mkGenPrimOp (fsLit "traceMarker#")  [deltaTyVar] [addrPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecBroadcastOp IntVec 16 W8) = mkGenPrimOp (fsLit "broadcastInt8X16#")  [] [intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 8 W16) = mkGenPrimOp (fsLit "broadcastInt16X8#")  [] [intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 4 W32) = mkGenPrimOp (fsLit "broadcastInt32X4#")  [] [intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 2 W64) = mkGenPrimOp (fsLit "broadcastInt64X2#")  [] [intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 32 W8) = mkGenPrimOp (fsLit "broadcastInt8X32#")  [] [intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 16 W16) = mkGenPrimOp (fsLit "broadcastInt16X16#")  [] [intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 8 W32) = mkGenPrimOp (fsLit "broadcastInt32X8#")  [] [intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 4 W64) = mkGenPrimOp (fsLit "broadcastInt64X4#")  [] [intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 64 W8) = mkGenPrimOp (fsLit "broadcastInt8X64#")  [] [intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 32 W16) = mkGenPrimOp (fsLit "broadcastInt16X32#")  [] [intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 16 W32) = mkGenPrimOp (fsLit "broadcastInt32X16#")  [] [intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecBroadcastOp IntVec 8 W64) = mkGenPrimOp (fsLit "broadcastInt64X8#")  [] [intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 16 W8) = mkGenPrimOp (fsLit "broadcastWord8X16#")  [] [wordPrimTy] (word8X16PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 8 W16) = mkGenPrimOp (fsLit "broadcastWord16X8#")  [] [wordPrimTy] (word16X8PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 4 W32) = mkGenPrimOp (fsLit "broadcastWord32X4#")  [] [wordPrimTy] (word32X4PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 2 W64) = mkGenPrimOp (fsLit "broadcastWord64X2#")  [] [wordPrimTy] (word64X2PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 32 W8) = mkGenPrimOp (fsLit "broadcastWord8X32#")  [] [wordPrimTy] (word8X32PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 16 W16) = mkGenPrimOp (fsLit "broadcastWord16X16#")  [] [wordPrimTy] (word16X16PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 8 W32) = mkGenPrimOp (fsLit "broadcastWord32X8#")  [] [wordPrimTy] (word32X8PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 4 W64) = mkGenPrimOp (fsLit "broadcastWord64X4#")  [] [wordPrimTy] (word64X4PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 64 W8) = mkGenPrimOp (fsLit "broadcastWord8X64#")  [] [wordPrimTy] (word8X64PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 32 W16) = mkGenPrimOp (fsLit "broadcastWord16X32#")  [] [wordPrimTy] (word16X32PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 16 W32) = mkGenPrimOp (fsLit "broadcastWord32X16#")  [] [wordPrimTy] (word32X16PrimTy)
-- primOpInfo (VecBroadcastOp WordVec 8 W64) = mkGenPrimOp (fsLit "broadcastWord64X8#")  [] [wordPrimTy] (word64X8PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 4 W32) = mkGenPrimOp (fsLit "broadcastFloatX4#")  [] [floatPrimTy] (floatX4PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 2 W64) = mkGenPrimOp (fsLit "broadcastDoubleX2#")  [] [doublePrimTy] (doubleX2PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 8 W32) = mkGenPrimOp (fsLit "broadcastFloatX8#")  [] [floatPrimTy] (floatX8PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 4 W64) = mkGenPrimOp (fsLit "broadcastDoubleX4#")  [] [doublePrimTy] (doubleX4PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 16 W32) = mkGenPrimOp (fsLit "broadcastFloatX16#")  [] [floatPrimTy] (floatX16PrimTy)
-- primOpInfo (VecBroadcastOp FloatVec 8 W64) = mkGenPrimOp (fsLit "broadcastDoubleX8#")  [] [doublePrimTy] (doubleX8PrimTy)
-- primOpInfo (VecPackOp IntVec 16 W8) = mkGenPrimOp (fsLit "packInt8X16#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int8X16PrimTy)
-- primOpInfo (VecPackOp IntVec 8 W16) = mkGenPrimOp (fsLit "packInt16X8#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int16X8PrimTy)
-- primOpInfo (VecPackOp IntVec 4 W32) = mkGenPrimOp (fsLit "packInt32X4#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int32X4PrimTy)
-- primOpInfo (VecPackOp IntVec 2 W64) = mkGenPrimOp (fsLit "packInt64X2#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy])] (int64X2PrimTy)
-- primOpInfo (VecPackOp IntVec 32 W8) = mkGenPrimOp (fsLit "packInt8X32#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int8X32PrimTy)
-- primOpInfo (VecPackOp IntVec 16 W16) = mkGenPrimOp (fsLit "packInt16X16#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int16X16PrimTy)
-- primOpInfo (VecPackOp IntVec 8 W32) = mkGenPrimOp (fsLit "packInt32X8#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int32X8PrimTy)
-- primOpInfo (VecPackOp IntVec 4 W64) = mkGenPrimOp (fsLit "packInt64X4#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int64X4PrimTy)
-- primOpInfo (VecPackOp IntVec 64 W8) = mkGenPrimOp (fsLit "packInt8X64#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int8X64PrimTy)
-- primOpInfo (VecPackOp IntVec 32 W16) = mkGenPrimOp (fsLit "packInt16X32#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int16X32PrimTy)
-- primOpInfo (VecPackOp IntVec 16 W32) = mkGenPrimOp (fsLit "packInt32X16#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int32X16PrimTy)
-- primOpInfo (VecPackOp IntVec 8 W64) = mkGenPrimOp (fsLit "packInt64X8#")  [] [(mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy])] (int64X8PrimTy)
-- primOpInfo (VecPackOp WordVec 16 W8) = mkGenPrimOp (fsLit "packWord8X16#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word8X16PrimTy)
-- primOpInfo (VecPackOp WordVec 8 W16) = mkGenPrimOp (fsLit "packWord16X8#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word16X8PrimTy)
-- primOpInfo (VecPackOp WordVec 4 W32) = mkGenPrimOp (fsLit "packWord32X4#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word32X4PrimTy)
-- primOpInfo (VecPackOp WordVec 2 W64) = mkGenPrimOp (fsLit "packWord64X2#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy])] (word64X2PrimTy)
-- primOpInfo (VecPackOp WordVec 32 W8) = mkGenPrimOp (fsLit "packWord8X32#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word8X32PrimTy)
-- primOpInfo (VecPackOp WordVec 16 W16) = mkGenPrimOp (fsLit "packWord16X16#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word16X16PrimTy)
-- primOpInfo (VecPackOp WordVec 8 W32) = mkGenPrimOp (fsLit "packWord32X8#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word32X8PrimTy)
-- primOpInfo (VecPackOp WordVec 4 W64) = mkGenPrimOp (fsLit "packWord64X4#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word64X4PrimTy)
-- primOpInfo (VecPackOp WordVec 64 W8) = mkGenPrimOp (fsLit "packWord8X64#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word8X64PrimTy)
-- primOpInfo (VecPackOp WordVec 32 W16) = mkGenPrimOp (fsLit "packWord16X32#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word16X32PrimTy)
-- primOpInfo (VecPackOp WordVec 16 W32) = mkGenPrimOp (fsLit "packWord32X16#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word32X16PrimTy)
-- primOpInfo (VecPackOp WordVec 8 W64) = mkGenPrimOp (fsLit "packWord64X8#")  [] [(mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy])] (word64X8PrimTy)
-- primOpInfo (VecPackOp FloatVec 4 W32) = mkGenPrimOp (fsLit "packFloatX4#")  [] [(mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy])] (floatX4PrimTy)
-- primOpInfo (VecPackOp FloatVec 2 W64) = mkGenPrimOp (fsLit "packDoubleX2#")  [] [(mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy])] (doubleX2PrimTy)
-- primOpInfo (VecPackOp FloatVec 8 W32) = mkGenPrimOp (fsLit "packFloatX8#")  [] [(mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy])] (floatX8PrimTy)
-- primOpInfo (VecPackOp FloatVec 4 W64) = mkGenPrimOp (fsLit "packDoubleX4#")  [] [(mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy])] (doubleX4PrimTy)
-- primOpInfo (VecPackOp FloatVec 16 W32) = mkGenPrimOp (fsLit "packFloatX16#")  [] [(mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy])] (floatX16PrimTy)
-- primOpInfo (VecPackOp FloatVec 8 W64) = mkGenPrimOp (fsLit "packDoubleX8#")  [] [(mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy])] (doubleX8PrimTy)
-- primOpInfo (VecUnpackOp IntVec 16 W8) = mkGenPrimOp (fsLit "unpackInt8X16#")  [] [int8X16PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 8 W16) = mkGenPrimOp (fsLit "unpackInt16X8#")  [] [int16X8PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 4 W32) = mkGenPrimOp (fsLit "unpackInt32X4#")  [] [int32X4PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 2 W64) = mkGenPrimOp (fsLit "unpackInt64X2#")  [] [int64X2PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 32 W8) = mkGenPrimOp (fsLit "unpackInt8X32#")  [] [int8X32PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 16 W16) = mkGenPrimOp (fsLit "unpackInt16X16#")  [] [int16X16PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 8 W32) = mkGenPrimOp (fsLit "unpackInt32X8#")  [] [int32X8PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 4 W64) = mkGenPrimOp (fsLit "unpackInt64X4#")  [] [int64X4PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 64 W8) = mkGenPrimOp (fsLit "unpackInt8X64#")  [] [int8X64PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 32 W16) = mkGenPrimOp (fsLit "unpackInt16X32#")  [] [int16X32PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 16 W32) = mkGenPrimOp (fsLit "unpackInt32X16#")  [] [int32X16PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp IntVec 8 W64) = mkGenPrimOp (fsLit "unpackInt64X8#")  [] [int64X8PrimTy] ((mkTupleTy UnboxedTuple [intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy, intPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 16 W8) = mkGenPrimOp (fsLit "unpackWord8X16#")  [] [word8X16PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 8 W16) = mkGenPrimOp (fsLit "unpackWord16X8#")  [] [word16X8PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 4 W32) = mkGenPrimOp (fsLit "unpackWord32X4#")  [] [word32X4PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 2 W64) = mkGenPrimOp (fsLit "unpackWord64X2#")  [] [word64X2PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 32 W8) = mkGenPrimOp (fsLit "unpackWord8X32#")  [] [word8X32PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 16 W16) = mkGenPrimOp (fsLit "unpackWord16X16#")  [] [word16X16PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 8 W32) = mkGenPrimOp (fsLit "unpackWord32X8#")  [] [word32X8PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 4 W64) = mkGenPrimOp (fsLit "unpackWord64X4#")  [] [word64X4PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 64 W8) = mkGenPrimOp (fsLit "unpackWord8X64#")  [] [word8X64PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 32 W16) = mkGenPrimOp (fsLit "unpackWord16X32#")  [] [word16X32PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 16 W32) = mkGenPrimOp (fsLit "unpackWord32X16#")  [] [word32X16PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp WordVec 8 W64) = mkGenPrimOp (fsLit "unpackWord64X8#")  [] [word64X8PrimTy] ((mkTupleTy UnboxedTuple [wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy, wordPrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 4 W32) = mkGenPrimOp (fsLit "unpackFloatX4#")  [] [floatX4PrimTy] ((mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 2 W64) = mkGenPrimOp (fsLit "unpackDoubleX2#")  [] [doubleX2PrimTy] ((mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 8 W32) = mkGenPrimOp (fsLit "unpackFloatX8#")  [] [floatX8PrimTy] ((mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 4 W64) = mkGenPrimOp (fsLit "unpackDoubleX4#")  [] [doubleX4PrimTy] ((mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 16 W32) = mkGenPrimOp (fsLit "unpackFloatX16#")  [] [floatX16PrimTy] ((mkTupleTy UnboxedTuple [floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy, floatPrimTy]))
-- primOpInfo (VecUnpackOp FloatVec 8 W64) = mkGenPrimOp (fsLit "unpackDoubleX8#")  [] [doubleX8PrimTy] ((mkTupleTy UnboxedTuple [doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy, doublePrimTy]))
-- primOpInfo (VecInsertOp IntVec 16 W8) = mkGenPrimOp (fsLit "insertInt8X16#")  [] [int8X16PrimTy, intPrimTy, intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecInsertOp IntVec 8 W16) = mkGenPrimOp (fsLit "insertInt16X8#")  [] [int16X8PrimTy, intPrimTy, intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecInsertOp IntVec 4 W32) = mkGenPrimOp (fsLit "insertInt32X4#")  [] [int32X4PrimTy, intPrimTy, intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecInsertOp IntVec 2 W64) = mkGenPrimOp (fsLit "insertInt64X2#")  [] [int64X2PrimTy, intPrimTy, intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecInsertOp IntVec 32 W8) = mkGenPrimOp (fsLit "insertInt8X32#")  [] [int8X32PrimTy, intPrimTy, intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecInsertOp IntVec 16 W16) = mkGenPrimOp (fsLit "insertInt16X16#")  [] [int16X16PrimTy, intPrimTy, intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecInsertOp IntVec 8 W32) = mkGenPrimOp (fsLit "insertInt32X8#")  [] [int32X8PrimTy, intPrimTy, intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecInsertOp IntVec 4 W64) = mkGenPrimOp (fsLit "insertInt64X4#")  [] [int64X4PrimTy, intPrimTy, intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecInsertOp IntVec 64 W8) = mkGenPrimOp (fsLit "insertInt8X64#")  [] [int8X64PrimTy, intPrimTy, intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecInsertOp IntVec 32 W16) = mkGenPrimOp (fsLit "insertInt16X32#")  [] [int16X32PrimTy, intPrimTy, intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecInsertOp IntVec 16 W32) = mkGenPrimOp (fsLit "insertInt32X16#")  [] [int32X16PrimTy, intPrimTy, intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecInsertOp IntVec 8 W64) = mkGenPrimOp (fsLit "insertInt64X8#")  [] [int64X8PrimTy, intPrimTy, intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecInsertOp WordVec 16 W8) = mkGenPrimOp (fsLit "insertWord8X16#")  [] [word8X16PrimTy, wordPrimTy, intPrimTy] (word8X16PrimTy)
-- primOpInfo (VecInsertOp WordVec 8 W16) = mkGenPrimOp (fsLit "insertWord16X8#")  [] [word16X8PrimTy, wordPrimTy, intPrimTy] (word16X8PrimTy)
-- primOpInfo (VecInsertOp WordVec 4 W32) = mkGenPrimOp (fsLit "insertWord32X4#")  [] [word32X4PrimTy, wordPrimTy, intPrimTy] (word32X4PrimTy)
-- primOpInfo (VecInsertOp WordVec 2 W64) = mkGenPrimOp (fsLit "insertWord64X2#")  [] [word64X2PrimTy, wordPrimTy, intPrimTy] (word64X2PrimTy)
-- primOpInfo (VecInsertOp WordVec 32 W8) = mkGenPrimOp (fsLit "insertWord8X32#")  [] [word8X32PrimTy, wordPrimTy, intPrimTy] (word8X32PrimTy)
-- primOpInfo (VecInsertOp WordVec 16 W16) = mkGenPrimOp (fsLit "insertWord16X16#")  [] [word16X16PrimTy, wordPrimTy, intPrimTy] (word16X16PrimTy)
-- primOpInfo (VecInsertOp WordVec 8 W32) = mkGenPrimOp (fsLit "insertWord32X8#")  [] [word32X8PrimTy, wordPrimTy, intPrimTy] (word32X8PrimTy)
-- primOpInfo (VecInsertOp WordVec 4 W64) = mkGenPrimOp (fsLit "insertWord64X4#")  [] [word64X4PrimTy, wordPrimTy, intPrimTy] (word64X4PrimTy)
-- primOpInfo (VecInsertOp WordVec 64 W8) = mkGenPrimOp (fsLit "insertWord8X64#")  [] [word8X64PrimTy, wordPrimTy, intPrimTy] (word8X64PrimTy)
-- primOpInfo (VecInsertOp WordVec 32 W16) = mkGenPrimOp (fsLit "insertWord16X32#")  [] [word16X32PrimTy, wordPrimTy, intPrimTy] (word16X32PrimTy)
-- primOpInfo (VecInsertOp WordVec 16 W32) = mkGenPrimOp (fsLit "insertWord32X16#")  [] [word32X16PrimTy, wordPrimTy, intPrimTy] (word32X16PrimTy)
-- primOpInfo (VecInsertOp WordVec 8 W64) = mkGenPrimOp (fsLit "insertWord64X8#")  [] [word64X8PrimTy, wordPrimTy, intPrimTy] (word64X8PrimTy)
-- primOpInfo (VecInsertOp FloatVec 4 W32) = mkGenPrimOp (fsLit "insertFloatX4#")  [] [floatX4PrimTy, floatPrimTy, intPrimTy] (floatX4PrimTy)
-- primOpInfo (VecInsertOp FloatVec 2 W64) = mkGenPrimOp (fsLit "insertDoubleX2#")  [] [doubleX2PrimTy, doublePrimTy, intPrimTy] (doubleX2PrimTy)
-- primOpInfo (VecInsertOp FloatVec 8 W32) = mkGenPrimOp (fsLit "insertFloatX8#")  [] [floatX8PrimTy, floatPrimTy, intPrimTy] (floatX8PrimTy)
-- primOpInfo (VecInsertOp FloatVec 4 W64) = mkGenPrimOp (fsLit "insertDoubleX4#")  [] [doubleX4PrimTy, doublePrimTy, intPrimTy] (doubleX4PrimTy)
-- primOpInfo (VecInsertOp FloatVec 16 W32) = mkGenPrimOp (fsLit "insertFloatX16#")  [] [floatX16PrimTy, floatPrimTy, intPrimTy] (floatX16PrimTy)
-- primOpInfo (VecInsertOp FloatVec 8 W64) = mkGenPrimOp (fsLit "insertDoubleX8#")  [] [doubleX8PrimTy, doublePrimTy, intPrimTy] (doubleX8PrimTy)
-- primOpInfo (VecAddOp IntVec 16 W8) = mkDyadic (fsLit "plusInt8X16#") int8X16PrimTy
-- primOpInfo (VecAddOp IntVec 8 W16) = mkDyadic (fsLit "plusInt16X8#") int16X8PrimTy
-- primOpInfo (VecAddOp IntVec 4 W32) = mkDyadic (fsLit "plusInt32X4#") int32X4PrimTy
-- primOpInfo (VecAddOp IntVec 2 W64) = mkDyadic (fsLit "plusInt64X2#") int64X2PrimTy
-- primOpInfo (VecAddOp IntVec 32 W8) = mkDyadic (fsLit "plusInt8X32#") int8X32PrimTy
-- primOpInfo (VecAddOp IntVec 16 W16) = mkDyadic (fsLit "plusInt16X16#") int16X16PrimTy
-- primOpInfo (VecAddOp IntVec 8 W32) = mkDyadic (fsLit "plusInt32X8#") int32X8PrimTy
-- primOpInfo (VecAddOp IntVec 4 W64) = mkDyadic (fsLit "plusInt64X4#") int64X4PrimTy
-- primOpInfo (VecAddOp IntVec 64 W8) = mkDyadic (fsLit "plusInt8X64#") int8X64PrimTy
-- primOpInfo (VecAddOp IntVec 32 W16) = mkDyadic (fsLit "plusInt16X32#") int16X32PrimTy
-- primOpInfo (VecAddOp IntVec 16 W32) = mkDyadic (fsLit "plusInt32X16#") int32X16PrimTy
-- primOpInfo (VecAddOp IntVec 8 W64) = mkDyadic (fsLit "plusInt64X8#") int64X8PrimTy
-- primOpInfo (VecAddOp WordVec 16 W8) = mkDyadic (fsLit "plusWord8X16#") word8X16PrimTy
-- primOpInfo (VecAddOp WordVec 8 W16) = mkDyadic (fsLit "plusWord16X8#") word16X8PrimTy
-- primOpInfo (VecAddOp WordVec 4 W32) = mkDyadic (fsLit "plusWord32X4#") word32X4PrimTy
-- primOpInfo (VecAddOp WordVec 2 W64) = mkDyadic (fsLit "plusWord64X2#") word64X2PrimTy
-- primOpInfo (VecAddOp WordVec 32 W8) = mkDyadic (fsLit "plusWord8X32#") word8X32PrimTy
-- primOpInfo (VecAddOp WordVec 16 W16) = mkDyadic (fsLit "plusWord16X16#") word16X16PrimTy
-- primOpInfo (VecAddOp WordVec 8 W32) = mkDyadic (fsLit "plusWord32X8#") word32X8PrimTy
-- primOpInfo (VecAddOp WordVec 4 W64) = mkDyadic (fsLit "plusWord64X4#") word64X4PrimTy
-- primOpInfo (VecAddOp WordVec 64 W8) = mkDyadic (fsLit "plusWord8X64#") word8X64PrimTy
-- primOpInfo (VecAddOp WordVec 32 W16) = mkDyadic (fsLit "plusWord16X32#") word16X32PrimTy
-- primOpInfo (VecAddOp WordVec 16 W32) = mkDyadic (fsLit "plusWord32X16#") word32X16PrimTy
-- primOpInfo (VecAddOp WordVec 8 W64) = mkDyadic (fsLit "plusWord64X8#") word64X8PrimTy
-- primOpInfo (VecAddOp FloatVec 4 W32) = mkDyadic (fsLit "plusFloatX4#") floatX4PrimTy
-- primOpInfo (VecAddOp FloatVec 2 W64) = mkDyadic (fsLit "plusDoubleX2#") doubleX2PrimTy
-- primOpInfo (VecAddOp FloatVec 8 W32) = mkDyadic (fsLit "plusFloatX8#") floatX8PrimTy
-- primOpInfo (VecAddOp FloatVec 4 W64) = mkDyadic (fsLit "plusDoubleX4#") doubleX4PrimTy
-- primOpInfo (VecAddOp FloatVec 16 W32) = mkDyadic (fsLit "plusFloatX16#") floatX16PrimTy
-- primOpInfo (VecAddOp FloatVec 8 W64) = mkDyadic (fsLit "plusDoubleX8#") doubleX8PrimTy
-- primOpInfo (VecSubOp IntVec 16 W8) = mkDyadic (fsLit "minusInt8X16#") int8X16PrimTy
-- primOpInfo (VecSubOp IntVec 8 W16) = mkDyadic (fsLit "minusInt16X8#") int16X8PrimTy
-- primOpInfo (VecSubOp IntVec 4 W32) = mkDyadic (fsLit "minusInt32X4#") int32X4PrimTy
-- primOpInfo (VecSubOp IntVec 2 W64) = mkDyadic (fsLit "minusInt64X2#") int64X2PrimTy
-- primOpInfo (VecSubOp IntVec 32 W8) = mkDyadic (fsLit "minusInt8X32#") int8X32PrimTy
-- primOpInfo (VecSubOp IntVec 16 W16) = mkDyadic (fsLit "minusInt16X16#") int16X16PrimTy
-- primOpInfo (VecSubOp IntVec 8 W32) = mkDyadic (fsLit "minusInt32X8#") int32X8PrimTy
-- primOpInfo (VecSubOp IntVec 4 W64) = mkDyadic (fsLit "minusInt64X4#") int64X4PrimTy
-- primOpInfo (VecSubOp IntVec 64 W8) = mkDyadic (fsLit "minusInt8X64#") int8X64PrimTy
-- primOpInfo (VecSubOp IntVec 32 W16) = mkDyadic (fsLit "minusInt16X32#") int16X32PrimTy
-- primOpInfo (VecSubOp IntVec 16 W32) = mkDyadic (fsLit "minusInt32X16#") int32X16PrimTy
-- primOpInfo (VecSubOp IntVec 8 W64) = mkDyadic (fsLit "minusInt64X8#") int64X8PrimTy
-- primOpInfo (VecSubOp WordVec 16 W8) = mkDyadic (fsLit "minusWord8X16#") word8X16PrimTy
-- primOpInfo (VecSubOp WordVec 8 W16) = mkDyadic (fsLit "minusWord16X8#") word16X8PrimTy
-- primOpInfo (VecSubOp WordVec 4 W32) = mkDyadic (fsLit "minusWord32X4#") word32X4PrimTy
-- primOpInfo (VecSubOp WordVec 2 W64) = mkDyadic (fsLit "minusWord64X2#") word64X2PrimTy
-- primOpInfo (VecSubOp WordVec 32 W8) = mkDyadic (fsLit "minusWord8X32#") word8X32PrimTy
-- primOpInfo (VecSubOp WordVec 16 W16) = mkDyadic (fsLit "minusWord16X16#") word16X16PrimTy
-- primOpInfo (VecSubOp WordVec 8 W32) = mkDyadic (fsLit "minusWord32X8#") word32X8PrimTy
-- primOpInfo (VecSubOp WordVec 4 W64) = mkDyadic (fsLit "minusWord64X4#") word64X4PrimTy
-- primOpInfo (VecSubOp WordVec 64 W8) = mkDyadic (fsLit "minusWord8X64#") word8X64PrimTy
-- primOpInfo (VecSubOp WordVec 32 W16) = mkDyadic (fsLit "minusWord16X32#") word16X32PrimTy
-- primOpInfo (VecSubOp WordVec 16 W32) = mkDyadic (fsLit "minusWord32X16#") word32X16PrimTy
-- primOpInfo (VecSubOp WordVec 8 W64) = mkDyadic (fsLit "minusWord64X8#") word64X8PrimTy
-- primOpInfo (VecSubOp FloatVec 4 W32) = mkDyadic (fsLit "minusFloatX4#") floatX4PrimTy
-- primOpInfo (VecSubOp FloatVec 2 W64) = mkDyadic (fsLit "minusDoubleX2#") doubleX2PrimTy
-- primOpInfo (VecSubOp FloatVec 8 W32) = mkDyadic (fsLit "minusFloatX8#") floatX8PrimTy
-- primOpInfo (VecSubOp FloatVec 4 W64) = mkDyadic (fsLit "minusDoubleX4#") doubleX4PrimTy
-- primOpInfo (VecSubOp FloatVec 16 W32) = mkDyadic (fsLit "minusFloatX16#") floatX16PrimTy
-- primOpInfo (VecSubOp FloatVec 8 W64) = mkDyadic (fsLit "minusDoubleX8#") doubleX8PrimTy
-- primOpInfo (VecMulOp IntVec 16 W8) = mkDyadic (fsLit "timesInt8X16#") int8X16PrimTy
-- primOpInfo (VecMulOp IntVec 8 W16) = mkDyadic (fsLit "timesInt16X8#") int16X8PrimTy
-- primOpInfo (VecMulOp IntVec 4 W32) = mkDyadic (fsLit "timesInt32X4#") int32X4PrimTy
-- primOpInfo (VecMulOp IntVec 2 W64) = mkDyadic (fsLit "timesInt64X2#") int64X2PrimTy
-- primOpInfo (VecMulOp IntVec 32 W8) = mkDyadic (fsLit "timesInt8X32#") int8X32PrimTy
-- primOpInfo (VecMulOp IntVec 16 W16) = mkDyadic (fsLit "timesInt16X16#") int16X16PrimTy
-- primOpInfo (VecMulOp IntVec 8 W32) = mkDyadic (fsLit "timesInt32X8#") int32X8PrimTy
-- primOpInfo (VecMulOp IntVec 4 W64) = mkDyadic (fsLit "timesInt64X4#") int64X4PrimTy
-- primOpInfo (VecMulOp IntVec 64 W8) = mkDyadic (fsLit "timesInt8X64#") int8X64PrimTy
-- primOpInfo (VecMulOp IntVec 32 W16) = mkDyadic (fsLit "timesInt16X32#") int16X32PrimTy
-- primOpInfo (VecMulOp IntVec 16 W32) = mkDyadic (fsLit "timesInt32X16#") int32X16PrimTy
-- primOpInfo (VecMulOp IntVec 8 W64) = mkDyadic (fsLit "timesInt64X8#") int64X8PrimTy
-- primOpInfo (VecMulOp WordVec 16 W8) = mkDyadic (fsLit "timesWord8X16#") word8X16PrimTy
-- primOpInfo (VecMulOp WordVec 8 W16) = mkDyadic (fsLit "timesWord16X8#") word16X8PrimTy
-- primOpInfo (VecMulOp WordVec 4 W32) = mkDyadic (fsLit "timesWord32X4#") word32X4PrimTy
-- primOpInfo (VecMulOp WordVec 2 W64) = mkDyadic (fsLit "timesWord64X2#") word64X2PrimTy
-- primOpInfo (VecMulOp WordVec 32 W8) = mkDyadic (fsLit "timesWord8X32#") word8X32PrimTy
-- primOpInfo (VecMulOp WordVec 16 W16) = mkDyadic (fsLit "timesWord16X16#") word16X16PrimTy
-- primOpInfo (VecMulOp WordVec 8 W32) = mkDyadic (fsLit "timesWord32X8#") word32X8PrimTy
-- primOpInfo (VecMulOp WordVec 4 W64) = mkDyadic (fsLit "timesWord64X4#") word64X4PrimTy
-- primOpInfo (VecMulOp WordVec 64 W8) = mkDyadic (fsLit "timesWord8X64#") word8X64PrimTy
-- primOpInfo (VecMulOp WordVec 32 W16) = mkDyadic (fsLit "timesWord16X32#") word16X32PrimTy
-- primOpInfo (VecMulOp WordVec 16 W32) = mkDyadic (fsLit "timesWord32X16#") word32X16PrimTy
-- primOpInfo (VecMulOp WordVec 8 W64) = mkDyadic (fsLit "timesWord64X8#") word64X8PrimTy
-- primOpInfo (VecMulOp FloatVec 4 W32) = mkDyadic (fsLit "timesFloatX4#") floatX4PrimTy
-- primOpInfo (VecMulOp FloatVec 2 W64) = mkDyadic (fsLit "timesDoubleX2#") doubleX2PrimTy
-- primOpInfo (VecMulOp FloatVec 8 W32) = mkDyadic (fsLit "timesFloatX8#") floatX8PrimTy
-- primOpInfo (VecMulOp FloatVec 4 W64) = mkDyadic (fsLit "timesDoubleX4#") doubleX4PrimTy
-- primOpInfo (VecMulOp FloatVec 16 W32) = mkDyadic (fsLit "timesFloatX16#") floatX16PrimTy
-- primOpInfo (VecMulOp FloatVec 8 W64) = mkDyadic (fsLit "timesDoubleX8#") doubleX8PrimTy
-- primOpInfo (VecDivOp FloatVec 4 W32) = mkDyadic (fsLit "divideFloatX4#") floatX4PrimTy
-- primOpInfo (VecDivOp FloatVec 2 W64) = mkDyadic (fsLit "divideDoubleX2#") doubleX2PrimTy
-- primOpInfo (VecDivOp FloatVec 8 W32) = mkDyadic (fsLit "divideFloatX8#") floatX8PrimTy
-- primOpInfo (VecDivOp FloatVec 4 W64) = mkDyadic (fsLit "divideDoubleX4#") doubleX4PrimTy
-- primOpInfo (VecDivOp FloatVec 16 W32) = mkDyadic (fsLit "divideFloatX16#") floatX16PrimTy
-- primOpInfo (VecDivOp FloatVec 8 W64) = mkDyadic (fsLit "divideDoubleX8#") doubleX8PrimTy
-- primOpInfo (VecQuotOp IntVec 16 W8) = mkDyadic (fsLit "quotInt8X16#") int8X16PrimTy
-- primOpInfo (VecQuotOp IntVec 8 W16) = mkDyadic (fsLit "quotInt16X8#") int16X8PrimTy
-- primOpInfo (VecQuotOp IntVec 4 W32) = mkDyadic (fsLit "quotInt32X4#") int32X4PrimTy
-- primOpInfo (VecQuotOp IntVec 2 W64) = mkDyadic (fsLit "quotInt64X2#") int64X2PrimTy
-- primOpInfo (VecQuotOp IntVec 32 W8) = mkDyadic (fsLit "quotInt8X32#") int8X32PrimTy
-- primOpInfo (VecQuotOp IntVec 16 W16) = mkDyadic (fsLit "quotInt16X16#") int16X16PrimTy
-- primOpInfo (VecQuotOp IntVec 8 W32) = mkDyadic (fsLit "quotInt32X8#") int32X8PrimTy
-- primOpInfo (VecQuotOp IntVec 4 W64) = mkDyadic (fsLit "quotInt64X4#") int64X4PrimTy
-- primOpInfo (VecQuotOp IntVec 64 W8) = mkDyadic (fsLit "quotInt8X64#") int8X64PrimTy
-- primOpInfo (VecQuotOp IntVec 32 W16) = mkDyadic (fsLit "quotInt16X32#") int16X32PrimTy
-- primOpInfo (VecQuotOp IntVec 16 W32) = mkDyadic (fsLit "quotInt32X16#") int32X16PrimTy
-- primOpInfo (VecQuotOp IntVec 8 W64) = mkDyadic (fsLit "quotInt64X8#") int64X8PrimTy
-- primOpInfo (VecQuotOp WordVec 16 W8) = mkDyadic (fsLit "quotWord8X16#") word8X16PrimTy
-- primOpInfo (VecQuotOp WordVec 8 W16) = mkDyadic (fsLit "quotWord16X8#") word16X8PrimTy
-- primOpInfo (VecQuotOp WordVec 4 W32) = mkDyadic (fsLit "quotWord32X4#") word32X4PrimTy
-- primOpInfo (VecQuotOp WordVec 2 W64) = mkDyadic (fsLit "quotWord64X2#") word64X2PrimTy
-- primOpInfo (VecQuotOp WordVec 32 W8) = mkDyadic (fsLit "quotWord8X32#") word8X32PrimTy
-- primOpInfo (VecQuotOp WordVec 16 W16) = mkDyadic (fsLit "quotWord16X16#") word16X16PrimTy
-- primOpInfo (VecQuotOp WordVec 8 W32) = mkDyadic (fsLit "quotWord32X8#") word32X8PrimTy
-- primOpInfo (VecQuotOp WordVec 4 W64) = mkDyadic (fsLit "quotWord64X4#") word64X4PrimTy
-- primOpInfo (VecQuotOp WordVec 64 W8) = mkDyadic (fsLit "quotWord8X64#") word8X64PrimTy
-- primOpInfo (VecQuotOp WordVec 32 W16) = mkDyadic (fsLit "quotWord16X32#") word16X32PrimTy
-- primOpInfo (VecQuotOp WordVec 16 W32) = mkDyadic (fsLit "quotWord32X16#") word32X16PrimTy
-- primOpInfo (VecQuotOp WordVec 8 W64) = mkDyadic (fsLit "quotWord64X8#") word64X8PrimTy
-- primOpInfo (VecRemOp IntVec 16 W8) = mkDyadic (fsLit "remInt8X16#") int8X16PrimTy
-- primOpInfo (VecRemOp IntVec 8 W16) = mkDyadic (fsLit "remInt16X8#") int16X8PrimTy
-- primOpInfo (VecRemOp IntVec 4 W32) = mkDyadic (fsLit "remInt32X4#") int32X4PrimTy
-- primOpInfo (VecRemOp IntVec 2 W64) = mkDyadic (fsLit "remInt64X2#") int64X2PrimTy
-- primOpInfo (VecRemOp IntVec 32 W8) = mkDyadic (fsLit "remInt8X32#") int8X32PrimTy
-- primOpInfo (VecRemOp IntVec 16 W16) = mkDyadic (fsLit "remInt16X16#") int16X16PrimTy
-- primOpInfo (VecRemOp IntVec 8 W32) = mkDyadic (fsLit "remInt32X8#") int32X8PrimTy
-- primOpInfo (VecRemOp IntVec 4 W64) = mkDyadic (fsLit "remInt64X4#") int64X4PrimTy
-- primOpInfo (VecRemOp IntVec 64 W8) = mkDyadic (fsLit "remInt8X64#") int8X64PrimTy
-- primOpInfo (VecRemOp IntVec 32 W16) = mkDyadic (fsLit "remInt16X32#") int16X32PrimTy
-- primOpInfo (VecRemOp IntVec 16 W32) = mkDyadic (fsLit "remInt32X16#") int32X16PrimTy
-- primOpInfo (VecRemOp IntVec 8 W64) = mkDyadic (fsLit "remInt64X8#") int64X8PrimTy
-- primOpInfo (VecRemOp WordVec 16 W8) = mkDyadic (fsLit "remWord8X16#") word8X16PrimTy
-- primOpInfo (VecRemOp WordVec 8 W16) = mkDyadic (fsLit "remWord16X8#") word16X8PrimTy
-- primOpInfo (VecRemOp WordVec 4 W32) = mkDyadic (fsLit "remWord32X4#") word32X4PrimTy
-- primOpInfo (VecRemOp WordVec 2 W64) = mkDyadic (fsLit "remWord64X2#") word64X2PrimTy
-- primOpInfo (VecRemOp WordVec 32 W8) = mkDyadic (fsLit "remWord8X32#") word8X32PrimTy
-- primOpInfo (VecRemOp WordVec 16 W16) = mkDyadic (fsLit "remWord16X16#") word16X16PrimTy
-- primOpInfo (VecRemOp WordVec 8 W32) = mkDyadic (fsLit "remWord32X8#") word32X8PrimTy
-- primOpInfo (VecRemOp WordVec 4 W64) = mkDyadic (fsLit "remWord64X4#") word64X4PrimTy
-- primOpInfo (VecRemOp WordVec 64 W8) = mkDyadic (fsLit "remWord8X64#") word8X64PrimTy
-- primOpInfo (VecRemOp WordVec 32 W16) = mkDyadic (fsLit "remWord16X32#") word16X32PrimTy
-- primOpInfo (VecRemOp WordVec 16 W32) = mkDyadic (fsLit "remWord32X16#") word32X16PrimTy
-- primOpInfo (VecRemOp WordVec 8 W64) = mkDyadic (fsLit "remWord64X8#") word64X8PrimTy
-- primOpInfo (VecNegOp IntVec 16 W8) = mkMonadic (fsLit "negateInt8X16#") int8X16PrimTy
-- primOpInfo (VecNegOp IntVec 8 W16) = mkMonadic (fsLit "negateInt16X8#") int16X8PrimTy
-- primOpInfo (VecNegOp IntVec 4 W32) = mkMonadic (fsLit "negateInt32X4#") int32X4PrimTy
-- primOpInfo (VecNegOp IntVec 2 W64) = mkMonadic (fsLit "negateInt64X2#") int64X2PrimTy
-- primOpInfo (VecNegOp IntVec 32 W8) = mkMonadic (fsLit "negateInt8X32#") int8X32PrimTy
-- primOpInfo (VecNegOp IntVec 16 W16) = mkMonadic (fsLit "negateInt16X16#") int16X16PrimTy
-- primOpInfo (VecNegOp IntVec 8 W32) = mkMonadic (fsLit "negateInt32X8#") int32X8PrimTy
-- primOpInfo (VecNegOp IntVec 4 W64) = mkMonadic (fsLit "negateInt64X4#") int64X4PrimTy
-- primOpInfo (VecNegOp IntVec 64 W8) = mkMonadic (fsLit "negateInt8X64#") int8X64PrimTy
-- primOpInfo (VecNegOp IntVec 32 W16) = mkMonadic (fsLit "negateInt16X32#") int16X32PrimTy
-- primOpInfo (VecNegOp IntVec 16 W32) = mkMonadic (fsLit "negateInt32X16#") int32X16PrimTy
-- primOpInfo (VecNegOp IntVec 8 W64) = mkMonadic (fsLit "negateInt64X8#") int64X8PrimTy
-- primOpInfo (VecNegOp FloatVec 4 W32) = mkMonadic (fsLit "negateFloatX4#") floatX4PrimTy
-- primOpInfo (VecNegOp FloatVec 2 W64) = mkMonadic (fsLit "negateDoubleX2#") doubleX2PrimTy
-- primOpInfo (VecNegOp FloatVec 8 W32) = mkMonadic (fsLit "negateFloatX8#") floatX8PrimTy
-- primOpInfo (VecNegOp FloatVec 4 W64) = mkMonadic (fsLit "negateDoubleX4#") doubleX4PrimTy
-- primOpInfo (VecNegOp FloatVec 16 W32) = mkMonadic (fsLit "negateFloatX16#") floatX16PrimTy
-- primOpInfo (VecNegOp FloatVec 8 W64) = mkMonadic (fsLit "negateDoubleX8#") doubleX8PrimTy
-- primOpInfo (VecIndexByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "indexInt8X16Array#")  [] [byteArrayPrimTy, intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "indexInt16X8Array#")  [] [byteArrayPrimTy, intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "indexInt32X4Array#")  [] [byteArrayPrimTy, intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "indexInt64X2Array#")  [] [byteArrayPrimTy, intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "indexInt8X32Array#")  [] [byteArrayPrimTy, intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "indexInt16X16Array#")  [] [byteArrayPrimTy, intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "indexInt32X8Array#")  [] [byteArrayPrimTy, intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "indexInt64X4Array#")  [] [byteArrayPrimTy, intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "indexInt8X64Array#")  [] [byteArrayPrimTy, intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "indexInt16X32Array#")  [] [byteArrayPrimTy, intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "indexInt32X16Array#")  [] [byteArrayPrimTy, intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "indexInt64X8Array#")  [] [byteArrayPrimTy, intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "indexWord8X16Array#")  [] [byteArrayPrimTy, intPrimTy] (word8X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "indexWord16X8Array#")  [] [byteArrayPrimTy, intPrimTy] (word16X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "indexWord32X4Array#")  [] [byteArrayPrimTy, intPrimTy] (word32X4PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "indexWord64X2Array#")  [] [byteArrayPrimTy, intPrimTy] (word64X2PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "indexWord8X32Array#")  [] [byteArrayPrimTy, intPrimTy] (word8X32PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "indexWord16X16Array#")  [] [byteArrayPrimTy, intPrimTy] (word16X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "indexWord32X8Array#")  [] [byteArrayPrimTy, intPrimTy] (word32X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "indexWord64X4Array#")  [] [byteArrayPrimTy, intPrimTy] (word64X4PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "indexWord8X64Array#")  [] [byteArrayPrimTy, intPrimTy] (word8X64PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "indexWord16X32Array#")  [] [byteArrayPrimTy, intPrimTy] (word16X32PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "indexWord32X16Array#")  [] [byteArrayPrimTy, intPrimTy] (word32X16PrimTy)
-- primOpInfo (VecIndexByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "indexWord64X8Array#")  [] [byteArrayPrimTy, intPrimTy] (word64X8PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "indexFloatX4Array#")  [] [byteArrayPrimTy, intPrimTy] (floatX4PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "indexDoubleX2Array#")  [] [byteArrayPrimTy, intPrimTy] (doubleX2PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "indexFloatX8Array#")  [] [byteArrayPrimTy, intPrimTy] (floatX8PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "indexDoubleX4Array#")  [] [byteArrayPrimTy, intPrimTy] (doubleX4PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "indexFloatX16Array#")  [] [byteArrayPrimTy, intPrimTy] (floatX16PrimTy)
-- primOpInfo (VecIndexByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "indexDoubleX8Array#")  [] [byteArrayPrimTy, intPrimTy] (doubleX8PrimTy)
-- primOpInfo (VecReadByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "readInt8X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "readInt16X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "readInt32X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X4PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "readInt64X2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X2PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "readInt8X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X32PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "readInt16X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "readInt32X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "readInt64X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X4PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "readInt8X64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X64PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "readInt16X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X32PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "readInt32X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "readInt64X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "readWord8X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "readWord16X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "readWord32X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X4PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "readWord64X2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X2PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "readWord8X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X32PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "readWord16X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "readWord32X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "readWord64X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X4PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "readWord8X64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X64PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "readWord16X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X32PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "readWord32X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X16PrimTy]))
-- primOpInfo (VecReadByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "readWord64X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X8PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "readFloatX4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX4PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "readDoubleX2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX2PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "readFloatX8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX8PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "readDoubleX4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX4PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "readFloatX16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX16PrimTy]))
-- primOpInfo (VecReadByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "readDoubleX8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX8PrimTy]))
-- primOpInfo (VecWriteByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "writeInt8X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "writeInt16X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "writeInt32X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "writeInt64X2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "writeInt8X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "writeInt16X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "writeInt32X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "writeInt64X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "writeInt8X64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "writeInt16X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "writeInt32X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "writeInt64X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "writeWord8X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "writeWord16X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "writeWord32X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "writeWord64X2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "writeWord8X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "writeWord16X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "writeWord32X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "writeWord64X4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "writeWord8X64Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "writeWord16X32Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "writeWord32X16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "writeWord64X8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "writeFloatX4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "writeDoubleX2Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "writeFloatX8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "writeDoubleX4Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "writeFloatX16Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "writeDoubleX8Array#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "indexInt8X16OffAddr#")  [] [addrPrimTy, intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "indexInt16X8OffAddr#")  [] [addrPrimTy, intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "indexInt32X4OffAddr#")  [] [addrPrimTy, intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "indexInt64X2OffAddr#")  [] [addrPrimTy, intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "indexInt8X32OffAddr#")  [] [addrPrimTy, intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "indexInt16X16OffAddr#")  [] [addrPrimTy, intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "indexInt32X8OffAddr#")  [] [addrPrimTy, intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "indexInt64X4OffAddr#")  [] [addrPrimTy, intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "indexInt8X64OffAddr#")  [] [addrPrimTy, intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "indexInt16X32OffAddr#")  [] [addrPrimTy, intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "indexInt32X16OffAddr#")  [] [addrPrimTy, intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "indexInt64X8OffAddr#")  [] [addrPrimTy, intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "indexWord8X16OffAddr#")  [] [addrPrimTy, intPrimTy] (word8X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "indexWord16X8OffAddr#")  [] [addrPrimTy, intPrimTy] (word16X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "indexWord32X4OffAddr#")  [] [addrPrimTy, intPrimTy] (word32X4PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "indexWord64X2OffAddr#")  [] [addrPrimTy, intPrimTy] (word64X2PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "indexWord8X32OffAddr#")  [] [addrPrimTy, intPrimTy] (word8X32PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "indexWord16X16OffAddr#")  [] [addrPrimTy, intPrimTy] (word16X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "indexWord32X8OffAddr#")  [] [addrPrimTy, intPrimTy] (word32X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "indexWord64X4OffAddr#")  [] [addrPrimTy, intPrimTy] (word64X4PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "indexWord8X64OffAddr#")  [] [addrPrimTy, intPrimTy] (word8X64PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "indexWord16X32OffAddr#")  [] [addrPrimTy, intPrimTy] (word16X32PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "indexWord32X16OffAddr#")  [] [addrPrimTy, intPrimTy] (word32X16PrimTy)
-- primOpInfo (VecIndexOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "indexWord64X8OffAddr#")  [] [addrPrimTy, intPrimTy] (word64X8PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "indexFloatX4OffAddr#")  [] [addrPrimTy, intPrimTy] (floatX4PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "indexDoubleX2OffAddr#")  [] [addrPrimTy, intPrimTy] (doubleX2PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "indexFloatX8OffAddr#")  [] [addrPrimTy, intPrimTy] (floatX8PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "indexDoubleX4OffAddr#")  [] [addrPrimTy, intPrimTy] (doubleX4PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "indexFloatX16OffAddr#")  [] [addrPrimTy, intPrimTy] (floatX16PrimTy)
-- primOpInfo (VecIndexOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "indexDoubleX8OffAddr#")  [] [addrPrimTy, intPrimTy] (doubleX8PrimTy)
-- primOpInfo (VecReadOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "readInt8X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "readInt16X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "readInt32X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X4PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "readInt64X2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X2PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "readInt8X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X32PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "readInt16X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "readInt32X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "readInt64X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X4PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "readInt8X64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X64PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "readInt16X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X32PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "readInt32X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "readInt64X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "readWord8X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "readWord16X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "readWord32X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X4PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "readWord64X2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X2PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "readWord8X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X32PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "readWord16X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "readWord32X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "readWord64X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X4PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "readWord8X64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X64PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "readWord16X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X32PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "readWord32X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X16PrimTy]))
-- primOpInfo (VecReadOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "readWord64X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X8PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "readFloatX4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX4PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "readDoubleX2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX2PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "readFloatX8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX8PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "readDoubleX4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX4PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "readFloatX16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX16PrimTy]))
-- primOpInfo (VecReadOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "readDoubleX8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX8PrimTy]))
-- primOpInfo (VecWriteOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "writeInt8X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "writeInt16X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "writeInt32X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "writeInt64X2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "writeInt8X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "writeInt16X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "writeInt32X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "writeInt64X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "writeInt8X64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "writeInt16X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "writeInt32X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "writeInt64X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "writeWord8X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "writeWord16X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "writeWord32X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "writeWord64X2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "writeWord8X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "writeWord16X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "writeWord32X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "writeWord64X4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "writeWord8X64OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "writeWord16X32OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "writeWord32X16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "writeWord64X8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "writeFloatX4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "writeDoubleX2OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "writeFloatX8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "writeDoubleX4OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "writeFloatX16OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "writeDoubleX8OffAddr#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "indexInt8ArrayAsInt8X16#")  [] [byteArrayPrimTy, intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "indexInt16ArrayAsInt16X8#")  [] [byteArrayPrimTy, intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "indexInt32ArrayAsInt32X4#")  [] [byteArrayPrimTy, intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "indexInt64ArrayAsInt64X2#")  [] [byteArrayPrimTy, intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "indexInt8ArrayAsInt8X32#")  [] [byteArrayPrimTy, intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "indexInt16ArrayAsInt16X16#")  [] [byteArrayPrimTy, intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "indexInt32ArrayAsInt32X8#")  [] [byteArrayPrimTy, intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "indexInt64ArrayAsInt64X4#")  [] [byteArrayPrimTy, intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "indexInt8ArrayAsInt8X64#")  [] [byteArrayPrimTy, intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "indexInt16ArrayAsInt16X32#")  [] [byteArrayPrimTy, intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "indexInt32ArrayAsInt32X16#")  [] [byteArrayPrimTy, intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "indexInt64ArrayAsInt64X8#")  [] [byteArrayPrimTy, intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "indexWord8ArrayAsWord8X16#")  [] [byteArrayPrimTy, intPrimTy] (word8X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "indexWord16ArrayAsWord16X8#")  [] [byteArrayPrimTy, intPrimTy] (word16X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "indexWord32ArrayAsWord32X4#")  [] [byteArrayPrimTy, intPrimTy] (word32X4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "indexWord64ArrayAsWord64X2#")  [] [byteArrayPrimTy, intPrimTy] (word64X2PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "indexWord8ArrayAsWord8X32#")  [] [byteArrayPrimTy, intPrimTy] (word8X32PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "indexWord16ArrayAsWord16X16#")  [] [byteArrayPrimTy, intPrimTy] (word16X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "indexWord32ArrayAsWord32X8#")  [] [byteArrayPrimTy, intPrimTy] (word32X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "indexWord64ArrayAsWord64X4#")  [] [byteArrayPrimTy, intPrimTy] (word64X4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "indexWord8ArrayAsWord8X64#")  [] [byteArrayPrimTy, intPrimTy] (word8X64PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "indexWord16ArrayAsWord16X32#")  [] [byteArrayPrimTy, intPrimTy] (word16X32PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "indexWord32ArrayAsWord32X16#")  [] [byteArrayPrimTy, intPrimTy] (word32X16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "indexWord64ArrayAsWord64X8#")  [] [byteArrayPrimTy, intPrimTy] (word64X8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "indexFloatArrayAsFloatX4#")  [] [byteArrayPrimTy, intPrimTy] (floatX4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "indexDoubleArrayAsDoubleX2#")  [] [byteArrayPrimTy, intPrimTy] (doubleX2PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "indexFloatArrayAsFloatX8#")  [] [byteArrayPrimTy, intPrimTy] (floatX8PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "indexDoubleArrayAsDoubleX4#")  [] [byteArrayPrimTy, intPrimTy] (doubleX4PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "indexFloatArrayAsFloatX16#")  [] [byteArrayPrimTy, intPrimTy] (floatX16PrimTy)
-- primOpInfo (VecIndexScalarByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "indexDoubleArrayAsDoubleX8#")  [] [byteArrayPrimTy, intPrimTy] (doubleX8PrimTy)
-- primOpInfo (VecReadScalarByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "readInt8ArrayAsInt8X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "readInt16ArrayAsInt16X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "readInt32ArrayAsInt32X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "readInt64ArrayAsInt64X2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X2PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "readInt8ArrayAsInt8X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X32PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "readInt16ArrayAsInt16X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "readInt32ArrayAsInt32X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "readInt64ArrayAsInt64X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "readInt8ArrayAsInt8X64#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X64PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "readInt16ArrayAsInt16X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X32PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "readInt32ArrayAsInt32X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "readInt64ArrayAsInt64X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "readWord8ArrayAsWord8X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "readWord16ArrayAsWord16X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "readWord32ArrayAsWord32X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "readWord64ArrayAsWord64X2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X2PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "readWord8ArrayAsWord8X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X32PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "readWord16ArrayAsWord16X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "readWord32ArrayAsWord32X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "readWord64ArrayAsWord64X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "readWord8ArrayAsWord8X64#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X64PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "readWord16ArrayAsWord16X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X32PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "readWord32ArrayAsWord32X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "readWord64ArrayAsWord64X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "readFloatArrayAsFloatX4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "readDoubleArrayAsDoubleX2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX2PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "readFloatArrayAsFloatX8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX8PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "readDoubleArrayAsDoubleX4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX4PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "readFloatArrayAsFloatX16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX16PrimTy]))
-- primOpInfo (VecReadScalarByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "readDoubleArrayAsDoubleX8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX8PrimTy]))
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 16 W8) = mkGenPrimOp (fsLit "writeInt8ArrayAsInt8X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 8 W16) = mkGenPrimOp (fsLit "writeInt16ArrayAsInt16X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 4 W32) = mkGenPrimOp (fsLit "writeInt32ArrayAsInt32X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 2 W64) = mkGenPrimOp (fsLit "writeInt64ArrayAsInt64X2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 32 W8) = mkGenPrimOp (fsLit "writeInt8ArrayAsInt8X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 16 W16) = mkGenPrimOp (fsLit "writeInt16ArrayAsInt16X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 8 W32) = mkGenPrimOp (fsLit "writeInt32ArrayAsInt32X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 4 W64) = mkGenPrimOp (fsLit "writeInt64ArrayAsInt64X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 64 W8) = mkGenPrimOp (fsLit "writeInt8ArrayAsInt8X64#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 32 W16) = mkGenPrimOp (fsLit "writeInt16ArrayAsInt16X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 16 W32) = mkGenPrimOp (fsLit "writeInt32ArrayAsInt32X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp IntVec 8 W64) = mkGenPrimOp (fsLit "writeInt64ArrayAsInt64X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, int64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 16 W8) = mkGenPrimOp (fsLit "writeWord8ArrayAsWord8X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 8 W16) = mkGenPrimOp (fsLit "writeWord16ArrayAsWord16X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 4 W32) = mkGenPrimOp (fsLit "writeWord32ArrayAsWord32X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 2 W64) = mkGenPrimOp (fsLit "writeWord64ArrayAsWord64X2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 32 W8) = mkGenPrimOp (fsLit "writeWord8ArrayAsWord8X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 16 W16) = mkGenPrimOp (fsLit "writeWord16ArrayAsWord16X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 8 W32) = mkGenPrimOp (fsLit "writeWord32ArrayAsWord32X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 4 W64) = mkGenPrimOp (fsLit "writeWord64ArrayAsWord64X4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 64 W8) = mkGenPrimOp (fsLit "writeWord8ArrayAsWord8X64#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 32 W16) = mkGenPrimOp (fsLit "writeWord16ArrayAsWord16X32#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 16 W32) = mkGenPrimOp (fsLit "writeWord32ArrayAsWord32X16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp WordVec 8 W64) = mkGenPrimOp (fsLit "writeWord64ArrayAsWord64X8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, word64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 4 W32) = mkGenPrimOp (fsLit "writeFloatArrayAsFloatX4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 2 W64) = mkGenPrimOp (fsLit "writeDoubleArrayAsDoubleX2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 8 W32) = mkGenPrimOp (fsLit "writeFloatArrayAsFloatX8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 4 W64) = mkGenPrimOp (fsLit "writeDoubleArrayAsDoubleX4#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 16 W32) = mkGenPrimOp (fsLit "writeFloatArrayAsFloatX16#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, floatX16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarByteArrayOp FloatVec 8 W64) = mkGenPrimOp (fsLit "writeDoubleArrayAsDoubleX8#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, doubleX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "indexInt8OffAddrAsInt8X16#")  [] [addrPrimTy, intPrimTy] (int8X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "indexInt16OffAddrAsInt16X8#")  [] [addrPrimTy, intPrimTy] (int16X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "indexInt32OffAddrAsInt32X4#")  [] [addrPrimTy, intPrimTy] (int32X4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "indexInt64OffAddrAsInt64X2#")  [] [addrPrimTy, intPrimTy] (int64X2PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "indexInt8OffAddrAsInt8X32#")  [] [addrPrimTy, intPrimTy] (int8X32PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "indexInt16OffAddrAsInt16X16#")  [] [addrPrimTy, intPrimTy] (int16X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "indexInt32OffAddrAsInt32X8#")  [] [addrPrimTy, intPrimTy] (int32X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "indexInt64OffAddrAsInt64X4#")  [] [addrPrimTy, intPrimTy] (int64X4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "indexInt8OffAddrAsInt8X64#")  [] [addrPrimTy, intPrimTy] (int8X64PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "indexInt16OffAddrAsInt16X32#")  [] [addrPrimTy, intPrimTy] (int16X32PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "indexInt32OffAddrAsInt32X16#")  [] [addrPrimTy, intPrimTy] (int32X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "indexInt64OffAddrAsInt64X8#")  [] [addrPrimTy, intPrimTy] (int64X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "indexWord8OffAddrAsWord8X16#")  [] [addrPrimTy, intPrimTy] (word8X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "indexWord16OffAddrAsWord16X8#")  [] [addrPrimTy, intPrimTy] (word16X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "indexWord32OffAddrAsWord32X4#")  [] [addrPrimTy, intPrimTy] (word32X4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "indexWord64OffAddrAsWord64X2#")  [] [addrPrimTy, intPrimTy] (word64X2PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "indexWord8OffAddrAsWord8X32#")  [] [addrPrimTy, intPrimTy] (word8X32PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "indexWord16OffAddrAsWord16X16#")  [] [addrPrimTy, intPrimTy] (word16X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "indexWord32OffAddrAsWord32X8#")  [] [addrPrimTy, intPrimTy] (word32X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "indexWord64OffAddrAsWord64X4#")  [] [addrPrimTy, intPrimTy] (word64X4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "indexWord8OffAddrAsWord8X64#")  [] [addrPrimTy, intPrimTy] (word8X64PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "indexWord16OffAddrAsWord16X32#")  [] [addrPrimTy, intPrimTy] (word16X32PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "indexWord32OffAddrAsWord32X16#")  [] [addrPrimTy, intPrimTy] (word32X16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "indexWord64OffAddrAsWord64X8#")  [] [addrPrimTy, intPrimTy] (word64X8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "indexFloatOffAddrAsFloatX4#")  [] [addrPrimTy, intPrimTy] (floatX4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "indexDoubleOffAddrAsDoubleX2#")  [] [addrPrimTy, intPrimTy] (doubleX2PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "indexFloatOffAddrAsFloatX8#")  [] [addrPrimTy, intPrimTy] (floatX8PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "indexDoubleOffAddrAsDoubleX4#")  [] [addrPrimTy, intPrimTy] (doubleX4PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "indexFloatOffAddrAsFloatX16#")  [] [addrPrimTy, intPrimTy] (floatX16PrimTy)
-- primOpInfo (VecIndexScalarOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "indexDoubleOffAddrAsDoubleX8#")  [] [addrPrimTy, intPrimTy] (doubleX8PrimTy)
-- primOpInfo (VecReadScalarOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "readInt8OffAddrAsInt8X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "readInt16OffAddrAsInt16X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "readInt32OffAddrAsInt32X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "readInt64OffAddrAsInt64X2#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X2PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "readInt8OffAddrAsInt8X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X32PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "readInt16OffAddrAsInt16X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "readInt32OffAddrAsInt32X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "readInt64OffAddrAsInt64X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "readInt8OffAddrAsInt8X64#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int8X64PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "readInt16OffAddrAsInt16X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int16X32PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "readInt32OffAddrAsInt32X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int32X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "readInt64OffAddrAsInt64X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, int64X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "readWord8OffAddrAsWord8X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "readWord16OffAddrAsWord16X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "readWord32OffAddrAsWord32X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "readWord64OffAddrAsWord64X2#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X2PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "readWord8OffAddrAsWord8X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X32PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "readWord16OffAddrAsWord16X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "readWord32OffAddrAsWord32X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "readWord64OffAddrAsWord64X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "readWord8OffAddrAsWord8X64#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word8X64PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "readWord16OffAddrAsWord16X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word16X32PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "readWord32OffAddrAsWord32X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word32X16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "readWord64OffAddrAsWord64X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, word64X8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "readFloatOffAddrAsFloatX4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "readDoubleOffAddrAsDoubleX2#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX2PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "readFloatOffAddrAsFloatX8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX8PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "readDoubleOffAddrAsDoubleX4#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX4PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "readFloatOffAddrAsFloatX16#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, floatX16PrimTy]))
-- primOpInfo (VecReadScalarOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "readDoubleOffAddrAsDoubleX8#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy UnboxedTuple [mkStatePrimTy deltaTy, doubleX8PrimTy]))
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 16 W8) = mkGenPrimOp (fsLit "writeInt8OffAddrAsInt8X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 8 W16) = mkGenPrimOp (fsLit "writeInt16OffAddrAsInt16X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 4 W32) = mkGenPrimOp (fsLit "writeInt32OffAddrAsInt32X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 2 W64) = mkGenPrimOp (fsLit "writeInt64OffAddrAsInt64X2#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 32 W8) = mkGenPrimOp (fsLit "writeInt8OffAddrAsInt8X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 16 W16) = mkGenPrimOp (fsLit "writeInt16OffAddrAsInt16X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 8 W32) = mkGenPrimOp (fsLit "writeInt32OffAddrAsInt32X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 4 W64) = mkGenPrimOp (fsLit "writeInt64OffAddrAsInt64X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 64 W8) = mkGenPrimOp (fsLit "writeInt8OffAddrAsInt8X64#")  [deltaTyVar] [addrPrimTy, intPrimTy, int8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 32 W16) = mkGenPrimOp (fsLit "writeInt16OffAddrAsInt16X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, int16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 16 W32) = mkGenPrimOp (fsLit "writeInt32OffAddrAsInt32X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, int32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp IntVec 8 W64) = mkGenPrimOp (fsLit "writeInt64OffAddrAsInt64X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, int64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 16 W8) = mkGenPrimOp (fsLit "writeWord8OffAddrAsWord8X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 8 W16) = mkGenPrimOp (fsLit "writeWord16OffAddrAsWord16X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 4 W32) = mkGenPrimOp (fsLit "writeWord32OffAddrAsWord32X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 2 W64) = mkGenPrimOp (fsLit "writeWord64OffAddrAsWord64X2#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 32 W8) = mkGenPrimOp (fsLit "writeWord8OffAddrAsWord8X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 16 W16) = mkGenPrimOp (fsLit "writeWord16OffAddrAsWord16X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 8 W32) = mkGenPrimOp (fsLit "writeWord32OffAddrAsWord32X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 4 W64) = mkGenPrimOp (fsLit "writeWord64OffAddrAsWord64X4#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 64 W8) = mkGenPrimOp (fsLit "writeWord8OffAddrAsWord8X64#")  [deltaTyVar] [addrPrimTy, intPrimTy, word8X64PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 32 W16) = mkGenPrimOp (fsLit "writeWord16OffAddrAsWord16X32#")  [deltaTyVar] [addrPrimTy, intPrimTy, word16X32PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 16 W32) = mkGenPrimOp (fsLit "writeWord32OffAddrAsWord32X16#")  [deltaTyVar] [addrPrimTy, intPrimTy, word32X16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp WordVec 8 W64) = mkGenPrimOp (fsLit "writeWord64OffAddrAsWord64X8#")  [deltaTyVar] [addrPrimTy, intPrimTy, word64X8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 4 W32) = mkGenPrimOp (fsLit "writeFloatOffAddrAsFloatX4#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 2 W64) = mkGenPrimOp (fsLit "writeDoubleOffAddrAsDoubleX2#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX2PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 8 W32) = mkGenPrimOp (fsLit "writeFloatOffAddrAsFloatX8#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 4 W64) = mkGenPrimOp (fsLit "writeDoubleOffAddrAsDoubleX4#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX4PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 16 W32) = mkGenPrimOp (fsLit "writeFloatOffAddrAsFloatX16#")  [deltaTyVar] [addrPrimTy, intPrimTy, floatX16PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
-- primOpInfo (VecWriteScalarOffAddrOp FloatVec 8 W64) = mkGenPrimOp (fsLit "writeDoubleOffAddrAsDoubleX8#")  [deltaTyVar] [addrPrimTy, intPrimTy, doubleX8PrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchByteArrayOp3 = mkGenPrimOp (fsLit "prefetchByteArray3#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchMutableByteArrayOp3 = mkGenPrimOp (fsLit "prefetchMutableByteArray3#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchAddrOp3 = mkGenPrimOp (fsLit "prefetchAddr3#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchValueOp3 = mkGenPrimOp (fsLit "prefetchValue3#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchByteArrayOp2 = mkGenPrimOp (fsLit "prefetchByteArray2#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchMutableByteArrayOp2 = mkGenPrimOp (fsLit "prefetchMutableByteArray2#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchAddrOp2 = mkGenPrimOp (fsLit "prefetchAddr2#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchValueOp2 = mkGenPrimOp (fsLit "prefetchValue2#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchByteArrayOp1 = mkGenPrimOp (fsLit "prefetchByteArray1#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchMutableByteArrayOp1 = mkGenPrimOp (fsLit "prefetchMutableByteArray1#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchAddrOp1 = mkGenPrimOp (fsLit "prefetchAddr1#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchValueOp1 = mkGenPrimOp (fsLit "prefetchValue1#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchByteArrayOp0 = mkGenPrimOp (fsLit "prefetchByteArray0#")  [deltaTyVar] [byteArrayPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchMutableByteArrayOp0 = mkGenPrimOp (fsLit "prefetchMutableByteArray0#")  [deltaTyVar] [mkMutableByteArrayPrimTy deltaTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchAddrOp0 = mkGenPrimOp (fsLit "prefetchAddr0#")  [deltaTyVar] [addrPrimTy, intPrimTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo PrefetchValueOp0 = mkGenPrimOp (fsLit "prefetchValue0#")  [alphaTyVar, deltaTyVar] [alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo Word64Eq            = mkCompare (fsLit "eqWord64#") word64PrimTy
primOpInfo Word64Ne            = mkCompare (fsLit "neWord64#") word64PrimTy
primOpInfo Word64Lt            = mkCompare (fsLit "ltWord64#") word64PrimTy
primOpInfo Word64Le            = mkCompare (fsLit "leWord64#") word64PrimTy
primOpInfo Word64Gt            = mkCompare (fsLit "gtWord64#") word64PrimTy
primOpInfo Word64Ge            = mkCompare (fsLit "geWord64#") word64PrimTy
primOpInfo Word64Quot          = mkDyadic  (fsLit "quotWord64#") word64PrimTy
primOpInfo Word64Rem           = mkDyadic  (fsLit "remWord64#") word64PrimTy
primOpInfo Word64And           = mkDyadic  (fsLit "and64#") word64PrimTy
primOpInfo Word64Or            = mkDyadic  (fsLit "or64#") word64PrimTy
primOpInfo Word64Xor           = mkDyadic  (fsLit "xor64#") word64PrimTy
primOpInfo Word64Not           = mkMonadic (fsLit "not64#") word64PrimTy
primOpInfo Word64SllOp         =
  mkGenPrimOp (fsLit "uncheckedShiftL64#") [] [word64PrimTy, intPrimTy] word64PrimTy
primOpInfo Word64SrlOp         =
  mkGenPrimOp (fsLit "uncheckedShiftRL64#") [] [word64PrimTy, intPrimTy] word64PrimTy
primOpInfo Int64Eq             = mkCompare (fsLit "eqInt64#") int64PrimTy
primOpInfo Int64Ne             = mkCompare (fsLit "neInt64#") int64PrimTy
primOpInfo Int64Lt             = mkCompare (fsLit "ltInt64#") int64PrimTy
primOpInfo Int64Le             = mkCompare (fsLit "leInt64#") int64PrimTy
primOpInfo Int64Gt             = mkCompare (fsLit "gtInt64#") int64PrimTy
primOpInfo Int64Ge             = mkCompare (fsLit "geInt64#") int64PrimTy
primOpInfo Int64Quot           = mkDyadic (fsLit "quotInt64#") int64PrimTy
primOpInfo Int64Rem            = mkDyadic (fsLit "remInt64#") int64PrimTy
primOpInfo Int64Add            = mkDyadic (fsLit "plusInt64#") int64PrimTy
primOpInfo Int64Sub            = mkDyadic (fsLit "minusInt64#") int64PrimTy
primOpInfo Int64Mul            = mkDyadic (fsLit "timesInt64#") int64PrimTy
primOpInfo Int64Neg            = mkMonadic (fsLit "negateInt64#") int64PrimTy
primOpInfo Int64SllOp          =
  mkGenPrimOp (fsLit "uncheckedIShiftL64#") [] [int64PrimTy, intPrimTy] int64PrimTy
primOpInfo Int64SraOp          =
  mkGenPrimOp (fsLit "uncheckedIShiftRA64#") [] [int64PrimTy, intPrimTy] int64PrimTy
primOpInfo Int64SrlOp          =
  mkGenPrimOp (fsLit "uncheckedIShiftRL64#") [] [int64PrimTy, intPrimTy] int64PrimTy
primOpInfo Int642Word64        =
  mkGenPrimOp (fsLit "int64ToWord64#") [] [int64PrimTy] word64PrimTy
primOpInfo Word642Int64        =
  mkGenPrimOp (fsLit "word64ToInt64#") [] [word64PrimTy] int64PrimTy
primOpInfo Int2Int64           =
  mkGenPrimOp (fsLit "intToInt64#") [] [intPrimTy] int64PrimTy
primOpInfo Int642Int           =
  mkGenPrimOp (fsLit "int64ToInt#") [] [int64PrimTy] intPrimTy
primOpInfo Word2Word64         =
  mkGenPrimOp (fsLit "wordToWord64#") [] [wordPrimTy] word64PrimTy
primOpInfo Word64ToWord        =
  mkGenPrimOp (fsLit "word64ToWord#") [] [word64PrimTy] wordPrimTy
primOpInfo DecodeDoubleInteger =
  mkGenPrimOp (fsLit "decodeDoubleUnsafe#") [alphaTyVar] [doublePrimTy]
  $ mkTupleTy UnboxedTuple [intPrimTy, mkObjectPrimTy alphaTy]
  -- HACK: I avoided wiring in the BigInteger tag type by just putting a generic argument,
  --       because BigInteger is slow and there's a good chance we'll be changing the
  --       implementation later. unsafeCoerce# should be used to get the original type
  --       (currently BigInteger)
primOpInfo ObjectArrayAtOp        =
  mkGenPrimOp (fsLit "jobjectArrayAt#") [alphaTyVar, betaTyVar, gammaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, mkObjectPrimTy gammaTy ]
primOpInfo ObjectArraySetOp        =
  mkGenPrimOp (fsLit "jobjectArraySet#") [alphaTyVar, betaTyVar, gammaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkObjectPrimTy gammaTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo IndexJByteArrayOp       =
  mkGenPrimOp (fsLit "indexJByteArray#") [alphaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy] $ jbytePrimTy
primOpInfo ReadJByteArrayOp        =
  mkGenPrimOp (fsLit "readJByteArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, jbytePrimTy]
primOpInfo WriteJByteArrayOp        =
  mkGenPrimOp (fsLit "writeJByteArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, jbytePrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo JByte2CharOp = mkGenPrimOp (fsLit "jbyte2char#")  [] [jbytePrimTy] charPrimTy
primOpInfo JBool2IntOp = mkGenPrimOp (fsLit "jbool2int#")  [] [jboolPrimTy] intPrimTy
primOpInfo StablePtr2AddrOp = mkGenPrimOp (fsLit "stablePtr2Addr#") [alphaTyVar] [mkStablePtrPrimTy alphaTy] addrPrimTy
primOpInfo Addr2StablePtrOp = mkGenPrimOp (fsLit "addr2StablePtr#") [alphaTyVar] [addrPrimTy] (mkStablePtrPrimTy alphaTy)
primOpInfo JByte2IntOp = mkGenPrimOp (fsLit "jbyte2int#")  [] [jbytePrimTy] intPrimTy
primOpInfo Int2JBoolOp = mkGenPrimOp (fsLit "int2jbool#")  [] [intPrimTy] jboolPrimTy
primOpInfo ClassCastOp = mkGenPrimOp (fsLit "classCast#") [alphaTyVar, betaTyVar]
                                     [ mkObjectPrimTy alphaTy ] (mkObjectPrimTy betaTy)
primOpInfo ObjectArrayNewOp =
  mkGenPrimOp (fsLit "jobjectArrayNew#") [alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar]
  [ intPrimTy, mkObjectPrimTy deltaTy, mkStatePrimTy gammaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy gammaTy, mkObjectPrimTy betaTy]
primOpInfo ArrayLengthOp =
  mkGenPrimOp (fsLit "alength#") [alphaTyVar]
  [ mkObjectPrimTy alphaTy ] intPrimTy
primOpInfo IsNullObjectOp = mkGenPrimOp (fsLit "isNullObject#")  [alphaTyVar] [(mkObjectPrimTy alphaTy)] intPrimTy
primOpInfo Int2JByteOp = mkGenPrimOp (fsLit "int2jbyte#")  [] [intPrimTy] jbytePrimTy
primOpInfo JShort2IntOp = mkGenPrimOp (fsLit "jshort2int#")  [] [jshortPrimTy] intPrimTy
primOpInfo Int2JShortOp = mkGenPrimOp (fsLit "int2jshort#")  [] [intPrimTy] jshortPrimTy
primOpInfo JChar2WordOp = mkGenPrimOp (fsLit "jchar2word#")  [] [jcharPrimTy] wordPrimTy
primOpInfo Word2JCharOp = mkGenPrimOp (fsLit "word2jchar#")  [] [wordPrimTy] jcharPrimTy
primOpInfo NewJByteArrayOp =
  mkGenPrimOp (fsLit "newJByteArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo NewJBooleanArrayOp =
  mkGenPrimOp (fsLit "newJBooleanArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJBooleanArrayOp =
  mkGenPrimOp (fsLit "readJBooleanArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, intPrimTy]
primOpInfo WriteJBooleanArrayOp =
  mkGenPrimOp (fsLit "writeJBooleanArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJCharArrayOp =
  mkGenPrimOp (fsLit "newJCharArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJCharArrayOp =
  mkGenPrimOp (fsLit "readJCharArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, jcharPrimTy]
primOpInfo WriteJCharArrayOp =
  mkGenPrimOp (fsLit "writeJCharArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, jcharPrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJShortArrayOp =
  mkGenPrimOp (fsLit "newJShortArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJShortArrayOp =
  mkGenPrimOp (fsLit "readJShortArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, jshortPrimTy]
primOpInfo WriteJShortArrayOp =
  mkGenPrimOp (fsLit "writeJShortArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, jshortPrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJIntArrayOp =
  mkGenPrimOp (fsLit "newJIntArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJIntArrayOp =
  mkGenPrimOp (fsLit "readJIntArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, intPrimTy]
primOpInfo WriteJIntArrayOp =
  mkGenPrimOp (fsLit "writeJIntArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJLongArrayOp =
  mkGenPrimOp (fsLit "newJLongArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJLongArrayOp =
  mkGenPrimOp (fsLit "readJLongArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, int64PrimTy]
primOpInfo WriteJLongArrayOp =
  mkGenPrimOp (fsLit "writeJLongArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, int64PrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJFloatArrayOp =
  mkGenPrimOp (fsLit "newJFloatArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJFloatArrayOp =
  mkGenPrimOp (fsLit "readJFloatArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, floatPrimTy]
primOpInfo WriteJFloatArrayOp =
  mkGenPrimOp (fsLit "writeJFloatArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, floatPrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo NewJDoubleArrayOp =
  mkGenPrimOp (fsLit "newJDoubleArray#") [alphaTyVar, betaTyVar]
  [ intPrimTy, mkStatePrimTy alphaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy alphaTy, mkObjectPrimTy betaTy]
primOpInfo ReadJDoubleArrayOp =
  mkGenPrimOp (fsLit "readJDoubleArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, mkStatePrimTy betaTy ]
  $ mkTupleTy UnboxedTuple [mkStatePrimTy betaTy, doublePrimTy]
primOpInfo WriteJDoubleArrayOp =
  mkGenPrimOp (fsLit "writeJDoubleArray#") [alphaTyVar, betaTyVar]
  [ mkObjectPrimTy alphaTy, intPrimTy, doublePrimTy, mkStatePrimTy betaTy ]
  $ mkStatePrimTy betaTy
primOpInfo Addr2Int64Op = mkGenPrimOp (fsLit "addr2Int64#")  [] [addrPrimTy] (int64PrimTy)
primOpInfo Int642AddrOp = mkGenPrimOp (fsLit "int642Addr#")  [] [int64PrimTy] (addrPrimTy)
primOpInfo WaitConnectOp = mkGenPrimOp (fsLit "waitConnect#")  [alphaTyVar, deltaTyVar] [mkObjectPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo WaitAcceptOp = mkGenPrimOp (fsLit "waitAccept#")  [alphaTyVar, deltaTyVar] [mkObjectPrimTy alphaTy, mkStatePrimTy deltaTy] (mkStatePrimTy deltaTy)
primOpInfo FreshStateTokenOp = mkGenPrimOp (fsLit "freshStateToken#") [openAlphaTyVar, betaTyVar] [openAlphaTy] (mkStatePrimTy betaTy)
primOpInfo FreshObjectTokenOp = mkGenPrimOp (fsLit "freshObjectToken#") [openAlphaTyVar, betaTyVar] [openAlphaTy, mkObjectPrimTy betaTy] (mkObjectPrimTy betaTy)
primOpInfo FreshNullObjectTokenOp = mkGenPrimOp (fsLit "freshNullObjectToken#") [openAlphaTyVar, betaTyVar] [openAlphaTy] (mkObjectPrimTy betaTy)
primOpInfo FloatFabsOp = mkMonadic (fsLit "fabsFloat#") floatPrimTy
primOpInfo DoubleFabsOp = mkMonadic (fsLit "fabsDouble#") doublePrimTy

{-
Here are a load of comments from the old primOp info:

A @Word#@ is an unsigned @Int#@.

@decodeFloat#@ is given w/ Integer-stuff (it's similar).

@decodeDouble#@ is given w/ Integer-stuff (it's similar).

Decoding of floating-point numbers is sorta Integer-related.  Encoding
is done with plain ccalls now (see PrelNumExtra.lhs).

A @Weak@ Pointer is created by the @mkWeak#@ primitive:

        mkWeak# :: k -> v -> f -> State# RealWorld
                        -> (# State# RealWorld, Weak# v #)

In practice, you'll use the higher-level

        data Weak v = Weak# v
        mkWeak :: k -> v -> IO () -> IO (Weak v)

The following operation dereferences a weak pointer.  The weak pointer
may have been finalized, so the operation returns a result code which
must be inspected before looking at the dereferenced value.

        deRefWeak# :: Weak# v -> State# RealWorld ->
                        (# State# RealWorld, v, Int# #)

Only look at v if the Int# returned is /= 0 !!

The higher-level op is

        deRefWeak :: Weak v -> IO (Maybe v)

Weak pointers can be finalized early by using the finalize# operation:

        finalizeWeak# :: Weak# v -> State# RealWorld ->
                           (# State# RealWorld, Int#, IO () #)

The Int# returned is either

        0 if the weak pointer has already been finalized, or it has no
          finalizer (the third component is then invalid).

        1 if the weak pointer is still alive, with the finalizer returned
          as the third component.

A {\em stable name/pointer} is an index into a table of stable name
entries.  Since the garbage collector is told about stable pointers,
it is safe to pass a stable pointer to external systems such as C
routines.

\begin{verbatim}
makeStablePtr#  :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
freeStablePtr   :: StablePtr# a -> State# RealWorld -> State# RealWorld
deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
eqStablePtr#    :: StablePtr# a -> StablePtr# a -> Int#
\end{verbatim}

It may seem a bit surprising that @makeStablePtr#@ is a @IO@
operation since it doesn't (directly) involve IO operations.  The
reason is that if some optimisation pass decided to duplicate calls to
@makeStablePtr#@ and we only pass one of the stable pointers over, a
massive space leak can result.  Putting it into the IO monad
prevents this.  (Another reason for putting them in a monad is to
ensure correct sequencing wrt the side-effecting @freeStablePtr@
operation.)

An important property of stable pointers is that if you call
makeStablePtr# twice on the same object you get the same stable
pointer back.

Note that we can implement @freeStablePtr#@ using @_ccall_@ (and,
besides, it's not likely to be used from Haskell) so it's not a
primop.

Question: Why @RealWorld@ - won't any instance of @_ST@ do the job? [ADR]

Stable Names
~~~~~~~~~~~~

A stable name is like a stable pointer, but with three important differences:

        (a) You can't deRef one to get back to the original object.
        (b) You can convert one to an Int.
        (c) You don't need to 'freeStableName'

The existence of a stable name doesn't guarantee to keep the object it
points to alive (unlike a stable pointer), hence (a).

Invariants:

        (a) makeStableName always returns the same value for a given
            object (same as stable pointers).

        (b) if two stable names are equal, it implies that the objects
            from which they were created were the same.

        (c) stableNameToInt always returns the same Int for a given
            stable name.


-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

These primops are pretty weird.

        dataToTag# :: a -> Int    (arg must be an evaluated data type)
        tagToEnum# :: Int -> a    (result type must be an enumerated type)

The constraints aren't currently checked by the front end, but the
code generator will fall over if they aren't satisfied.

************************************************************************
*                                                                      *
            Which PrimOps are out-of-line
*                                                                      *
************************************************************************

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.
-}

primOpOutOfLine :: PrimOp -> Bool
primOpOutOfLine DoubleDecode_2IntOp = True
primOpOutOfLine DoubleDecode_Int64Op = True
primOpOutOfLine FloatDecode_IntOp = True
primOpOutOfLine NewArrayOp = True
primOpOutOfLine UnsafeThawArrayOp = True
primOpOutOfLine CopyArrayOp = True
primOpOutOfLine CopyMutableArrayOp = True
primOpOutOfLine CloneArrayOp = True
primOpOutOfLine CloneMutableArrayOp = True
primOpOutOfLine FreezeArrayOp = True
primOpOutOfLine ThawArrayOp = True
primOpOutOfLine CasArrayOp = True
primOpOutOfLine NewSmallArrayOp = True
primOpOutOfLine UnsafeThawSmallArrayOp = True
primOpOutOfLine CopySmallArrayOp = True
primOpOutOfLine CopySmallMutableArrayOp = True
primOpOutOfLine CloneSmallArrayOp = True
primOpOutOfLine CloneSmallMutableArrayOp = True
primOpOutOfLine FreezeSmallArrayOp = True
primOpOutOfLine ThawSmallArrayOp = True
primOpOutOfLine CasSmallArrayOp = True
primOpOutOfLine NewByteArrayOp_Char = True
primOpOutOfLine NewPinnedByteArrayOp_Char = True
primOpOutOfLine NewAlignedPinnedByteArrayOp_Char = True
primOpOutOfLine ShrinkMutableByteArrayOp_Char = True
primOpOutOfLine ResizeMutableByteArrayOp_Char = True
primOpOutOfLine NewArrayArrayOp = True
primOpOutOfLine CopyArrayArrayOp = True
primOpOutOfLine CopyMutableArrayArrayOp = True
primOpOutOfLine AtomicModifyMutVarOp = True
primOpOutOfLine CasMutVarOp = True
primOpOutOfLine CatchOp = True
primOpOutOfLine RaiseOp = True
primOpOutOfLine RaiseIOOp = True
primOpOutOfLine MaskAsyncExceptionsOp = True
primOpOutOfLine MaskUninterruptibleOp = True
primOpOutOfLine UnmaskAsyncExceptionsOp = True
primOpOutOfLine MaskStatus = True
primOpOutOfLine AtomicallyOp = True
primOpOutOfLine RetryOp = True
primOpOutOfLine CatchRetryOp = True
primOpOutOfLine CatchSTMOp = True
primOpOutOfLine Check = True
primOpOutOfLine ReadTVarOp = True
primOpOutOfLine ReadTVarIOOp = True
primOpOutOfLine WriteTVarOp = True
primOpOutOfLine NewMVarOp = True
primOpOutOfLine TakeMVarOp = True
primOpOutOfLine TryTakeMVarOp = True
primOpOutOfLine PutMVarOp = True
primOpOutOfLine TryPutMVarOp = True
primOpOutOfLine ReadMVarOp = True
primOpOutOfLine TryReadMVarOp = True
primOpOutOfLine IsEmptyMVarOp = True
primOpOutOfLine DelayOp = True
primOpOutOfLine WaitReadOp = True
primOpOutOfLine WaitWriteOp = True
primOpOutOfLine WaitConnectOp = True
primOpOutOfLine WaitAcceptOp = True
primOpOutOfLine ForkOp = True
primOpOutOfLine ForkOnOp = True
primOpOutOfLine KillThreadOp = True
primOpOutOfLine YieldOp = True
primOpOutOfLine LabelThreadOp = True
primOpOutOfLine IsCurrentThreadBoundOp = True
primOpOutOfLine NoDuplicateOp = True
primOpOutOfLine ThreadStatusOp = True
primOpOutOfLine MkWeakOp = True
primOpOutOfLine MkWeakNoFinalizerOp = True
primOpOutOfLine AddCFinalizerToWeakOp = True
primOpOutOfLine DeRefWeakOp = True
primOpOutOfLine FinalizeWeakOp = True
primOpOutOfLine MakeStablePtrOp = True
primOpOutOfLine DeRefStablePtrOp = True
primOpOutOfLine MakeStableNameOp = True
primOpOutOfLine GetSparkOp = True
primOpOutOfLine NumSparks = True
primOpOutOfLine MkApUpd0_Op = True
primOpOutOfLine NewBCOOp = True
primOpOutOfLine UnpackClosureOp = True
primOpOutOfLine GetApStackValOp = True
primOpOutOfLine TraceEventOp = True
primOpOutOfLine TraceMarkerOp = True
primOpOutOfLine _ = False

{-
************************************************************************
*                                                                      *
            Failure and side effects
*                                                                      *
************************************************************************

Note [PrimOp can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both can_fail and has_side_effects mean that the primop has
some effect that is not captured entirely by its result value.

----------  has_side_effects ---------------------
A primop "has_side_effects" if it has some *write* effect, visible
elsewhere
    - writing to the world (I/O)
    - writing to a mutable data structure (writeIORef)
    - throwing a synchronous Haskell exception

Often such primops have a type like
   State -> input -> (State, output)
so the state token guarantees ordering.  In general we rely *only* on
data dependencies of the state token to enforce write-effect ordering

 * NB1: if you inline unsafePerformIO, you may end up with
   side-effecting ops whose 'state' output is discarded.
   And programmers may do that by hand; see Trac #9390.
   That is why we (conservatively) do not discard write-effecting
   primops even if both their state and result is discarded.

 * NB2: We consider primops, such as raiseIO#, that can raise a
   (Haskell) synchronous exception to "have_side_effects" but not
   "can_fail".  We must be careful about not discarding such things;
   see the paper "A semantics for imprecise exceptions".

 * NB3: *Read* effects (like reading an IORef) don't count here,
   because it doesn't matter if we don't do them, or do them more than
   once.  *Sequencing* is maintained by the data dependency of the state
   token.

----------  can_fail ----------------------------
A primop "can_fail" if it can fail with an *unchecked* exception on
some elements of its input domain. Main examples:
   division (fails on zero demoninator)
   array indexing (fails if the index is out of bounds)

An "unchecked exception" is one that is an outright error, (not
turned into a Haskell exception,) such as seg-fault or
divide-by-zero error.  Such can_fail primops are ALWAYS surrounded
with a test that checks for the bad cases, but we need to be
very careful about code motion that might move it out of
the scope of the test.

Note [Transformations affected by can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The can_fail and has_side_effects properties have the following effect
on program transformations.  Summary table is followed by details.

            can_fail     has_side_effects
Discard        NO            NO
Float in       YES           YES
Float out      NO            NO
Duplicate      YES           NO

* Discarding.   case (a `op` b) of _ -> rhs  ===>   rhs
  You should not discard a has_side_effects primop; e.g.
     case (writeIntArray# a i v s of (# _, _ #) -> True
  Arguably you should be able to discard this, since the
  returned stat token is not used, but that relies on NEVER
  inlining unsafePerformIO, and programmers sometimes write
  this kind of stuff by hand (Trac #9390).  So we (conservatively)
  never discard a has_side_effects primop.

  However, it's fine to discard a can_fail primop.  For example
     case (indexIntArray# a i) of _ -> True
  We can discard indexIntArray#; it has can_fail, but not
  has_side_effects; see Trac #5658 which was all about this.
  Notice that indexIntArray# is (in a more general handling of
  effects) read effect, but we don't care about that here, and
  treat read effects as *not* has_side_effects.

  Similarly (a `/#` b) can be discarded.  It can seg-fault or
  cause a hardware exception, but not a synchronous Haskell
  exception.



  Synchronous Haskell exceptions, e.g. from raiseIO#, are treated
  as has_side_effects and hence are not discarded.

* Float in.  You can float a can_fail or has_side_effects primop
  *inwards*, but not inside a lambda (see Duplication below).

* Float out.  You must not float a can_fail primop *outwards* lest
  you escape the dynamic scope of the test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
  Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0

  Nor can you float out a has_side_effects primop.  For example:
       if blah then case writeMutVar# v True s0 of (# s1 #) -> s1
               else s0
  Notice that s0 is mentioned in both branches of the 'if', but
  only one of these two will actually be consumed.  But if we
  float out to
      case writeMutVar# v True s0 of (# s1 #) ->
      if blah then s1 else s0
  the writeMutVar will be performed in both branches, which is
  utterly wrong.

* Duplication.  You cannot duplicate a has_side_effect primop.  You
  might wonder how this can occur given the state token threading, but
  just look at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get
  something like this
        p = case readMutVar# s v of
              (# s', r #) -> (S# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  Trac #3207 is real example of this happening.

  However, it's fine to duplicate a can_fail primop.  That is really
  the only difference between can_fail and has_side_effects.

Note [Implementation: how can_fail/has_side_effects affect transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that that floating/duplication/discarding are done right
in the simplifier?

Two main predicates on primpops test these flags:
  primOpOkForSideEffects <=> not has_side_effects
  primOpOkForSpeculation <=> not (has_side_effects || can_fail)

  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a can_fail or has_side_effects primop.  The RHS of a
    let-binding (which can float in and out freely) satisfies
    exprOkForSpeculation; this is the let/app invariant.  And
    exprOkForSpeculation is false of can_fail and has_side_effects.

  * So can_fail and has_side_effects primops will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * The no-duplicate thing is done via primOpIsCheap, by making
    has_side_effects things (very very very) not-cheap!
-}

primOpHasSideEffects :: PrimOp -> Bool
primOpHasSideEffects NewArrayOp = True
primOpHasSideEffects ReadArrayOp = True
primOpHasSideEffects WriteArrayOp = True
primOpHasSideEffects UnsafeFreezeArrayOp = True
primOpHasSideEffects UnsafeThawArrayOp = True
primOpHasSideEffects CopyArrayOp = True
primOpHasSideEffects CopyMutableArrayOp = True
primOpHasSideEffects CloneArrayOp = True
primOpHasSideEffects CloneMutableArrayOp = True
primOpHasSideEffects FreezeArrayOp = True
primOpHasSideEffects ThawArrayOp = True
primOpHasSideEffects CasArrayOp = True
primOpHasSideEffects NewSmallArrayOp = True
primOpHasSideEffects ReadSmallArrayOp = True
primOpHasSideEffects WriteSmallArrayOp = True
primOpHasSideEffects UnsafeFreezeSmallArrayOp = True
primOpHasSideEffects UnsafeThawSmallArrayOp = True
primOpHasSideEffects CopySmallArrayOp = True
primOpHasSideEffects CopySmallMutableArrayOp = True
primOpHasSideEffects CloneSmallArrayOp = True
primOpHasSideEffects CloneSmallMutableArrayOp = True
primOpHasSideEffects FreezeSmallArrayOp = True
primOpHasSideEffects ThawSmallArrayOp = True
primOpHasSideEffects CasSmallArrayOp = True
primOpHasSideEffects NewByteArrayOp_Char = True
primOpHasSideEffects NewPinnedByteArrayOp_Char = True
primOpHasSideEffects NewAlignedPinnedByteArrayOp_Char = True
primOpHasSideEffects ShrinkMutableByteArrayOp_Char = True
primOpHasSideEffects ResizeMutableByteArrayOp_Char = True
primOpHasSideEffects UnsafeFreezeByteArrayOp = True
primOpHasSideEffects ReadByteArrayOp_Char = True
primOpHasSideEffects ReadByteArrayOp_WideChar = True
primOpHasSideEffects ReadByteArrayOp_Int = True
primOpHasSideEffects ReadByteArrayOp_Word = True
primOpHasSideEffects ReadByteArrayOp_Addr = True
primOpHasSideEffects ReadByteArrayOp_Float = True
primOpHasSideEffects ReadByteArrayOp_Double = True
primOpHasSideEffects ReadByteArrayOp_StablePtr = True
primOpHasSideEffects ReadByteArrayOp_Int8 = True
primOpHasSideEffects ReadByteArrayOp_Int16 = True
primOpHasSideEffects ReadByteArrayOp_Int32 = True
primOpHasSideEffects ReadByteArrayOp_Int64 = True
primOpHasSideEffects ReadByteArrayOp_Word8 = True
primOpHasSideEffects ReadByteArrayOp_Word16 = True
primOpHasSideEffects ReadByteArrayOp_Word32 = True
primOpHasSideEffects ReadByteArrayOp_Word64 = True
primOpHasSideEffects WriteByteArrayOp_Char = True
primOpHasSideEffects WriteByteArrayOp_WideChar = True
primOpHasSideEffects WriteByteArrayOp_Int = True
primOpHasSideEffects WriteByteArrayOp_Word = True
primOpHasSideEffects WriteByteArrayOp_Addr = True
primOpHasSideEffects WriteByteArrayOp_Float = True
primOpHasSideEffects WriteByteArrayOp_Double = True
primOpHasSideEffects WriteByteArrayOp_StablePtr = True
primOpHasSideEffects WriteByteArrayOp_Int8 = True
primOpHasSideEffects WriteByteArrayOp_Int16 = True
primOpHasSideEffects WriteByteArrayOp_Int32 = True
primOpHasSideEffects WriteByteArrayOp_Int64 = True
primOpHasSideEffects WriteByteArrayOp_Word8 = True
primOpHasSideEffects WriteByteArrayOp_Word16 = True
primOpHasSideEffects WriteByteArrayOp_Word32 = True
primOpHasSideEffects WriteByteArrayOp_Word64 = True
primOpHasSideEffects CopyByteArrayOp = True
primOpHasSideEffects CopyMutableByteArrayOp = True
primOpHasSideEffects CopyByteArrayToAddrOp = True
primOpHasSideEffects CopyMutableByteArrayToAddrOp = True
primOpHasSideEffects CopyAddrToByteArrayOp = True
primOpHasSideEffects SetByteArrayOp = True
primOpHasSideEffects AtomicReadByteArrayOp_Int = True
primOpHasSideEffects AtomicWriteByteArrayOp_Int = True
primOpHasSideEffects CasByteArrayOp_Int = True
primOpHasSideEffects FetchAddByteArrayOp_Int = True
primOpHasSideEffects FetchSubByteArrayOp_Int = True
primOpHasSideEffects FetchAndByteArrayOp_Int = True
primOpHasSideEffects FetchNandByteArrayOp_Int = True
primOpHasSideEffects FetchOrByteArrayOp_Int = True
primOpHasSideEffects FetchXorByteArrayOp_Int = True
primOpHasSideEffects NewArrayArrayOp = True
primOpHasSideEffects UnsafeFreezeArrayArrayOp = True
primOpHasSideEffects ReadArrayArrayOp_ByteArray = True
primOpHasSideEffects ReadArrayArrayOp_MutableByteArray = True
primOpHasSideEffects ReadArrayArrayOp_ArrayArray = True
primOpHasSideEffects ReadArrayArrayOp_MutableArrayArray = True
primOpHasSideEffects WriteArrayArrayOp_ByteArray = True
primOpHasSideEffects WriteArrayArrayOp_MutableByteArray = True
primOpHasSideEffects WriteArrayArrayOp_ArrayArray = True
primOpHasSideEffects WriteArrayArrayOp_MutableArrayArray = True
primOpHasSideEffects CopyArrayArrayOp = True
primOpHasSideEffects CopyMutableArrayArrayOp = True
primOpHasSideEffects ReadOffAddrOp_Char = True
primOpHasSideEffects ReadOffAddrOp_WideChar = True
primOpHasSideEffects ReadOffAddrOp_Int = True
primOpHasSideEffects ReadOffAddrOp_Word = True
primOpHasSideEffects ReadOffAddrOp_Addr = True
primOpHasSideEffects ReadOffAddrOp_Float = True
primOpHasSideEffects ReadOffAddrOp_Double = True
primOpHasSideEffects ReadOffAddrOp_StablePtr = True
primOpHasSideEffects ReadOffAddrOp_Int8 = True
primOpHasSideEffects ReadOffAddrOp_Int16 = True
primOpHasSideEffects ReadOffAddrOp_Int32 = True
primOpHasSideEffects ReadOffAddrOp_Int64 = True
primOpHasSideEffects ReadOffAddrOp_Word8 = True
primOpHasSideEffects ReadOffAddrOp_Word16 = True
primOpHasSideEffects ReadOffAddrOp_Word32 = True
primOpHasSideEffects ReadOffAddrOp_Word64 = True
primOpHasSideEffects WriteOffAddrOp_Char = True
primOpHasSideEffects WriteOffAddrOp_WideChar = True
primOpHasSideEffects WriteOffAddrOp_Int = True
primOpHasSideEffects WriteOffAddrOp_Word = True
primOpHasSideEffects WriteOffAddrOp_Addr = True
primOpHasSideEffects WriteOffAddrOp_Float = True
primOpHasSideEffects WriteOffAddrOp_Double = True
primOpHasSideEffects WriteOffAddrOp_StablePtr = True
primOpHasSideEffects WriteOffAddrOp_Int8 = True
primOpHasSideEffects WriteOffAddrOp_Int16 = True
primOpHasSideEffects WriteOffAddrOp_Int32 = True
primOpHasSideEffects WriteOffAddrOp_Int64 = True
primOpHasSideEffects WriteOffAddrOp_Word8 = True
primOpHasSideEffects WriteOffAddrOp_Word16 = True
primOpHasSideEffects WriteOffAddrOp_Word32 = True
primOpHasSideEffects WriteOffAddrOp_Word64 = True
primOpHasSideEffects NewMutVarOp = True
primOpHasSideEffects ReadMutVarOp = True
primOpHasSideEffects WriteMutVarOp = True
primOpHasSideEffects AtomicModifyMutVarOp = True
primOpHasSideEffects CasMutVarOp = True
primOpHasSideEffects CatchOp = True
primOpHasSideEffects RaiseOp = True
primOpHasSideEffects RaiseIOOp = True
primOpHasSideEffects MaskAsyncExceptionsOp = True
primOpHasSideEffects MaskUninterruptibleOp = True
primOpHasSideEffects UnmaskAsyncExceptionsOp = True
primOpHasSideEffects MaskStatus = True
primOpHasSideEffects AtomicallyOp = True
primOpHasSideEffects RetryOp = True
primOpHasSideEffects CatchRetryOp = True
primOpHasSideEffects CatchSTMOp = True
primOpHasSideEffects Check = True
primOpHasSideEffects NewTVarOp = True
primOpHasSideEffects ReadTVarOp = True
primOpHasSideEffects ReadTVarIOOp = True
primOpHasSideEffects WriteTVarOp = True
primOpHasSideEffects NewMVarOp = True
primOpHasSideEffects TakeMVarOp = True
primOpHasSideEffects TryTakeMVarOp = True
primOpHasSideEffects PutMVarOp = True
primOpHasSideEffects TryPutMVarOp = True
primOpHasSideEffects ReadMVarOp = True
primOpHasSideEffects TryReadMVarOp = True
primOpHasSideEffects IsEmptyMVarOp = True
primOpHasSideEffects DelayOp = True
primOpHasSideEffects WaitReadOp = True
primOpHasSideEffects WaitWriteOp = True
primOpHasSideEffects WaitConnectOp = True
primOpHasSideEffects WaitAcceptOp = True
primOpHasSideEffects ForkOp = True
primOpHasSideEffects ForkOnOp = True
primOpHasSideEffects KillThreadOp = True
primOpHasSideEffects YieldOp = True
primOpHasSideEffects MyThreadIdOp = True
primOpHasSideEffects LabelThreadOp = True
primOpHasSideEffects IsCurrentThreadBoundOp = True
primOpHasSideEffects NoDuplicateOp = True
primOpHasSideEffects ThreadStatusOp = True
primOpHasSideEffects MkWeakOp = True
primOpHasSideEffects MkWeakNoFinalizerOp = True
primOpHasSideEffects AddCFinalizerToWeakOp = True
primOpHasSideEffects DeRefWeakOp = True
primOpHasSideEffects FinalizeWeakOp = True
primOpHasSideEffects TouchOp = True
primOpHasSideEffects MakeStablePtrOp = True
primOpHasSideEffects DeRefStablePtrOp = True
primOpHasSideEffects EqStablePtrOp = True
primOpHasSideEffects MakeStableNameOp = True
primOpHasSideEffects ParOp = True
primOpHasSideEffects SparkOp = True
primOpHasSideEffects GetSparkOp = True
primOpHasSideEffects NumSparks = True
primOpHasSideEffects ParGlobalOp = True
primOpHasSideEffects ParLocalOp = True
primOpHasSideEffects ParAtOp = True
primOpHasSideEffects ParAtAbsOp = True
primOpHasSideEffects ParAtRelOp = True
primOpHasSideEffects ParAtForNowOp = True
primOpHasSideEffects NewBCOOp = True
primOpHasSideEffects TraceEventOp = True
primOpHasSideEffects TraceMarkerOp = True
-- primOpHasSideEffects (VecReadByteArrayOp _ _ _) = True
-- primOpHasSideEffects (VecWriteByteArrayOp _ _ _) = True
-- primOpHasSideEffects (VecReadOffAddrOp _ _ _) = True
-- primOpHasSideEffects (VecWriteOffAddrOp _ _ _) = True
-- primOpHasSideEffects (VecReadScalarByteArrayOp _ _ _) = True
-- primOpHasSideEffects (VecWriteScalarByteArrayOp _ _ _) = True
-- primOpHasSideEffects (VecReadScalarOffAddrOp _ _ _) = True
-- primOpHasSideEffects (VecWriteScalarOffAddrOp _ _ _) = True
primOpHasSideEffects PrefetchByteArrayOp3 = True
primOpHasSideEffects PrefetchMutableByteArrayOp3 = True
primOpHasSideEffects PrefetchAddrOp3 = True
primOpHasSideEffects PrefetchValueOp3 = True
primOpHasSideEffects PrefetchByteArrayOp2 = True
primOpHasSideEffects PrefetchMutableByteArrayOp2 = True
primOpHasSideEffects PrefetchAddrOp2 = True
primOpHasSideEffects PrefetchValueOp2 = True
primOpHasSideEffects PrefetchByteArrayOp1 = True
primOpHasSideEffects PrefetchMutableByteArrayOp1 = True
primOpHasSideEffects PrefetchAddrOp1 = True
primOpHasSideEffects PrefetchValueOp1 = True
primOpHasSideEffects PrefetchByteArrayOp0 = True
primOpHasSideEffects PrefetchMutableByteArrayOp0 = True
primOpHasSideEffects PrefetchAddrOp0 = True
primOpHasSideEffects PrefetchValueOp0 = True
primOpHasSideEffects ObjectArrayAtOp      = True
primOpHasSideEffects ObjectArraySetOp     = True
primOpHasSideEffects ObjectArrayNewOp     = True
primOpHasSideEffects ArrayLengthOp        = True
primOpHasSideEffects ReadJByteArrayOp     = True
primOpHasSideEffects WriteJByteArrayOp    = True
primOpHasSideEffects NewJByteArrayOp      = True
primOpHasSideEffects NewJBooleanArrayOp   = True
primOpHasSideEffects ReadJBooleanArrayOp  = True
primOpHasSideEffects WriteJBooleanArrayOp = True
primOpHasSideEffects NewJCharArrayOp      = True
primOpHasSideEffects ReadJCharArrayOp     = True
primOpHasSideEffects WriteJCharArrayOp    = True
primOpHasSideEffects NewJShortArrayOp     = True
primOpHasSideEffects ReadJShortArrayOp    = True
primOpHasSideEffects WriteJShortArrayOp   = True
primOpHasSideEffects NewJIntArrayOp       = True
primOpHasSideEffects ReadJIntArrayOp      = True
primOpHasSideEffects WriteJIntArrayOp     = True
primOpHasSideEffects NewJLongArrayOp      = True
primOpHasSideEffects ReadJLongArrayOp     = True
primOpHasSideEffects WriteJLongArrayOp    = True
primOpHasSideEffects NewJFloatArrayOp     = True
primOpHasSideEffects ReadJFloatArrayOp    = True
primOpHasSideEffects WriteJFloatArrayOp   = True
primOpHasSideEffects NewJDoubleArrayOp    = True
primOpHasSideEffects ReadJDoubleArrayOp   = True
primOpHasSideEffects WriteJDoubleArrayOp  = True
-- Start
-- These don't technically do any side effects since they either return one of the args, or
-- a new Object, but will be treated as such to avoid speculation in the optimizer.
primOpHasSideEffects FreshStateTokenOp    = True
primOpHasSideEffects FreshObjectTokenOp     = True
primOpHasSideEffects FreshNullObjectTokenOp = True
-- End
primOpHasSideEffects _ = False

primOpCanFail :: PrimOp -> Bool
primOpCanFail IntQuotOp = True
primOpCanFail IntRemOp = True
primOpCanFail IntQuotRemOp = True
primOpCanFail WordQuotOp = True
primOpCanFail WordRemOp = True
primOpCanFail WordQuotRemOp = True
primOpCanFail WordQuotRem2Op = True
primOpCanFail DoubleDivOp = True
primOpCanFail DoubleLogOp = True
primOpCanFail DoubleAsinOp = True
primOpCanFail DoubleAcosOp = True
primOpCanFail FloatDivOp = True
primOpCanFail FloatLogOp = True
primOpCanFail FloatAsinOp = True
primOpCanFail FloatAcosOp = True
primOpCanFail ReadArrayOp = True
primOpCanFail WriteArrayOp = True
primOpCanFail IndexArrayOp = True
primOpCanFail CopyArrayOp = True
primOpCanFail CopyMutableArrayOp = True
primOpCanFail CloneArrayOp = True
primOpCanFail CloneMutableArrayOp = True
primOpCanFail FreezeArrayOp = True
primOpCanFail ThawArrayOp = True
primOpCanFail ReadSmallArrayOp = True
primOpCanFail WriteSmallArrayOp = True
primOpCanFail IndexSmallArrayOp = True
primOpCanFail CopySmallArrayOp = True
primOpCanFail CopySmallMutableArrayOp = True
primOpCanFail CloneSmallArrayOp = True
primOpCanFail CloneSmallMutableArrayOp = True
primOpCanFail FreezeSmallArrayOp = True
primOpCanFail ThawSmallArrayOp = True
primOpCanFail IndexByteArrayOp_Char = True
primOpCanFail IndexByteArrayOp_WideChar = True
primOpCanFail IndexByteArrayOp_Int = True
primOpCanFail IndexByteArrayOp_Word = True
primOpCanFail IndexByteArrayOp_Addr = True
primOpCanFail IndexByteArrayOp_Float = True
primOpCanFail IndexByteArrayOp_Double = True
primOpCanFail IndexByteArrayOp_StablePtr = True
primOpCanFail IndexByteArrayOp_Int8 = True
primOpCanFail IndexByteArrayOp_Int16 = True
primOpCanFail IndexByteArrayOp_Int32 = True
primOpCanFail IndexByteArrayOp_Int64 = True
primOpCanFail IndexByteArrayOp_Word8 = True
primOpCanFail IndexByteArrayOp_Word16 = True
primOpCanFail IndexByteArrayOp_Word32 = True
primOpCanFail IndexByteArrayOp_Word64 = True
primOpCanFail ReadByteArrayOp_Char = True
primOpCanFail ReadByteArrayOp_WideChar = True
primOpCanFail ReadByteArrayOp_Int = True
primOpCanFail ReadByteArrayOp_Word = True
primOpCanFail ReadByteArrayOp_Addr = True
primOpCanFail ReadByteArrayOp_Float = True
primOpCanFail ReadByteArrayOp_Double = True
primOpCanFail ReadByteArrayOp_StablePtr = True
primOpCanFail ReadByteArrayOp_Int8 = True
primOpCanFail ReadByteArrayOp_Int16 = True
primOpCanFail ReadByteArrayOp_Int32 = True
primOpCanFail ReadByteArrayOp_Int64 = True
primOpCanFail ReadByteArrayOp_Word8 = True
primOpCanFail ReadByteArrayOp_Word16 = True
primOpCanFail ReadByteArrayOp_Word32 = True
primOpCanFail ReadByteArrayOp_Word64 = True
primOpCanFail WriteByteArrayOp_Char = True
primOpCanFail WriteByteArrayOp_WideChar = True
primOpCanFail WriteByteArrayOp_Int = True
primOpCanFail WriteByteArrayOp_Word = True
primOpCanFail WriteByteArrayOp_Addr = True
primOpCanFail WriteByteArrayOp_Float = True
primOpCanFail WriteByteArrayOp_Double = True
primOpCanFail WriteByteArrayOp_StablePtr = True
primOpCanFail WriteByteArrayOp_Int8 = True
primOpCanFail WriteByteArrayOp_Int16 = True
primOpCanFail WriteByteArrayOp_Int32 = True
primOpCanFail WriteByteArrayOp_Int64 = True
primOpCanFail WriteByteArrayOp_Word8 = True
primOpCanFail WriteByteArrayOp_Word16 = True
primOpCanFail WriteByteArrayOp_Word32 = True
primOpCanFail WriteByteArrayOp_Word64 = True
primOpCanFail CopyByteArrayOp = True
primOpCanFail CopyMutableByteArrayOp = True
primOpCanFail CopyByteArrayToAddrOp = True
primOpCanFail CopyMutableByteArrayToAddrOp = True
primOpCanFail CopyAddrToByteArrayOp = True
primOpCanFail SetByteArrayOp = True
primOpCanFail AtomicReadByteArrayOp_Int = True
primOpCanFail AtomicWriteByteArrayOp_Int = True
primOpCanFail CasByteArrayOp_Int = True
primOpCanFail FetchAddByteArrayOp_Int = True
primOpCanFail FetchSubByteArrayOp_Int = True
primOpCanFail FetchAndByteArrayOp_Int = True
primOpCanFail FetchNandByteArrayOp_Int = True
primOpCanFail FetchOrByteArrayOp_Int = True
primOpCanFail FetchXorByteArrayOp_Int = True
primOpCanFail IndexArrayArrayOp_ByteArray = True
primOpCanFail IndexArrayArrayOp_ArrayArray = True
primOpCanFail ReadArrayArrayOp_ByteArray = True
primOpCanFail ReadArrayArrayOp_MutableByteArray = True
primOpCanFail ReadArrayArrayOp_ArrayArray = True
primOpCanFail ReadArrayArrayOp_MutableArrayArray = True
primOpCanFail WriteArrayArrayOp_ByteArray = True
primOpCanFail WriteArrayArrayOp_MutableByteArray = True
primOpCanFail WriteArrayArrayOp_ArrayArray = True
primOpCanFail WriteArrayArrayOp_MutableArrayArray = True
primOpCanFail CopyArrayArrayOp = True
primOpCanFail CopyMutableArrayArrayOp = True
primOpCanFail IndexOffAddrOp_Char = True
primOpCanFail IndexOffAddrOp_WideChar = True
primOpCanFail IndexOffAddrOp_Int = True
primOpCanFail IndexOffAddrOp_Word = True
primOpCanFail IndexOffAddrOp_Addr = True
primOpCanFail IndexOffAddrOp_Float = True
primOpCanFail IndexOffAddrOp_Double = True
primOpCanFail IndexOffAddrOp_StablePtr = True
primOpCanFail IndexOffAddrOp_Int8 = True
primOpCanFail IndexOffAddrOp_Int16 = True
primOpCanFail IndexOffAddrOp_Int32 = True
primOpCanFail IndexOffAddrOp_Int64 = True
primOpCanFail IndexOffAddrOp_Word8 = True
primOpCanFail IndexOffAddrOp_Word16 = True
primOpCanFail IndexOffAddrOp_Word32 = True
primOpCanFail IndexOffAddrOp_Word64 = True
primOpCanFail ReadOffAddrOp_Char = True
primOpCanFail ReadOffAddrOp_WideChar = True
primOpCanFail ReadOffAddrOp_Int = True
primOpCanFail ReadOffAddrOp_Word = True
primOpCanFail ReadOffAddrOp_Addr = True
primOpCanFail ReadOffAddrOp_Float = True
primOpCanFail ReadOffAddrOp_Double = True
primOpCanFail ReadOffAddrOp_StablePtr = True
primOpCanFail ReadOffAddrOp_Int8 = True
primOpCanFail ReadOffAddrOp_Int16 = True
primOpCanFail ReadOffAddrOp_Int32 = True
primOpCanFail ReadOffAddrOp_Int64 = True
primOpCanFail ReadOffAddrOp_Word8 = True
primOpCanFail ReadOffAddrOp_Word16 = True
primOpCanFail ReadOffAddrOp_Word32 = True
primOpCanFail ReadOffAddrOp_Word64 = True
primOpCanFail WriteOffAddrOp_Char = True
primOpCanFail WriteOffAddrOp_WideChar = True
primOpCanFail WriteOffAddrOp_Int = True
primOpCanFail WriteOffAddrOp_Word = True
primOpCanFail WriteOffAddrOp_Addr = True
primOpCanFail WriteOffAddrOp_Float = True
primOpCanFail WriteOffAddrOp_Double = True
primOpCanFail WriteOffAddrOp_StablePtr = True
primOpCanFail WriteOffAddrOp_Int8 = True
primOpCanFail WriteOffAddrOp_Int16 = True
primOpCanFail WriteOffAddrOp_Int32 = True
primOpCanFail WriteOffAddrOp_Int64 = True
primOpCanFail WriteOffAddrOp_Word8 = True
primOpCanFail WriteOffAddrOp_Word16 = True
primOpCanFail WriteOffAddrOp_Word32 = True
primOpCanFail WriteOffAddrOp_Word64 = True
primOpCanFail ReadMutVarOp = True
primOpCanFail WriteMutVarOp = True
primOpCanFail AtomicModifyMutVarOp = True
-- primOpCanFail (VecInsertOp _ _ _) = True
-- primOpCanFail (VecDivOp _ _ _) = True
-- primOpCanFail (VecQuotOp _ _ _) = True
-- primOpCanFail (VecRemOp _ _ _) = True
-- primOpCanFail (VecIndexByteArrayOp _ _ _) = True
-- primOpCanFail (VecReadByteArrayOp _ _ _) = True
-- primOpCanFail (VecWriteByteArrayOp _ _ _) = True
-- primOpCanFail (VecIndexOffAddrOp _ _ _) = True
-- primOpCanFail (VecReadOffAddrOp _ _ _) = True
-- primOpCanFail (VecWriteOffAddrOp _ _ _) = True
-- primOpCanFail (VecIndexScalarByteArrayOp _ _ _) = True
-- primOpCanFail (VecReadScalarByteArrayOp _ _ _) = True
-- primOpCanFail (VecWriteScalarByteArrayOp _ _ _) = True
-- primOpCanFail (VecIndexScalarOffAddrOp _ _ _) = True
-- primOpCanFail (VecReadScalarOffAddrOp _ _ _) = True
-- primOpCanFail (VecWriteScalarOffAddrOp _ _ _) = True
-- ETA-specific
-- TODO: Do they really fail?
primOpCanFail Word64Quot          = True
primOpCanFail Word64Rem           = True
primOpCanFail Int64Quot           = True
primOpCanFail Int64Rem            = True
primOpCanFail ObjectArrayAtOp     = True
primOpCanFail ObjectArraySetOp    = True
primOpCanFail ObjectArrayNewOp    = True
primOpCanFail ArrayLengthOp       = True
primOpCanFail IndexJByteArrayOp   = True
primOpCanFail ReadJByteArrayOp    = True
primOpCanFail WriteJByteArrayOp   = True
primOpCanFail ClassCastOp         = True
primOpCanFail IsNullObjectOp       = True
primOpCanFail NewJByteArrayOp      = True
primOpCanFail NewJBooleanArrayOp   = True
primOpCanFail ReadJBooleanArrayOp  = True
primOpCanFail WriteJBooleanArrayOp = True
primOpCanFail NewJCharArrayOp      = True
primOpCanFail ReadJCharArrayOp     = True
primOpCanFail WriteJCharArrayOp    = True
primOpCanFail NewJShortArrayOp     = True
primOpCanFail ReadJShortArrayOp    = True
primOpCanFail WriteJShortArrayOp   = True
primOpCanFail NewJIntArrayOp       = True
primOpCanFail ReadJIntArrayOp      = True
primOpCanFail WriteJIntArrayOp     = True
primOpCanFail NewJLongArrayOp      = True
primOpCanFail ReadJLongArrayOp     = True
primOpCanFail WriteJLongArrayOp    = True
primOpCanFail NewJFloatArrayOp     = True
primOpCanFail ReadJFloatArrayOp    = True
primOpCanFail WriteJFloatArrayOp   = True
primOpCanFail NewJDoubleArrayOp    = True
primOpCanFail ReadJDoubleArrayOp   = True
primOpCanFail WriteJDoubleArrayOp  = True
primOpCanFail _ = False

primOpOkForSpeculation :: PrimOp -> Bool
  -- See Note [PrimOp can_fail and has_side_effects]
  -- See comments with CoreUtils.exprOkForSpeculation
  -- primOpOkForSpeculation => primOpOkForSideEffects
primOpOkForSpeculation op
  =  primOpOkForSideEffects op
  && not (primOpOutOfLine op || primOpCanFail op)
    -- I think the "out of line" test is because out of line things can
    -- be expensive (eg sine, cosine), and so we may not want to speculate them

primOpOkForSideEffects :: PrimOp -> Bool
primOpOkForSideEffects op
  = not (primOpHasSideEffects op)

{-
Note [primOpIsCheap]
~~~~~~~~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once, and/or push it inside a lambda.  The latter could change the
behaviour of 'seq' for primops that can fail, so we don't treat them as cheap.
-}

primOpIsCheap :: PrimOp -> Bool
-- See Note [PrimOp can_fail and has_side_effects]
primOpIsCheap op = primOpOkForSpeculation op
-- In March 2001, we changed this to
--      primOpIsCheap op = False
-- thereby making *no* primops seem cheap.  But this killed eta
-- expansion on case (x ==# y) of True -> \s -> ...
-- which is bad.  In particular a loop like
--      doLoop n = loop 0
--     where
--         loop i | i == n    = return ()
--                | otherwise = bar i >> loop (i+1)
-- allocated a closure every time round because it doesn't eta expand.
--
-- The problem that originally gave rise to the change was
--      let x = a +# b *# c in x +# x
-- were we don't want to inline x. But primopIsCheap doesn't control
-- that (it's exprIsDupable that does) so the problem doesn't occur
-- even if primOpIsCheap sometimes says 'True'.

{-
************************************************************************
*                                                                      *
               PrimOp code size
*                                                                      *
************************************************************************

primOpCodeSize
~~~~~~~~~~~~~~
Gives an indication of the code size of a primop, for the purposes of
calculating unfolding sizes; see CoreUnfold.sizeExpr.
-}

primOpCodeSize :: PrimOp -> Int
primOpCodeSize OrdOp = 0
primOpCodeSize IntAddCOp = 2
primOpCodeSize IntSubCOp = 2
primOpCodeSize ChrOp = 0
primOpCodeSize Int2WordOp = 0
primOpCodeSize Word2IntOp = 0
primOpCodeSize DoubleExpOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleLogOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleSqrtOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleSinOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleCosOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleTanOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleAsinOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleAcosOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleAtanOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleSinhOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleCoshOp =  primOpCodeSizeForeignCall
primOpCodeSize DoubleTanhOp =  primOpCodeSizeForeignCall
primOpCodeSize DoublePowerOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatExpOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatLogOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatSqrtOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatSinOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatCosOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatTanOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatAsinOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatAcosOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatAtanOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatSinhOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatCoshOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatTanhOp =  primOpCodeSizeForeignCall
primOpCodeSize FloatPowerOp =  primOpCodeSizeForeignCall
primOpCodeSize WriteArrayOp = 2
primOpCodeSize CopyByteArrayOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize CopyMutableByteArrayOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize CopyByteArrayToAddrOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize CopyMutableByteArrayToAddrOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize CopyAddrToByteArrayOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize SetByteArrayOp =  primOpCodeSizeForeignCall + 4
primOpCodeSize Addr2IntOp = 0
primOpCodeSize Int2AddrOp = 0
primOpCodeSize Addr2Int64Op = 0
primOpCodeSize Int642AddrOp = 0
primOpCodeSize WriteMutVarOp =  primOpCodeSizeForeignCall
primOpCodeSize TouchOp =  0
primOpCodeSize ParOp =  primOpCodeSizeForeignCall
primOpCodeSize SparkOp =  primOpCodeSizeForeignCall
primOpCodeSize AddrToAnyOp = 0
primOpCodeSize JBool2IntOp = 0
primOpCodeSize JByte2IntOp = 0
primOpCodeSize Int2JBoolOp = 0
primOpCodeSize Int2JByteOp = 0
primOpCodeSize JShort2IntOp = 0
primOpCodeSize Int2JShortOp = 0
primOpCodeSize JChar2WordOp = 0
primOpCodeSize Word2JCharOp = 0
primOpCodeSize StablePtr2AddrOp = primOpCodeSizeForeignCall
primOpCodeSize Addr2StablePtrOp = primOpCodeSizeForeignCall
primOpCodeSize FreshStateTokenOp = 0
primOpCodeSize FreshObjectTokenOp = 0
primOpCodeSize FreshNullObjectTokenOp = 0
primOpCodeSize _ =  primOpCodeSizeDefault

primOpCodeSizeDefault :: Int
primOpCodeSizeDefault = 1
  -- CoreUnfold.primOpSize already takes into account primOpOutOfLine
  -- and adds some further costs for the args in that case.

primOpCodeSizeForeignCall :: Int
primOpCodeSizeForeignCall = 4

{-
************************************************************************
*                                                                      *
               PrimOp types
*                                                                      *
************************************************************************
-}

primOpType :: PrimOp -> Type  -- you may want to use primOpSig instead
primOpType op
  = case primOpInfo op of
    Dyadic  _occ ty -> dyadic_fun_ty ty
    Monadic _occ ty -> monadic_fun_ty ty
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkForAllTys tyvars (mkFunTys arg_tys res_ty)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case primOpInfo op of
               Dyadic    occ _     -> occ
               Monadic   occ _     -> occ
               Compare   occ _     -> occ
               GenPrimOp occ _ _ _ -> occ

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOp -> ([TyVar], [Type], Type, Arity, StrictSig)
primOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo op) of
        Monadic   _occ ty                    -> ([],     [ty],    ty       )
        Dyadic    _occ ty                    -> ([],     [ty,ty], ty       )
        Compare   _occ ty                    -> ([],     [ty,ty], intPrimTy)
        GenPrimOp _occ tyvars arg_tys res_ty -> (tyvars, arg_tys, res_ty   )

data PrimOpResultInfo
  = ReturnsPrim     PrimRep
  | ReturnsAlg      TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty                        -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty                        -> ReturnsPrim (typePrimRep ty)
      Compare _ _                         -> ReturnsPrim (tyConPrimRep intPrimTyCon)
      GenPrimOp _ _ _ ty | isPrimTyCon tc -> ReturnsPrim (tyConPrimRep tc)
                         | otherwise      -> ReturnsAlg tc
                         where
                           tc = tyConAppTyCon ty
                        -- All primops return a tycon-app result
                        -- The tycon can be an unboxed tuple, though, which
                        -- gives rise to a ReturnAlg

{-
We do not currently make use of whether primops are commutable.

We used to try to move constants to the right hand side for strength
reduction.
-}

{-
commutableOp :: PrimOp -> Bool
commutableOp CharEqOp = True
commutableOp CharNeOp = True
commutableOp IntAddOp = True
commutableOp IntMulOp = True
commutableOp IntMulMayOfloOp = True
commutableOp AndIOp = True
commutableOp OrIOp = True
commutableOp XorIOp = True
commutableOp IntEqOp = True
commutableOp IntNeOp = True
commutableOp WordAddOp = True
commutableOp WordAdd2Op = True
commutableOp WordMulOp = True
commutableOp WordMul2Op = True
commutableOp AndOp = True
commutableOp OrOp = True
commutableOp XorOp = True
commutableOp DoubleEqOp = True
commutableOp DoubleNeOp = True
commutableOp DoubleAddOp = True
commutableOp DoubleMulOp = True
commutableOp FloatEqOp = True
commutableOp FloatNeOp = True
commutableOp FloatAddOp = True
commutableOp FloatMulOp = True
commutableOp (VecAddOp _ _ _) = True
commutableOp (VecMulOp _ _ _) = True
commutableOp _ = False
-}

-- Utils:

dyadic_fun_ty, monadic_fun_ty, compare_fun_ty :: Type -> Type
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] intPrimTy

-- Output stuff:

pprPrimOp  :: PrimOp -> SDoc
pprPrimOp other_op = pprOccName (primOpOcc other_op)

{-
************************************************************************
*                                                                      *
\subsubsection[PrimCall]{User-imported primitive calls}
*                                                                      *
************************************************************************
-}

data PrimCall = PrimCall CLabelString UnitId

instance Outputable PrimCall where
  ppr (PrimCall lbl pkgId)
        = text "__primcall" <+> ppr pkgId <+> ppr lbl
