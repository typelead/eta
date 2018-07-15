{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.PrimitiveBase
-- Copyright   :  (c) Rahul Muttineni 2016-2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Dealing with native Java primitives.
--
-----------------------------------------------------------------------------

module Java.PrimitiveBase
  ( Byte(..)
  , Short(..)
  , JChar(..) )
where

import Data.Bits
import Data.Maybe

import GHC.Prim
import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Word hiding (uncheckedShiftL64#, uncheckedShiftRL64#)
import GHC.Show

-- TODO: Add rewrite rules

{- | The Byte type (8-bit signed integer) with associated instances. -}

data Byte = B# JByte#

instance Eq Byte where
    (==) (B# x) (B# y) = isTrue# ((jbyte2int# x) ==# (jbyte2int# y))

instance Ord Byte where
    compare (B# x) (B# y) = compareInt# (jbyte2int# x)  (jbyte2int# y)
    (<)     (B# x) (B# y) = isTrue# ((jbyte2int# x) <#  (jbyte2int# y))
    (<=)    (B# x) (B# y) = isTrue# ((jbyte2int# x) <=# (jbyte2int# y))
    (>=)    (B# x) (B# y) = isTrue# ((jbyte2int# x) >=# (jbyte2int# y))
    (>)     (B# x) (B# y) = isTrue# ((jbyte2int# x) >#  (jbyte2int# y))

instance Num Byte where
    (B# x#) + (B# y#)    = B# (int2jbyte# ((jbyte2int# x#) +# (jbyte2int# y#)))
    (B# x#) - (B# y#)    = B# (int2jbyte# ((jbyte2int# x#) -# (jbyte2int# y#)))
    (B# x#) * (B# y#)    = B# (int2jbyte# ((jbyte2int# x#) *# (jbyte2int# y#)))
    negate (B# x#)        = B# (int2jbyte# (negateInt# (jbyte2int# x#)))
    abs x | x >= 0        = x
          | otherwise     = negate x
    signum x | x > 0      = 1
    signum 0              = 0
    signum _              = -1
    fromInteger i         = B# (int2jbyte# (integerToInt i))

instance Real Byte where
    toRational x = toInteger x % 1

instance  Bounded Byte where
    minBound =  -0x80
    maxBound =  0x7F

instance Enum Byte where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Byte"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Byte"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Byte) && i <= fromIntegral (maxBound::Byte)
                        = B# (int2jbyte# i#)
        | otherwise     = toEnumError "Byte" i (minBound::Byte, maxBound::Byte)
    fromEnum (B# x#)    = I# (jbyte2int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Byte where
    quot    x@(B# x#) y@(B# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = B# (int2jbyte# ((jbyte2int# x#) `quotInt#` (jbyte2int# y#)))
    rem     (B# x#) y@(B# y#)
        | y == 0                     = divZeroError
        | otherwise                  = B# (int2jbyte# ((jbyte2int# x#) `remInt#` (jbyte2int# y#)))
    div     x@(B# x#) y@(B# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = B# (int2jbyte# ((jbyte2int# x#) `divInt#` (jbyte2int# y#)))
    mod       (B# x#) y@(B# y#)
        | y == 0                     = divZeroError
        | otherwise                  = B# (int2jbyte# ((jbyte2int# x#) `modInt#` (jbyte2int# y#)))
    quotRem x@(B# x#) y@(B# y#)
        | y == 0                     = divZeroError
          -- Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case (jbyte2int# x#) `quotRemInt#` (jbyte2int# y#) of
                                       (# q, r #) ->
                                           (B# (int2jbyte# q),
                                            B# (int2jbyte# r))
    divMod  x@(B# x#) y@(B# y#)
        | y == 0                     = divZeroError
          -- Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case (jbyte2int# x#) `divModInt#` (jbyte2int# y#) of
                                       (# d, m #) ->
                                           (B# (int2jbyte# d),
                                            B# (int2jbyte# m))
    toInteger (B# x#)               = smallInteger (jbyte2int# x#)

instance Ix Byte where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

instance Show Byte where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Read Byte where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Byte where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (B# x#) .&.   (B# y#)   = B# (int2jbyte# (word2Int# (int2Word# (jbyte2int# x#) `and#` int2Word# (jbyte2int# y#))))
    (B# x#) .|.   (B# y#)   = B# (int2jbyte# (word2Int# (int2Word# (jbyte2int# x#) `or#`  int2Word# (jbyte2int# y#))))
    (B# x#) `xor` (B# y#)   = B# (int2jbyte# (word2Int# (int2Word# (jbyte2int# x#) `xor#` int2Word# (jbyte2int# y#))))
    complement (B# x#)       = B# (int2jbyte# (word2Int# (not# (int2Word# (jbyte2int# x#)))))
    (B# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#) = B# (int2jbyte# (narrow8Int# ((jbyte2int# x#) `iShiftL#` i#)))
        | otherwise           = B# (int2jbyte# ((jbyte2int# x#) `iShiftRA#` negateInt# i#))
    (B# x#) `shiftL`       (I# i#) = B# (int2jbyte# (narrow8Int# ((jbyte2int# x#) `iShiftL#` i#)))
    (B# x#) `unsafeShiftL` (I# i#) = B# (int2jbyte# (narrow8Int# ((jbyte2int# x#) `uncheckedIShiftL#` i#)))
    (B# x#) `shiftR`       (I# i#) = B# (int2jbyte# ((jbyte2int# x#) `iShiftRA#` i#))
    (B# x#) `unsafeShiftR` (I# i#) = B# (int2jbyte# ((jbyte2int# x#) `uncheckedIShiftRA#` i#))
    (B# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = B# x#
        | otherwise
        = B# (int2jbyte# (narrow8Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                       (x'# `uncheckedShiftRL#` (8# -# i'#))))))
        where
        !x'# = narrow8Word# (int2Word# (jbyte2int# x#))
        !i'# = word2Int# (int2Word# i# `and#` 7##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = True
    popCount (B# x#)         = I# (word2Int# (popCnt8# (int2Word# (jbyte2int# x#))))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Byte where
    finiteBitSize _ = 8
    countLeadingZeros  (B# x#) = I# (word2Int# (clz8# (int2Word# (jbyte2int# x#))))
    countTrailingZeros (B# x#) = I# (word2Int# (ctz8# (int2Word# (jbyte2int# x#))))

{- | The Short type (16-bit signed integer) with associated instances. -}

data Short  = S# JShort#

instance Eq Short where
    (==) (S# x) (S# y) = isTrue# ((jshort2int# x) ==# (jshort2int# y))

instance Ord Short where
    compare (S# x) (S# y) = compareInt# (jshort2int# x)  (jshort2int# y)
    (<)     (S# x) (S# y) = isTrue# ((jshort2int# x) <#  (jshort2int# y))
    (<=)    (S# x) (S# y) = isTrue# ((jshort2int# x) <=# (jshort2int# y))
    (>=)    (S# x) (S# y) = isTrue# ((jshort2int# x) >=# (jshort2int# y))
    (>)     (S# x) (S# y) = isTrue# ((jshort2int# x) >#  (jshort2int# y))

instance Num Short where
    (S# x#) + (S# y#)    = S# (int2jshort# ((jshort2int# x#) +# (jshort2int# y#)))
    (S# x#) - (S# y#)    = S# (int2jshort# ((jshort2int# x#) -# (jshort2int# y#)))
    (S# x#) * (S# y#)    = S# (int2jshort# ((jshort2int# x#) *# (jshort2int# y#)))
    negate (S# x#)        = S# (int2jshort# (negateInt# (jshort2int# x#)))
    abs x | x >= 0        = x
          | otherwise     = negate x
    signum x | x > 0      = 1
    signum 0              = 0
    signum _              = -1
    fromInteger i         = S# (int2jshort# (integerToInt i))

instance Real Short where
    toRational x = toInteger x % 1

instance  Bounded Short where
    minBound =  -0x8000
    maxBound =  0x7FFF

instance Enum Short where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Short"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Short"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Short) && i <= fromIntegral (maxBound::Short)
                        = S# (int2jshort# i#)
        | otherwise     = toEnumError "Short" i (minBound::Short, maxBound::Short)
    fromEnum (S# x#)    = I# (jshort2int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Short where
    quot    x@(S# x#) y@(S# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = S# (int2jshort# ((jshort2int# x#) `quotInt#` (jshort2int# y#)))
    rem     (S# x#) y@(S# y#)
        | y == 0                     = divZeroError
        | otherwise                  = S# (int2jshort# ((jshort2int# x#) `remInt#` (jshort2int# y#)))
    div     x@(S# x#) y@(S# y#)
        | y == 0                     = divZeroError
        | y == (-1) && x == minBound = overflowError -- Note [Order of tests]
        | otherwise                  = S# (int2jshort# ((jshort2int# x#) `divInt#` (jshort2int# y#)))
    mod       (S# x#) y@(S# y#)
        | y == 0                     = divZeroError
        | otherwise                  = S# (int2jshort# ((jshort2int# x#) `modInt#` (jshort2int# y#)))
    quotRem x@(S# x#) y@(S# y#)
        | y == 0                     = divZeroError
          -- Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case (jshort2int# x#) `quotRemInt#` (jshort2int# y#) of
                                       (# q, r #) ->
                                           (S# (int2jshort# q),
                                            S# (int2jshort# r))
    divMod  x@(S# x#) y@(S# y#)
        | y == 0                     = divZeroError
          -- Note [Order of tests]
        | y == (-1) && x == minBound = (overflowError, 0)
        | otherwise                  = case (jshort2int# x#) `divModInt#` (jshort2int# y#) of
                                       (# d, m #) ->
                                           (S# (int2jshort# d),
                                            S# (int2jshort# m))
    toInteger (S# x#)               = smallInteger (jshort2int# x#)

instance Ix Short where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

instance Show Short where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Read Short where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Short where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (S# x#) .&.   (S# y#)   = S# (int2jshort# (word2Int# (int2Word# (jshort2int# x#) `and#` int2Word# (jshort2int# y#))))
    (S# x#) .|.   (S# y#)   = S# (int2jshort# (word2Int# (int2Word# (jshort2int# x#) `or#`  int2Word# (jshort2int# y#))))
    (S# x#) `xor` (S# y#)   = S# (int2jshort# (word2Int# (int2Word# (jshort2int# x#) `xor#` int2Word# (jshort2int# y#))))
    complement (S# x#)       = S# (int2jshort# (word2Int# (not# (int2Word# (jshort2int# x#)))))
    (S# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#) = S# (int2jshort# (narrow16Int# ((jshort2int# x#) `iShiftL#` i#)))
        | otherwise           = S# (int2jshort# ((jshort2int# x#) `iShiftRA#` negateInt# i#))
    (S# x#) `shiftL`       (I# i#) = S# (int2jshort# (narrow16Int# ((jshort2int# x#) `iShiftL#` i#)))
    (S# x#) `unsafeShiftL` (I# i#) = S# (int2jshort# (narrow16Int# ((jshort2int# x#) `uncheckedIShiftL#` i#)))
    (S# x#) `shiftR`       (I# i#) = S# (int2jshort# ((jshort2int# x#) `iShiftRA#` i#))
    (S# x#) `unsafeShiftR` (I# i#) = S# (int2jshort# ((jshort2int# x#) `uncheckedIShiftRA#` i#))
    (S# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#)
        = S# x#
        | otherwise
        = S# (int2jshort# (narrow16Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                       (x'# `uncheckedShiftRL#` (16# -# i'#))))))
        where
        !x'# = narrow16Word# (int2Word# (jshort2int# x#))
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = True
    popCount (S# x#)         = I# (word2Int# (popCnt16# (int2Word# (jshort2int# x#))))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Short where
    finiteBitSize _ = 16
    countLeadingZeros  (S# x#) = I# (word2Int# (clz16# (int2Word# (jshort2int# x#))))
    countTrailingZeros (S# x#) = I# (word2Int# (ctz16# (int2Word# (jshort2int# x#))))


{- | The JChar type (16-bit unsigned integer) with associated instances. -}

data JChar = JC# JChar#

instance Eq JChar where
    (==) (JC# x) (JC# y) = isTrue# ((jchar2word# x) `eqWord#` (jchar2word# y))

instance Ord JChar where
    compare (JC# x) (JC# y) = compareWord# (jchar2word# x)  (jchar2word# y)
    (<)     (JC# x) (JC# y) = isTrue# ((jchar2word# x) `ltWord#` (jchar2word# y))
    (<=)    (JC# x) (JC# y) = isTrue# ((jchar2word# x) `leWord#` (jchar2word# y))
    (>=)    (JC# x) (JC# y) = isTrue# ((jchar2word# x) `geWord#` (jchar2word# y))
    (>)     (JC# x) (JC# y) = isTrue# ((jchar2word# x) `gtWord#` (jchar2word# y))

instance Num JChar where
    (JC# x#) + (JC# y#)    = JC# (word2jchar# ((jchar2word# x#) `plusWord#` (jchar2word# y#)))
    (JC# x#) - (JC# y#)    = JC# (word2jchar# ((jchar2word# x#) `minusWord#` (jchar2word# y#)))
    (JC# x#) * (JC# y#)    = JC# (word2jchar# ((jchar2word# x#) `timesWord#` (jchar2word# y#)))
    negate (JC# x#)        = JC# (word2jchar# (int2Word# (negateInt# (word2Int# (jchar2word# x#)))))
    abs x                 = x
    signum 0              = 0
    signum _              = 1
    fromInteger i         = JC# (word2jchar# (integerToWord i))

instance Real JChar where
    toRational x = toInteger x % 1

instance Bounded JChar where
    minBound =  0
    maxBound =  0xFFFF

instance Enum JChar where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "JChar"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "JChar"
    toEnum i@(I# i#)
        | i >= 0 && i <= fromIntegral (maxBound::JChar)
                        = JC# (word2jchar# (int2Word# i#))
        | otherwise     = toEnumError "JChar" i (minBound::JChar, maxBound::JChar)
    fromEnum (JC# x#)    = I# (word2Int# (jchar2word# x#))
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral JChar where
    quot    x@(JC# x#) y@(JC# y#)
        | y /= 0                     = JC# (word2jchar# ((jchar2word# x#) `quotWord#` (jchar2word# y#)))
        | otherwise                  = divZeroError
    rem     (JC# x#) y@(JC# y#)
        | y /= 0                     = JC# (word2jchar# ((jchar2word# x#) `remWord#` (jchar2word# y#)))
        | otherwise                  = divZeroError
    div     x@(JC# x#) y@(JC# y#)
        | y /= 0                     = JC# (word2jchar# ((jchar2word# x#) `quotWord#` (jchar2word# y#)))
        | otherwise                  = divZeroError
    mod       (JC# x#) y@(JC# y#)
        | y /= 0                     = JC# (word2jchar# ((jchar2word# x#) `remWord#` (jchar2word# y#)))
        | otherwise                  = divZeroError
    quotRem x@(JC# x#) y@(JC# y#)
        | y /= 0                     = case (jchar2word# x#) `quotRemWord#` (jchar2word# y#) of
                                       (# q, r #) ->
                                           (JC# (word2jchar# q),
                                            JC# (word2jchar# r))
        | otherwise                  = divZeroError
    divMod  (JC# x#) y@(JC# y#)
        | y /= 0                    = (JC# (word2jchar# ((jchar2word# x#) `quotWord#` (jchar2word# y#))), JC# (word2jchar# ((jchar2word# x#) `remWord#` (jchar2word# y#))))
        | otherwise                 = divZeroError
    toInteger (JC# x#)               = smallInteger (word2Int# (jchar2word# x#))

instance Ix JChar where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Show JChar where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Read JChar where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits JChar where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (JC# x#) .&.   (JC# y#)   = JC# (word2jchar# ((jchar2word# x#) `and#` (jchar2word# y#)))
    (JC# x#) .|.   (JC# y#)   = JC# (word2jchar# ((jchar2word# x#) `or#`  (jchar2word# y#)))
    (JC# x#) `xor` (JC# y#)   = JC# (word2jchar# ((jchar2word# x#) `xor#` (jchar2word# y#)))
    complement (JC# x#)       = JC# (word2jchar# ((jchar2word# x#) `xor#` (jchar2word# mb#)))
        where !(JC# mb#) = maxBound
    (JC# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = JC# (word2jchar# ((jchar2word# x#) `shiftL#` i#))
        | otherwise            = JC# (word2jchar# ((jchar2word# x#) `shiftRL#` negateInt# i#))
    (JC# x#) `shiftL`       (I# i#) = JC# (word2jchar# ((jchar2word# x#) `shiftL#` i#))
    (JC# x#) `unsafeShiftL` (I# i#) = JC# (word2jchar# ((jchar2word# x#) `uncheckedShiftL#` i#))
    (JC# x#) `shiftR`       (I# i#) = JC# (word2jchar# ((jchar2word# x#) `shiftRL#` i#))
    (JC# x#) `unsafeShiftR` (I# i#) = JC# (word2jchar# ((jchar2word# x#) `uncheckedShiftRL#` i#))
    (JC# x#) `rotate`       (I# i#)
        | isTrue# (i'# ==# 0#) = JC# x#
        | otherwise  = JC# (word2jchar# (((jchar2word# x#) `uncheckedShiftL#` i'#) `or#`
                                         ((jchar2word# x#) `uncheckedShiftRL#` (16# -# i'#))))
        where
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = True
    popCount (JC# x#)         = I# (word2Int# (popCnt16# (jchar2word# x#)))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits JChar where
    finiteBitSize _ = 16
    countLeadingZeros  (JC# x#) = I# (word2Int# (clz16# (jchar2word# x#)))
    countTrailingZeros (JC# x#) = I# (word2Int# (ctz16# (jchar2word# x#)))
