{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude, UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}

-- Fast integer logarithms to base 2.
-- integerLog2# and wordLog2# are of general usefulness,
-- the others are only needed for a fast implementation of
-- fromRational.
-- Since they are needed in GHC.Float, we must expose this
-- module, but it should not show up in the docs.

module GHC.Integer.Logarithms.Internals
    ( integerLog2#
    , integerLog2IsPowerOf2#
    , wordLog2#
    , roundingMode#
    ) where

import GHC.Prim
import GHC.Types (isTrue#)
import GHC.Integer.Type
import GHC.Integer.BigInteger.Prim

-- TODO: Implement using Google Guava's BigIntegerMath/IntMath
default ()

integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i)
integerLog2# (J# o#) = log2Integer# o#

integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
integerLog2IsPowerOf2# (S# i) =
  case int2Word# i of
    w -> (# wordLog2# w, bool2Int# (isPowerOf2Word# w) #)
integerLog2IsPowerOf2# (J# o#) = (# log2Integer# o#, bool2Int# (isPowerOf2Integer# o#) #)

roundingMode# :: Integer -> Int# -> Int#
roundingMode# = unsafeCoerce# 0#

foreign import java unsafe "@static ghcvm.integer.Utils.isPowerOfTwo" isPowerOf2Word#
  :: Word# -> JBool#

foreign import java unsafe "@static ghcvm.integer.Utils.isPowerOfTwo" isPowerOf2Integer#
  :: Integer# -> JBool#

foreign import java unsafe "@static ghcvm.integer.Utils.log2" log2Integer#
  :: Integer# -> Int#

foreign import java unsafe "@static ghcvm.integer.Utils.log2" wordLog2#
  :: Word# -> Int#
