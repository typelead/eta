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
    w -> (# wordLog2# w, jbool2int# (isPowerOf2Word# w) #)
integerLog2IsPowerOf2# (J# o#) = (# log2Integer# o#, jbool2int# (isPowerOf2Integer# o#) #)

-- TODO: Work on more efficient implementation
-- roundingMode# :: Integer -> Int# -> Int#
-- roundingMode# (S# i) t =
--     case int2Word# i `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##) of
--       k -> case uncheckedShiftL# 1## t of
--             c -> if isTrue# (c `gtWord#` k)
--                     then 0#
--                     else if isTrue# (c `ltWord#` k)
--                             then 2#
--                             else 1#

roundingMode# :: Integer -> Int# -> Int#
roundingMode# m h =
    case oneInteger `shiftLInteger` h of
      c -> case m `andInteger`
                ((c `plusInteger` c) `minusInteger` oneInteger) of
             r ->
               if c `ltInteger` r
                 then 2#
                 else if c `gtInteger` r
                        then 0#
                        else 1#

oneInteger :: Integer
oneInteger = S# 1#

foreign import java unsafe "@static eta.integer.Utils.isPowerOfTwo" isPowerOf2Word#
  :: Word# -> JBool#

foreign import java unsafe "@static eta.integer.Utils.isPowerOfTwo" isPowerOf2Integer#
  :: Integer# -> JBool#

foreign import java unsafe "@static eta.integer.Utils.log2" log2Integer#
  :: Integer# -> Int#

foreign import java unsafe "@static eta.integer.Utils.log2" wordLog2#
  :: Word# -> Int#
