{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, UnboxedTuples
           , UnliftedFFITypes, GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}

-- TODO: Rewrite this module once GHCVM supports external java files.
module GHC.Integer.BigInteger.Prim (
  Integer#,
  IntegerPair#,
  cmpInteger#,
  cmpIntegerInt#,

  plusInteger#,
  plusIntegerInt#,
  minusInteger#,
  minusIntegerInt#,
  timesInteger#,
  timesIntegerInt#,

  quotRemInteger#,
  quotRemIntegerWord#,
  quotInteger#,
  quotIntegerWord#,
  remInteger#,
  remIntegerWord#,

  divModInteger#,
  divModIntegerWord#,
  divInteger#,
  divIntegerWord#,
  modInteger#,
  modIntegerWord#,
  divExactInteger#,
  divExactIntegerWord#,

  gcdInteger#,
  gcdExtInteger#,
  gcdIntegerInt#,
  gcdInt#,

  decodeDouble#,

  int2Integer#,
  integer2Int#,

  word2Integer#,
  integer2Word#,

  andInteger#,
  orInteger#,
  xorInteger#,
  complementInteger#,

  testBitInteger#,
  mul2ExpInteger#,
  fdivQ2ExpInteger#,

  powInteger#,
  powModInteger#,
  powModSecInteger#,
  recipModInteger#,

  nextPrimeInteger#,
  testPrimeInteger#,

  -- sizeInBaseInteger#,
  -- importIntegerFromByteArray#,
  -- importIntegerFromAddr#,
  -- exportIntegerToMutableByteArray#,
  -- exportIntegerToAddr#,

  int64ToInteger#,  integerToInt64#,
  word64ToInteger#, integerToWord64#,
  -- GHCVM-specific
  zeroInteger#,
  equalsInteger#,
  absInteger#,
  bitsInteger#,
  signumInteger#,
  negateInteger#,
  integer2Float#,
  integer2Double#,
  ) where

import GHC.Prim
import GHC.Types
import GHC.Classes
import GHC.JArray

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

data {-# CLASS "java.math.BigInteger" #-} BigInteger = BigInteger (Object# BigInteger)

type Integer# = Object# BigInteger
type IntegerPair# = ObjectArray# BigInteger

-- TODO: Add derive mechanism
instance Class BigInteger where
  obj = BigInteger
  unobj (BigInteger x) = x

-- NOTE: We need to do this in order to bypass the back that you can't have top level
-- unboxed value bindings.
foreign import prim "@static @field java.math.BigInteger.ZERO" zeroInteger# :: Any -> Integer#
foreign import prim "java.math.BigInteger.equals" equalsInteger# :: Integer# -> Integer# -> Int#
foreign import prim "java.math.BigInteger.abs" absInteger# :: Integer# -> Integer#
foreign import prim "java.math.BigInteger.bitLength" bitsInteger# :: Integer# -> Int#
foreign import prim "java.math.BigInteger.signum" signumInteger# :: Integer# -> Int#
foreign import prim "java.math.BigInteger.negate" negateInteger# :: Integer# -> Integer#

foreign import prim "java.math.BigInteger.compareTo" cmpInteger#
  :: Integer# -> Integer# -> Int#

cmpIntegerInt# :: Integer# -> Int# -> Int#
cmpIntegerInt# bigInt int = cmpInteger# bigInt (int2Integer# int)

foreign import prim "java.math.BigInteger.add" plusInteger#
  :: Integer# -> Integer# -> Integer#

plusIntegerInt# :: Integer# -> Int# -> Integer#
plusIntegerInt# bigInt int = plusInteger# bigInt (int2Integer# int)

foreign import prim "java.math.BigInteger.subtract" minusInteger#
  :: Integer# -> Integer# -> Integer#

minusIntegerInt# :: Integer# -> Int# -> Integer#
minusIntegerInt# bigInt int = minusInteger# bigInt (int2Integer# int)

foreign import prim "java.math.BigInteger.multiply" timesInteger#
  :: Integer# -> Integer# -> Integer#

timesIntegerInt# :: Integer# -> Int# -> Integer#
timesIntegerInt# bigInt int = timesInteger# bigInt (int2Integer# int)

foreign import prim "java.math.BigInteger.divideAndRemainder" quotRemInteger#
  :: Integer# -> Integer# -> IntegerPair#

quotRemIntegerWord# :: Integer# -> Word# -> IntegerPair#
quotRemIntegerWord# bigInt word = quotRemInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.divide" quotInteger#
  :: Integer# -> Integer# -> Integer#

quotIntegerWord# :: Integer# -> Word# -> Integer#
quotIntegerWord# bigInt word = quotInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.remainder" remInteger#
  :: Integer# -> Integer# -> Integer#

-- TODO: Reconcile differences between divMod/quotRem
remIntegerWord# :: Integer# -> Word# -> Integer#
remIntegerWord# bigInt word = remInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.divideAndRemainder" divModInteger#
  :: Integer# -> Integer# -> IntegerPair#

divModIntegerWord# :: Integer# -> Word# -> IntegerPair#
divModIntegerWord# bigInt word = divModInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.divide" divInteger#
  :: Integer# -> Integer# -> Integer#

divIntegerWord# :: Integer# -> Word# -> Integer#
divIntegerWord# bigInt word = divInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.remainder" modInteger#
  :: Integer# -> Integer# -> Integer#

modIntegerWord# :: Integer# -> Word# -> Integer#
modIntegerWord# bigInt word = modInteger# bigInt (word2Integer# word)

-- TODO: Optimize divExactInteger#
foreign import prim "java.math.BigInteger.divide" divExactInteger#
  :: Integer# -> Integer# -> Integer#

divExactIntegerWord# :: Integer# -> Word# -> Integer#
divExactIntegerWord# bigInt word = divExactInteger# bigInt (word2Integer# word)

foreign import prim "java.math.BigInteger.gcd" gcdInteger#
  :: Integer# -> Integer# -> Integer#

foreign import prim "@static ghcvm.integer.Utils.extendedEuclid" gcdExtInteger#
  :: Integer# -> Integer# -> IntegerPair#

gcdIntegerInt# :: Integer# -> Int# -> Integer#
gcdIntegerInt# bigInt int = gcdInteger# bigInt (int2Integer# int)

foreign import prim "@static ghcvm.integer.Utils.gcd" gcdInt#
  :: Int# -> Int# -> Int#

foreign import prim "@inline decodeDouble" decodeDouble#
  :: Int# -> Int# -> (# Int#, Integer# #)

int2Integer# :: Int# -> Integer#
int2Integer# i# = int64ToInteger# (intToInt64# i#)

foreign import prim "@static ghcvm.integer.Utils.toUnsignedBigInteger" word2Integer#
  :: Word# -> Integer#

foreign import prim "java.math.BigInteger.and" andInteger#
  :: Integer# -> Integer# -> Integer#

foreign import prim "java.math.BigInteger.or" orInteger#
  :: Integer# -> Integer# -> Integer#

foreign import prim "java.math.BigInteger.xor" xorInteger#
  :: Integer# -> Integer# -> Integer#

foreign import prim "java.math.BigInteger.testBit" testBitInteger#
  :: Integer# -> Int# -> Int#

foreign import prim "java.math.BigInteger.shiftLeft" mul2ExpInteger#
  :: Integer# -> Int# -> Integer#

foreign import prim "java.math.BigInteger.shiftRight" fdivQ2ExpInteger#
  :: Integer# -> Int# -> Integer#

foreign import prim "java.math.BigInteger.pow" powInteger#
  :: Integer# -> Int# -> Integer#

foreign import prim "java.math.BigInteger.modPow" powModInteger#
  :: Integer# -> Integer# -> Integer# -> Integer#

-- TODO: Use the secure version of the algorithm
foreign import prim "java.math.BigInteger.modPow" powModSecInteger#
  :: Integer# -> Integer# -> Integer# -> Integer#

foreign import prim "java.math.BigInteger.modInverse" recipModInteger#
  :: Integer# -> Integer# -> Integer#

foreign import prim "java.math.BigInteger.nextProbablePrime" nextPrimeInteger#
  :: Integer# -> Integer#

-- NOTE: This is different from GHC's integer-gmp in that the 2nd argument is
--       certainty instead of number of rounds.
foreign import prim "java.math.BigInteger.isProbablePrime" testPrimeInteger#
  :: Integer# -> Int# -> Integer#

foreign import prim "java.math.BigInteger.not" complementInteger#
  :: Integer# -> Integer#

foreign import prim "java.math.BigInteger.valueOf" int64ToInteger#
  :: Int64# -> Integer#

-- TODO: Is this correct?
foreign import prim "java.math.BigInteger.valueOf" word64ToInteger#
  :: Word64# -> Integer#

foreign import prim "java.math.BigInteger.longValue" integerToInt64#
  :: Integer# -> Int64#

-- TODO: Is this correct?
foreign import prim "java.math.BigInteger.longValue" integerToWord64#
  :: Integer# -> Word64#

foreign import prim "java.math.BigInteger.intValue" integer2Int#
  :: Integer# -> Int#

-- TODO: Is this correct?
foreign import prim "java.math.BigInteger.intValue" integer2Word#
  :: Integer# -> Word#

foreign import prim "java.math.BigInteger.floatValue"  integer2Float#
  :: Integer# -> Float#
foreign import prim "java.math.BigInteger.doubleValue" integer2Double#
  :: Integer# -> Double#
