{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, UnboxedTuples
           , UnliftedFFITypes, GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}

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
  -- ETA-specific
  zeroInteger#,
  equalsInteger#,
  absInteger#,
  bitsInteger#,
  signumInteger#,
  negateInteger#,
  integer2Float#,
  integer2Double#,

  encodeFloat#,
  int_encodeFloat#,
  encodeDouble#,
  int_encodeDouble#
  ) where

import GHC.Prim
import GHC.Types
import GHC.Classes

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

data {-# CLASS "java.math.BigInteger" #-} BigInteger
  = BigInteger (Object# BigInteger)
  deriving Class
data {-# CLASS "java.math.BigInteger[]" #-} BigIntegerArray
  = BigIntegerArray (Object# BigIntegerArray)
  deriving Class

type Integer# = Object# BigInteger
type IntegerPair# = Object# BigIntegerArray

-- NOTE: We need to do this in order to bypass the back that you can't have top level
-- unboxed value bindings.
foreign import java unsafe "@static @field java.math.BigInteger.ZERO" zeroInteger# :: Void# -> Integer#
foreign import java unsafe "equals" equalsInteger'# :: Integer# -> Object# Object -> JBool#

equalsInteger# :: Integer# -> Integer# -> JBool#
equalsInteger# i1 i2 = equalsInteger'# i1 (unsafeCoerce# i2)

foreign import java unsafe "abs" absInteger# :: Integer# -> Integer#
foreign import java unsafe "bitLength" bitsInteger# :: Integer# -> Int#
foreign import java unsafe "signum" signumInteger# :: Integer# -> Int#
foreign import java unsafe "negate" negateInteger# :: Integer# -> Integer#

foreign import java unsafe "compareTo" cmpInteger#
  :: Integer# -> Integer# -> Int#

cmpIntegerInt# :: Integer# -> Int# -> Int#
cmpIntegerInt# bigInt int = cmpInteger# bigInt (int2Integer# int)

foreign import java unsafe "add" plusInteger#
  :: Integer# -> Integer# -> Integer#

plusIntegerInt# :: Integer# -> Int# -> Integer#
plusIntegerInt# bigInt int = plusInteger# bigInt (int2Integer# int)

foreign import java unsafe "subtract" minusInteger#
  :: Integer# -> Integer# -> Integer#

minusIntegerInt# :: Integer# -> Int# -> Integer#
minusIntegerInt# bigInt int = minusInteger# bigInt (int2Integer# int)

foreign import java unsafe "multiply" timesInteger#
  :: Integer# -> Integer# -> Integer#

timesIntegerInt# :: Integer# -> Int# -> Integer#
timesIntegerInt# bigInt int = timesInteger# bigInt (int2Integer# int)

foreign import java unsafe "divideAndRemainder" quotRemInteger#
  :: Integer# -> Integer# -> IntegerPair#

quotRemIntegerWord# :: Integer# -> Word# -> IntegerPair#
quotRemIntegerWord# bigInt word = quotRemInteger# bigInt (word2Integer# word)

foreign import java unsafe "divide" quotInteger#
  :: Integer# -> Integer# -> Integer#

quotIntegerWord# :: Integer# -> Word# -> Integer#
quotIntegerWord# bigInt word = quotInteger# bigInt (word2Integer# word)

foreign import java unsafe "remainder" remInteger#
  :: Integer# -> Integer# -> Integer#

-- TODO: Reconcile differences between divMod/quotRem
remIntegerWord# :: Integer# -> Word# -> Integer#
remIntegerWord# bigInt word = remInteger# bigInt (word2Integer# word)

foreign import java unsafe "divideAndRemainder" divModInteger#
  :: Integer# -> Integer# -> IntegerPair#

divModIntegerWord# :: Integer# -> Word# -> IntegerPair#
divModIntegerWord# bigInt word = divModInteger# bigInt (word2Integer# word)

foreign import java unsafe "divide" divInteger#
  :: Integer# -> Integer# -> Integer#

divIntegerWord# :: Integer# -> Word# -> Integer#
divIntegerWord# bigInt word = divInteger# bigInt (word2Integer# word)

foreign import java unsafe "remainder" modInteger#
  :: Integer# -> Integer# -> Integer#

modIntegerWord# :: Integer# -> Word# -> Integer#
modIntegerWord# bigInt word = modInteger# bigInt (word2Integer# word)

-- TODO: Optimize divExactInteger#
foreign import java unsafe "divide" divExactInteger#
  :: Integer# -> Integer# -> Integer#

divExactIntegerWord# :: Integer# -> Word# -> Integer#
divExactIntegerWord# bigInt word = divExactInteger# bigInt (word2Integer# word)

foreign import java unsafe "gcd" gcdInteger#
  :: Integer# -> Integer# -> Integer#

foreign import java unsafe "@static eta.integer.Utils.extendedEuclid" gcdExtInteger#
  :: Integer# -> Integer# -> IntegerPair#

gcdIntegerInt# :: Integer# -> Int# -> Int#
gcdIntegerInt# bigInt int = integer2Int# (gcdInteger# bigInt (int2Integer# int))

foreign import java unsafe "@static eta.integer.Utils.gcd" gcdInt#
  :: Int# -> Int# -> Int#

foreign import prim "eta.integer.Utils.decodeDouble" decodeDouble#
  :: Double# -> (# Int#, Integer# #)

int2Integer# :: Int# -> Integer#
int2Integer# i# = int64ToInteger# (intToInt64# i#)

foreign import java unsafe "@static eta.integer.Utils.toUnsignedBigInteger" word2Integer#
  :: Word# -> Integer#

foreign import java unsafe "and" andInteger#
  :: Integer# -> Integer# -> Integer#

foreign import java unsafe "or" orInteger#
  :: Integer# -> Integer# -> Integer#

foreign import java unsafe "xor" xorInteger#
  :: Integer# -> Integer# -> Integer#

foreign import java unsafe "testBit" testBitInteger#
  :: Integer# -> Int# -> JBool#

foreign import java unsafe "shiftLeft" mul2ExpInteger#
  :: Integer# -> Int# -> Integer#

foreign import java unsafe "shiftRight" fdivQ2ExpInteger#
  :: Integer# -> Int# -> Integer#

foreign import java unsafe "pow" powInteger#
  :: Integer# -> Word# -> Integer#

foreign import java unsafe "modPow" powModInteger#
  :: Integer# -> Integer# -> Integer# -> Integer#

-- TODO: Use the secure version of the algorithm
foreign import java unsafe "modPow" powModSecInteger#
  :: Integer# -> Integer# -> Integer# -> Integer#

foreign import java unsafe "modInverse" recipModInteger#
  :: Integer# -> Integer# -> Integer#

foreign import java unsafe "nextProbablePrime" nextPrimeInteger#
  :: Integer# -> Integer#

-- NOTE: This is different from GHC's integer-gmp in that the 2nd argument is
--       certainty instead of number of rounds.
foreign import java unsafe "isProbablePrime" testPrimeInteger#
  :: Integer# -> Int# -> JBool#

foreign import java unsafe "not" complementInteger#
  :: Integer# -> Integer#

foreign import java unsafe "@static java.math.BigInteger.valueOf" int64ToInteger#
  :: Int64# -> Integer#

-- TODO: Is this correct?
foreign import java unsafe "@static java.math.BigInteger.valueOf" word64ToInteger#
  :: Word64# -> Integer#

foreign import java unsafe "longValue" integerToInt64#
  :: Integer# -> Int64#

-- TODO: Is this correct?
foreign import java unsafe "longValue" integerToWord64#
  :: Integer# -> Word64#

foreign import java unsafe "intValue" integer2Int#
  :: Integer# -> Int#

-- TODO: Is this correct?
foreign import java unsafe "intValue" integer2Word#
  :: Integer# -> Word#

foreign import java unsafe "floatValue"  integer2Float#
  :: Integer# -> Float#

foreign import java unsafe "doubleValue" integer2Double#
  :: Integer# -> Double#

foreign import java unsafe "@static eta.integer.Utils.encodeFloat"
        encodeFloat# :: Integer# -> Int# -> Float#

foreign import java unsafe "@static eta.integer.Utils.int_encodeFloat"
        int_encodeFloat# :: Int# -> Int# -> Float#

foreign import java unsafe "@static eta.integer.Utils.encodeDouble"
        encodeDouble# :: Integer# -> Int# -> Double#

foreign import java unsafe "@static eta.integer.Utils.int_encodeDouble"
        int_encodeDouble# :: Int# -> Int# -> Double#
