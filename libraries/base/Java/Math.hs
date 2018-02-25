{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Math
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Math utilities
--
-----------------------------------------------------------------------------

module Java.Math where

import GHC.Base
import GHC.Int
import Java.Array
import Java.PrimitiveBase
import Java.Wrappers

-- Start java.math.BigDecimal

data {-# CLASS "java.math.BigDecimal" #-} BigDecimal = BigDecimal (Object# BigDecimal)
  deriving Class

data {-# CLASS "java.math.BigDecimal[]" #-} BigDecimalArray = BigDecimalArray (Object# BigDecimalArray)
  deriving Class

instance JArray BigDecimal BigDecimalArray

foreign import java unsafe abs :: BigDecimal -> BigDecimal

foreign import java unsafe "abs" absMathContext :: BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe add :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "add"
  addMathContext :: BigDecimal -> BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe byteValueExact :: BigDecimal -> Byte

foreign import java unsafe compareTo :: BigDecimal -> BigDecimal -> Int

foreign import java unsafe divide :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "divide" divide2 :: BigDecimal -> BigDecimal -> Int -> BigDecimal

foreign import java unsafe "divide" divide3 :: BigDecimal -> BigDecimal -> Int -> Int -> BigDecimal

foreign import java unsafe "divide"
  divide4 :: BigDecimal -> BigDecimal -> Int -> RoundingMode -> BigDecimal

foreign import java unsafe "divide" divide5 :: BigDecimal -> BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe "divide" divide6 :: BigDecimal -> BigDecimal -> RoundingMode -> BigDecimal

foreign import java unsafe divideAndRemainder :: BigDecimal -> BigDecimal -> BigDecimalArray

foreign import java unsafe "divideAndRemainder"
  divideAndRemainder2 :: BigDecimal -> BigDecimal -> MathContext -> BigDecimalArray

foreign import java unsafe divideToIntegralValue :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "divideToIntegralValue"
  divideToIntegralValue2 :: BigDecimal -> BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe doubleValue :: BigDecimal -> Double

foreign import java unsafe equals :: BigDecimal -> Object -> Bool

foreign import java unsafe floatValue :: BigDecimal -> Float

foreign import java unsafe hashCode :: BigDecimal -> Int

foreign import java unsafe intValue :: BigDecimal -> Int

foreign import java unsafe intValueExact :: BigDecimal -> Int

foreign import java unsafe longValue :: BigDecimal -> Int64

foreign import java unsafe longValueExact :: BigDecimal -> Int64

foreign import java unsafe max :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe min :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe movePointLeft :: BigDecimal -> Int -> BigDecimal

foreign import java unsafe movePointRight :: BigDecimal -> Int -> BigDecimal

foreign import java unsafe multiply :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "multiply"
  multiplyMathContext :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe negate :: BigDecimal -> BigDecimal

foreign import java unsafe "negate" negateMathContext :: BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe plus :: BigDecimal -> BigDecimal

foreign import java unsafe "plus" plusMathContext :: BigDecimal -> BigDecimal

foreign import java unsafe pow :: BigDecimal -> Int -> BigDecimal

foreign import java unsafe "pow" powMathContect :: BigDecimal -> Int -> MathContext -> BigDecimal

foreign import java unsafe precision :: BigDecimal -> Int

foreign import java unsafe remainder :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "remainder"
  remainderMathContext :: BigDecimal -> BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe round :: BigDecimal -> MathContext -> BigDecimal

foreign import java unsafe scale :: BigDecimal -> Int

foreign import java unsafe scaleByPowerOfTen :: BigDecimal -> Int -> BigDecimal

foreign import java unsafe setScale :: BigDecimal -> Int -> BigDecimal

foreign import java unsafe "setScale" setScale2 :: BigDecimal -> Int -> Int -> BigDecimal

foreign import java unsafe "setScale" setScale3 :: BigDecimal -> Int -> RoundingMode -> BigDecimal

foreign import java unsafe shortValueExact :: BigDecimal -> Short

foreign import java unsafe signum :: BigDecimal -> Int

foreign import java unsafe stripTrailingZeros :: BigDecimal -> BigDecimal

foreign import java unsafe subtract :: BigDecimal -> BigDecimal -> BigDecimal

foreign import java unsafe "subtract"
  subtractMathContext :: BigDecimal -> BigDecimal -> MathContext -> BigDecimal

-- foreign import java unsafe toBigInteger :: BigDecimal -> BigInteger
--
-- foreign import java unsafe toBigIntegerExact :: BigDecimal -> BigInteger

foreign import java unsafe toEngineeringString :: BigDecimal -> String

foreign import java unsafe toPlainString :: BigDecimal -> String

foreign import java unsafe toString :: BigDecimal -> String

foreign import java unsafe ulp :: BigDecimal -> BigDecimal

-- foreign import java unsafe unscaledValue :: BigDecimal -> BigInteger

-- End java.math.BigDecimal

-- Start java.math.MathContext

data {-# CLASS "java.math.MathContext" #-} MathContext = MathContext (Object# MathContext)
  deriving Class

foreign import java unsafe getPrecision :: MathContext -> Int

foreign import java unsafe getRoundingMode :: MathContext -> RoundingMode

foreign import java unsafe "hashCode" hashCodeMathContext :: MathContext -> Int

-- End java.math.MathContext

-- Start java.math.RoundingMode

data {-# CLASS "java.math.RoundingMode" #-} RoundingMode = RoundingMode (Object# RoundingMode)
  deriving Class

foreign import java unsafe "@static @field java.math.RoundingMode.CEILING" rmCEILING :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.DOWN" rmDOWN :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.FLOOR" rmFLOOR :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.HALF_DOWN" rmHALF_DOWN :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.HALF_EVEN" rmHALF_EVEN :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.HALF_UP" rmHALF_UP :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.UNNECESSARY" rmUNNECESSARY :: RoundingMode

foreign import java unsafe "@static @field java.math.RoundingMode.UP" rmUP :: RoundingMode

-- End java.math.RoundingMode

-- Start java.math.BigInteger

data BigInteger = BigInteger @java.math.BigInteger
  deriving Class

type instance Inherits BigInteger = '[JNumber, Object]

-- End java.math.BigInteger
