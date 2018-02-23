{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             FlexibleInstances, TypeOperators, DataKinds, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Wrappers
-- Copyright   :  (c) Rahul Muttineni 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Working with the Java wrappers.
--
-----------------------------------------------------------------------------

module Java.Wrappers
  ( JBoolean
  , JByte
  , JShort
  , JCharacter
  , JInteger
  , JNumber
  , JLong
  , JFloat
  , JDouble
  , ToJava(..)
  , FromJava(..) )
where

import GHC.Base
import GHC.Show
import GHC.Int
import GHC.Real
import Java.Core
import Java.PrimitiveBase
import Java.StringBase
import Java.Array

data {-# CLASS "java.lang.Boolean" #-} JBoolean = JBoolean (Object# JBoolean)
  deriving (Class, Eq, Show)

foreign import java unsafe "@static @field java.lang.Boolean.TRUE"
  tRUE :: JBoolean
foreign import java unsafe "@static @field java.lang.Boolean.FALSE"
  fALSE :: JBoolean

-- JBoolean <=> Bool

instance ToJava Bool JBoolean where
  toJava True = tRUE
  toJava False = fALSE

instance FromJava Bool JBoolean where
  fromJava x
    | x == tRUE = True
    | otherwise = False

data {-# CLASS "java.lang.Byte" #-} JByte = JByte (Object# JByte)
  deriving (Class, Eq, Show)

type instance Inherits JByte = '[JNumber]

foreign import java unsafe "@new" toJByte :: Byte -> JByte


-- Integral a <=> JByte

instance Integral a => ToJava a JByte where
  toJava byte = toJByte (fromIntegral byte)
instance Integral a => FromJava a JByte where
  fromJava byte = fromIntegral (byteValue byte)

data {-# CLASS "java.lang.Short" #-} JShort = JShort (Object# JShort)
  deriving (Class, Eq, Show)

type instance Inherits JShort = '[JNumber]

foreign import java unsafe "@new" toJShort :: Short -> JShort

-- Integral a <=> JShort

instance Integral a => ToJava a JShort where
  toJava short = toJShort (fromIntegral short)
instance Integral a => FromJava a JShort where
  fromJava short = fromIntegral (shortValue short)

data {-# CLASS "java.lang.Character" #-} JCharacter = JCharacter (Object# JCharacter)
  deriving (Class, Eq, Show)

data {-# CLASS "java.lang.Integer" #-} JInteger = JInteger (Object# JInteger)
  deriving (Class, Eq, Show)

type instance Inherits JInteger = '[JNumber]

foreign import java unsafe "@new" toJInteger :: Int -> JInteger

-- Integral a <=> JInteger

instance Integral a => ToJava a JInteger where
  toJava int = toJInteger (fromIntegral int)
instance Integral a => FromJava a JInteger where
  fromJava int = fromIntegral (intValue int)

data {-# CLASS "java.lang.Long" #-} JLong = JLong (Object# JLong)
  deriving (Class, Eq, Show)

type instance Inherits JLong = '[JNumber]

foreign import java unsafe "@new" toJLong :: Int64 -> JLong
foreign import java unsafe longValue :: JLong -> Int64

-- Integral a <=> JLong

instance Integral a => ToJava a JLong where
  toJava long = toJLong (fromIntegral long)
instance Integral a => FromJava a JLong where
  fromJava long = fromIntegral (longValue long)

data {-# CLASS "java.lang.Float" #-} JFloat = JFloat (Object# JFloat)
  deriving (Class, Eq, Show)

type instance Inherits JFloat = '[JNumber]

foreign import java unsafe "@new" toJFloat :: Float -> JFloat

-- Float <=> JFloat

instance ToJava Float JFloat where
  toJava   = toJFloat

instance FromJava Float JFloat where
  fromJava = floatValue

data {-# CLASS "java.lang.Double" #-} JDouble = JDouble (Object# JDouble)
  deriving (Class, Eq, Show)

type instance Inherits JDouble = '[JNumber]

foreign import java unsafe "@new" toJDouble :: Double -> JDouble

-- Double <=> JDouble 

instance ToJava Double JDouble where
  toJava   = toJDouble

instance FromJava Double JDouble where  
  fromJava = doubleValue

-- Start java.lang.Number

data {-# CLASS "java.lang.Number" #-} JNumber = JNumber (Object# JNumber)
  deriving (Class, Eq, Show)

foreign import java unsafe byteValue :: (b <: JNumber) => b -> Byte

foreign import java unsafe doubleValue :: (b <: JNumber) => b -> Double

foreign import java unsafe floatValue :: (b <: JNumber) => b -> Float

foreign import java unsafe intValue :: (b <: JNumber) => b -> Int

foreign import java unsafe longalue :: (b <: JNumber) => b -> Int64

foreign import java unsafe shortValue :: (b <: JNumber) => b -> Short

-- End java.lang.Number
