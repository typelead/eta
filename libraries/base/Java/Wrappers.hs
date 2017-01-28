{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             FlexibleInstances #-}
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
  , JLong
  , JFloat
  , JDouble
  , JavaConverter(..) )
where

import GHC.Base
import GHC.Show
import GHC.Int
import GHC.Real
import Java.Primitive
import Java.String

class JavaConverter a b where
  toJava   :: a -> b
  fromJava :: b -> a

data {-# CLASS "java.lang.Boolean" #-} JBoolean = JBoolean (Object# JBoolean)
  deriving (Class, Eq, Show)

foreign import java unsafe "@static @field java.lang.Boolean.TRUE"
  tRUE :: JBoolean
foreign import java unsafe "@static @field java.lang.Boolean.FALSE"
  fALSE :: JBoolean

instance JavaConverter Bool JBoolean where
  toJava True = tRUE
  toJava False = fALSE
  fromJava x
    | x == tRUE = True
    | otherwise = False

data {-# CLASS "java.lang.Byte" #-} JByte = JByte (Object# JByte)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJByte :: Byte -> JByte
foreign import java unsafe byteValue :: JByte -> Byte

instance Integral a => JavaConverter a JByte where
  toJava byte = toJByte (fromIntegral byte)
  fromJava byte = fromIntegral (byteValue byte)

data {-# CLASS "java.lang.Short" #-} JShort = JShort (Object# JShort)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJShort :: Short -> JShort
foreign import java unsafe shortValue :: JShort -> Short

instance Integral a => JavaConverter a JShort where
  toJava short = toJShort (fromIntegral short)
  fromJava short = fromIntegral (shortValue short)

data {-# CLASS "java.lang.Character" #-} JCharacter = JCharacter (Object# JCharacter)
  deriving (Class, Eq, Show)

data {-# CLASS "java.lang.Integer" #-} JInteger = JInteger (Object# JInteger)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJInteger :: Int -> JInteger
foreign import java unsafe intValue :: JInteger -> Int

instance Integral a => JavaConverter a JInteger where
  toJava int = toJInteger (fromIntegral int)
  fromJava int = fromIntegral (intValue int)

data {-# CLASS "java.lang.Long" #-} JLong = JLong (Object# JLong)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJLong :: Int64 -> JLong
foreign import java unsafe longValue :: JLong -> Int64

instance Integral a => JavaConverter a JLong where
  toJava long = toJLong (fromIntegral long)
  fromJava long = fromIntegral (longValue long)

data {-# CLASS "java.lang.Float" #-} JFloat = JFloat (Object# JFloat)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJFloat :: Float -> JFloat
foreign import java unsafe floatValue :: JFloat -> Float

instance JavaConverter Float JFloat where
  toJava   = toJFloat
  fromJava = floatValue

data {-# CLASS "java.lang.Double" #-} JDouble = JDouble (Object# JDouble)
  deriving (Class, Eq, Show)

foreign import java unsafe "@new" toJDouble :: Double -> JDouble
foreign import java unsafe doubleValue :: JDouble -> Double

instance JavaConverter Double JDouble where
  toJava   = toJDouble
  fromJava = doubleValue

instance JavaConverter String JString where
  toJava   = toJString
  fromJava = fromJString
