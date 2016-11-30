{-# LANGUAGE NoImplicitPrelude, MagicHash, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Array
-- Copyright   :  (c) Rahul Muttineni 2016
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Dealing with native Java arrays.
--
-----------------------------------------------------------------------------

module Java.Array
  ( JByteArray(..),
    JShortArray(..),
    JCharArray(..),
    JIntArray(..),
    JLongArray(..),
    JFloatArray(..),
    JDoubleArray(..),
    JStringArray(..),
    JArray(..)
  )
where

import GHC.Base

data {-# CLASS "boolean[]" #-} JBooleanArray = JBooleanArray (Object# JBooleanArray)
data {-# CLASS "byte[]"    #-} JByteArray    = JByteArray    (Object# JByteArray)
data {-# CLASS "char[]"    #-} JCharArray    = JCharArray    (Object# JCharArray)
data {-# CLASS "short[]"   #-} JShortArray   = JShortArray   (Object# JShortArray)
data {-# CLASS "int[]"     #-} JIntArray     = JIntArray     (Object# JIntArray)
data {-# CLASS "long[]"    #-} JLongArray    = JLongArray    (Object# JLongArray)
data {-# CLASS "float[]"   #-} JFloatArray   = JFloatArray   (Object# JFloatArray)
data {-# CLASS "double[]"  #-} JDoubleArray  = JDoubleArray  (Object# JDoubleArray)

data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)

instance Class JBooleanArray where
  unobj (JBooleanArray o) = o
  obj = JBooleanArray

instance Class JByteArray where
  unobj (JByteArray o) = o
  obj = JByteArray

instance Class JCharArray where
  unobj (JCharArray o) = o
  obj = JCharArray

instance Class JShortArray where
  unobj (JShortArray o) = o
  obj = JShortArray

instance Class JIntArray where
  unobj (JIntArray o) = o
  obj = JIntArray

instance Class JLongArray where
  unobj (JLongArray o) = o
  obj = JLongArray

instance Class JFloatArray where
  unobj (JFloatArray o) = o
  obj = JFloatArray

instance Class JDoubleArray where
  unobj (JDoubleArray o) = o
  obj = JDoubleArray

instance Class JStringArray where
  unobj (JStringArray o) = o
  obj = JStringArray

class (Class c) => JArray e c | c -> e where
  jarrayNew :: Int -> Java a c
  jarrayAt  :: Int -> Java c e
  jarraySet :: Int -> e -> Java c ()
