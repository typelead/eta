{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeFamilies,
             UnboxedTuples, BangPatterns, FlexibleInstances,
             FlexibleContexts, UndecidableInstances, DefaultSignatures,
             ScopedTypeVariables #-}
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
    JArray(..),
    alength,
    toList,
    fromList,
  )
where

import GHC.Base
import GHC.List
import GHC.Num
import Java.Core

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

class (Class c) => JArray c where
  type JElem c :: *
  anew :: Int -> Java a c

  default anew :: (Class (JElem c)) => Int -> Java a c
  {-# INLINE anew #-}
  anew (I# n#) = Java $ \o ->
    case obj (jobjectArrayNew# (proxy# :: Proxy# (JElem c)) n#) of
      c -> (# o, c #)

  aget :: Int -> Java c (JElem c)
  default aget :: (Class (JElem c)) => Int -> Java c (JElem c)
  {-# INLINE aget #-}
  aget (I# n#) = Java $ \o ->
    case jobjectArrayAt# o n# realWorld# of
      (# _, o' #) -> case obj o' of
        o'' -> (# o, o'' #)

  aset :: Int -> JElem c -> Java c ()
  default aset :: (Class (JElem c)) => Int -> JElem c -> Java c ()
  {-# INLINE aset #-}
  aset (I# n#) e = Java $ \o ->
    case jobjectArraySet# o n# (unobj e) realWorld# of
      _ -> (# o, () #)

instance JArray JStringArray where
  type JElem JStringArray = JString

alength :: JArray c => Java c Int
alength = Java $ \o -> (# o, I# (alength# o) #)

toList :: JArray c => Java c [JElem c]
toList = do
  len <- alength
  go (len - 1) []
  where go n xs
          | n >= 0 = do
            x  <- aget n
            go (n - 1) (x:xs)
          | otherwise = return xs

fromList :: JArray c => [JElem c] -> Java a c
fromList xs = do
  jarray <- anew (length xs)
  jarray <.> go 0 xs
  return jarray
  where go _  []     = return ()
        go !n (x:xs) = aset n x >> go (n + 1) xs
