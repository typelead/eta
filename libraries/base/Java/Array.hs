{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             UnboxedTuples, BangPatterns, FlexibleInstances,
             FlexibleContexts, UndecidableInstances, DefaultSignatures,
             DeriveAnyClass, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Array
-- Copyright   :  (c) Rahul Muttineni 2016-2017
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
  deriving Class
data {-# CLASS "char[]"    #-} JCharArray    = JCharArray    (Object# JCharArray)
  deriving Class
data {-# CLASS "short[]"   #-} JShortArray   = JShortArray   (Object# JShortArray)
  deriving Class
data {-# CLASS "int[]"     #-} JIntArray     = JIntArray     (Object# JIntArray)
  deriving Class
data {-# CLASS "long[]"    #-} JLongArray    = JLongArray    (Object# JLongArray)
  deriving Class
data {-# CLASS "float[]"   #-} JFloatArray   = JFloatArray   (Object# JFloatArray)
  deriving Class
data {-# CLASS "double[]"  #-} JDoubleArray  = JDoubleArray  (Object# JDoubleArray)
  deriving Class
data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)
  deriving Class

instance JArray JString JStringArray

-- We provide a manual instance here since it is originally defined in ghc-prim.
instance Class JByteArray where
  unobj (JByteArray o) = o
  obj = JByteArray

class (Class c) => JArray e c | c -> e where
  anew :: Int -> Java a c

  default anew :: (Class e) => Int -> Java a c
  {-# INLINE anew #-}
  anew (I# n#) = Java $ \o ->
    case jobjectArrayNew# n# realWorld# of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)

  aget :: Int -> Java c e
  default aget :: (Class e) => Int -> Java c e
  {-# INLINE aget #-}
  aget (I# n#) = Java $ \o ->
    case jobjectArrayAt# o n# realWorld# of
      (# _, o' #) -> case obj o' of
        o'' -> (# o, o'' #)

  aset :: Int -> e -> Java c ()
  default aset :: (Class e) => Int -> e -> Java c ()
  {-# INLINE aset #-}
  aset (I# n#) e = Java $ \o ->
    case jobjectArraySet# o n# (unobj e) realWorld# of
      _ -> (# o, () #)

{-# INLINE alength #-}
alength :: JArray e c => Java c Int
alength = Java $ \o -> (# o, I# (alength# o) #)

{-# INLINE toList #-}
toList :: JArray e c => Java c [e]
toList = do
  len <- alength
  go (len - 1) []
  where go n xs
          | n >= 0 = do
            x  <- aget n
            go (n - 1) (x:xs)
          | otherwise = return xs

{-# INLINE fromList #-}
fromList :: JArray e c => [e] -> Java a c
fromList xs = do
  jarray <- anew (length xs)
  jarray <.> go 0 xs
  return jarray
  where go _  []     = return ()
        go !n (x:xs) = aset n x >> go (n + 1) xs
