{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             UnboxedTuples, BangPatterns, FlexibleInstances,
             FlexibleContexts, UndecidableInstances, DefaultSignatures,
             DeriveAnyClass, FunctionalDependencies, StandaloneDeriving,
             ScopedTypeVariables #-}
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
    JObjectArray(..),
    JArray(..),
    alength,
    arrayToList,
    arrayFromList
  )
where

import GHC.Base
import GHC.Int
import GHC.List
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Word
import Java.Core
import Java.PrimitiveBase
import Java.Utils

class (Class c) => JArray e c | c -> e, e -> c where
  anew :: Int -> Java a c

  default anew :: forall a. (Class e) => Int -> Java a c
  {-# INLINE anew #-}
  anew (I# n#) = Java $ \o ->
    case getClass (Proxy :: Proxy e) of
      JClass c# -> case runRW# (\s -> jobjectArrayNew# n# c# s) of
        (# _, o' #) -> case obj o' of
          c -> (# o, c #)

  aget :: Int -> Java c e
  default aget :: (Class e) => Int -> Java c e
  {-# INLINE aget #-}
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> jobjectArrayAt# o n# s) of
      (# _, o' #) -> case obj o' of
        o'' -> (# o, o'' #)

  aset :: Int -> e -> Java c ()
  default aset :: (Class e) => Int -> e -> Java c ()
  {-# INLINE aset #-}
  aset (I# n#) e = Java $ \o ->
    case runRW# (\s -> jobjectArraySet# o n# (unobj e) s) of
      _ -> (# o, () #)

data {-# CLASS "boolean[]" #-} JBooleanArray = JBooleanArray (Object# JBooleanArray)
  deriving (Class, Show)

instance JArray Bool JBooleanArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJBooleanArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJBooleanArray# o n# s) of
      (# _, i# #) -> (# o, isTrue# i# #)
  aset (I# n#) b = Java $ \o ->
    case runRW# (\s -> writeJBooleanArray# o n# (dataToTag# b) s) of
      _ -> (# o, () #)

deriving instance Class JByteArray

instance JArray Byte JByteArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJByteArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJByteArray# o n# s) of
      (# _, b #) -> (# o, B# b #)
  aset (I# n#) (B# e#) = Java $ \o ->
    case runRW# (\s -> writeJByteArray# o n# e# s) of
      _ -> (# o, () #)

instance JavaConverter [Word8] JByteArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Byte]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

instance JavaConverter [Int8] JByteArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Byte]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

data {-# CLASS "char[]"    #-} JCharArray    = JCharArray    (Object# JCharArray)
  deriving (Class, Show)

instance JArray JChar JCharArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJCharArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJCharArray# o n# s) of
      (# _, e# #) -> (# o, JC# e# #)
  aset (I# n#) (JC# e#) = Java $ \o ->
    case runRW# (\s -> writeJCharArray# o n# e# s) of
      _ -> (# o, () #)

data {-# CLASS "short[]"   #-} JShortArray   = JShortArray   (Object# JShortArray)
  deriving (Class, Show)

instance JArray Short JShortArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJShortArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJShortArray# o n# s) of
      (# _, e# #) -> (# o, S# e# #)
  aset (I# n#) (S# e#) = Java $ \o ->
    case runRW# (\s -> writeJShortArray# o n# e# s) of
      _ -> (# o, () #)

instance JavaConverter [Word16] JShortArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Short]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

instance JavaConverter [Int16] JShortArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Short]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

data {-# CLASS "int[]"     #-} JIntArray     = JIntArray     (Object# JIntArray)
  deriving (Class, Show)

instance JArray Int JIntArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJIntArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJIntArray# o n# s) of
      (# _, e# #) -> (# o, I# e# #)
  aset (I# n#) (I# e#) = Java $ \o ->
    case runRW# (\s -> writeJIntArray# o n# e# s) of
      _ -> (# o, () #)

instance JavaConverter [Word32] JIntArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Int]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

instance JavaConverter [Int32] JIntArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Int]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

data {-# CLASS "long[]"    #-} JLongArray    = JLongArray    (Object# JLongArray)
  deriving (Class, Show)

instance JArray Int64 JLongArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJLongArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJLongArray# o n# s) of
      (# _, e# #) -> (# o, I64# e# #)
  aset (I# n#) (I64# e#) = Java $ \o ->
    case runRW# (\s -> writeJLongArray# o n# e# s) of
      _ -> (# o, () #)

instance JavaConverter [Word64] JLongArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map fromIntegral ws :: [Int64]
  fromJava ba = map fromIntegral $ unsafePerformJavaWith ba arrayToList

data {-# CLASS "float[]"   #-} JFloatArray   = JFloatArray   (Object# JFloatArray)
  deriving (Class, Show)

instance JArray Float JFloatArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJFloatArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJFloatArray# o n# s) of
      (# _, e# #) -> (# o, F# e# #)
  aset (I# n#) (F# e#) = Java $ \o ->
    case runRW# (\s -> writeJFloatArray# o n# e# s) of
      _ -> (# o, () #)

data {-# CLASS "double[]"  #-} JDoubleArray  = JDoubleArray  (Object# JDoubleArray)
  deriving (Class, Show)

instance JArray Double JDoubleArray where
  anew (I# n#) = Java $ \o ->
    case runRW# (\s -> newJDoubleArray# n# s) of
      (# _, o' #) -> case obj o' of
        c -> (# o, c #)
  aget (I# n#) = Java $ \o ->
    case runRW# (\s -> readJDoubleArray# o n# s) of
      (# s1, e# #) -> (# o, D# e# #)

  aset (I# n#) (D# e#) = Java $ \o ->
    case runRW# (\s -> writeJDoubleArray# o n# e# s) of
      _ -> (# o, () #)

data {-# CLASS "java.lang.Object[]" #-} JObjectArray = JObjectArray (Object# JObjectArray)
  deriving (Class, Show)

instance JArray Object JObjectArray

{-# INLINE alength #-}
alength :: JArray e c => Java c Int
alength = Java $ \o -> (# o, I# (alength# o) #)

{-# INLINE arrayToList #-}
arrayToList :: JArray e c => Java c [e]
arrayToList = do
  len <- alength
  go (len - 1) []
  where go n xs
          | n >= 0 = do
            x  <- aget n
            go (n - 1) (x:xs)
          | otherwise = return xs

{-# INLINE arrayFromList #-}
arrayFromList :: JArray e c => [e] -> Java a c
arrayFromList xs = do
  jarray <- anew (length xs)
  jarray <.> go 0 xs
  return jarray
  where go _  []     = return ()
        go !n (x:xs) = aset n x >> go (n + 1) xs

instance {-# OVERLAPS #-} (JArray e c) => JavaConverter [e] c where
  toJava  xs = unsafePerformJava $ arrayFromList xs
  fromJava c = unsafePerformJavaWith c arrayToList
  {-# INLINE toJava #-}
  {-# INLINE fromJava #-}
