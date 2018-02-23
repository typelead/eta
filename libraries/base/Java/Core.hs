{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
             FunctionalDependencies, ScopedTypeVariables, ExplicitNamespaces,
             UnliftedFFITypes, FlexibleInstances, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Core
-- Copyright   :  (c) Rahul Muttineni 2016-2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The core facilities to help with Java FFI usage.
--
-----------------------------------------------------------------------------

module Java.Core
  ( java, javaWith, unsafePerformJava, unsafePerformJavaWith, io, withObject
  , maybeToJava, maybeFromJava
  , (<.>), (>-)
  -- Useful exports
  , Int64
  , Object#
  , Object
  , JString
  , Class(..)
  , Extends(..)
  , type (<:)
  , Inherits(..)
  , Java(..)
  , JavaConverter(..)
  , withThis
  )
where

import GHC.Base
import Data.Int(Int64)
import GHC.IO(unsafePerformIO)

foreign import java unsafe "@new" globalObject :: Object

{-# INLINE java #-}
java :: (forall c. Java c a) -> IO a
java (Java m) = IO $ \s ->
  case m (freshNullObjectToken# s) of
      (# o1, a #) -> (# freshStateToken# o1, a #)

{-# INLINE javaWith #-}
javaWith :: (Class c) => c -> Java c a -> IO a
javaWith c (Java m) = IO $ \s ->
  case m (freshObjectToken# s (unobj c)) of
    (# o1, a #) -> (# freshStateToken# o1, a #)

{-# INLINE unsafePerformJava #-}
unsafePerformJava :: (forall c. Java c a) -> a
unsafePerformJava action = unsafePerformIO (java action)

{-# INLINE unsafePerformJavaWith #-}
unsafePerformJavaWith :: (Class c) => c -> Java c a -> a
unsafePerformJavaWith c action = unsafePerformIO (javaWith c action)

{-# INLINE (<.>) #-}
(<.>) :: (Class c) => c -> Java c a -> Java b a
(<.>) cls (Java m) = Java $ \o ->
  case m (freshObjectToken# o (unobj cls)) of
    (# o1, a #) -> (# freshObjectToken# o1 o, a #)

{-# INLINE withObject #-}
withObject :: (Class c) => c -> Java c a -> Java b a
withObject = (<.>)

{-# INLINE io #-}
io :: IO a -> Java c a
io (IO m) = Java $ \o ->
  case m (freshStateToken# o) of
    (# o1, a #) -> (# freshObjectToken# o1 o, a #)

{-# INLINE (>-) #-}
(>-) :: (Class b) => Java a b -> Java b c -> Java a c
(>-) (Java m) (Java n) =
  Java $ \a ->
           case m a of
             (# a', b #) ->
               case n (unobj b) of
                 (# _, c #) -> (# a', c #)

class JavaConverter a b where
  toJava   :: a -> b
  fromJava :: b -> a

maybeToJava :: (JavaConverter a b, Class b) => Maybe a -> b
maybeToJava (Just x) = toJava x
maybeToJava Nothing  = obj (unsafeCoerce# nullAddr#)

maybeFromJava :: (JavaConverter a b, Class b) => b -> Maybe a
maybeFromJava x = case isNullObject# (unobj x) of
  0# -> Just (fromJava x)
  _  -> Nothing

withThis :: (Class a) => (a -> Java a b) -> Java a b
withThis f = Java $ \a -> unJava (f (obj a)) a
