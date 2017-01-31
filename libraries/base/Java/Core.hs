{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
             FunctionalDependencies, ScopedTypeVariables, ExplicitNamespaces,
             UnliftedFFITypes, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Core
-- Copyright   :  (c) Rahul Muttineni 2016
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
  ( java, javaWith, pureJava, pureJavaWith, io, withObject
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
  , Byte
  , Short
  , JChar
  , JavaConverter(..)
  )
where

import GHC.Base
import Data.Int(Int64)
import Java.Primitive

foreign import java unsafe "@new" globalObject :: Object

{-# INLINE java #-}
java :: Java c a -> IO a
java (Java m) = IO $ \s ->
  case m (unsafeCoerce# (unobj globalObject)) of
    (# _, a #) -> (# s, a #)

{-# INLINE javaWith #-}
javaWith :: (Class c) => c -> Java c a -> IO a
javaWith c (Java m) = IO $ \s -> case m (unobj c) of (# _, a #) -> (# s, a #)

{-# INLINE pureJava #-}
pureJava :: Java c a -> a
pureJava (Java m) =
  case m (unsafeCoerce# (unobj globalObject)) of
    (# _, a #) -> a

{-# INLINE pureJavaWith #-}
pureJavaWith :: (Class c) => c -> Java c a -> a
pureJavaWith c (Java m) = case m (unobj c) of (# _, a #) -> a

{-# INLINE (<.>) #-}
(<.>) :: (Class c) => c -> Java c a -> Java b a
(<.>) cls (Java m) = Java $ \o -> case m (unobj cls) of (# _, a #) -> (# o, a #)

{-# INLINE withObject #-}
withObject :: (Class c) => c -> Java c a -> Java b a
withObject = (<.>)

{-# INLINE io #-}
io :: IO a -> Java c a
io (IO m) = Java $ \o -> case m realWorld# of (# _, a #) -> (# o, a #)

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
