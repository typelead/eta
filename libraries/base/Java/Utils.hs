{-# LANGUAGE NoImplicitPrelude, MagicHash, ScopedTypeVariables, KindSignatures,
             UnboxedTuples, FlexibleContexts, UnliftedFFITypes, TypeOperators,
             AllowAmbiguousTypes, DataKinds, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Utils
-- Copyright   :  (c) Rahul Muttineni 2016
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The utility functions for the Java FFI.
--
-----------------------------------------------------------------------------

module Java.Utils
  ( JClass(..)
  , getClass
  , toString
  , equals
  , instanceOf
  , classObject
  , hashCode
  , Proxy(..)
  , eqObject#
  , toString#
  , safeDowncast
  , Void
  , Comparator(..)
  , Comparable(..)
  , Enum(..) )
where

import GHC.Base
import Data.Proxy
import Java.StringBase (fromJString, toJString)

data {-# CLASS "java.lang.Class" #-} JClass a = JClass (Object# (JClass a))
  deriving Class

getClass :: forall a. Class a => Proxy a -> JClass a
getClass _ = forName (classIdentifier (proxy# :: Proxy# a))

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> JClass a

foreign import java unsafe "getClass" classObject :: (a <: Object) => a -> JClass a
foreign import java unsafe toString    :: (a <: Object) => a -> JString
foreign import java unsafe hashCode    :: (a <: Object) => a -> Int

foreign import java unsafe equals :: (a <: Object, b <: Object)
                                  => a -> b -> Bool

foreign import java unsafe "equals" eqObject# :: Object# a -> Object# b -> Bool
foreign import java unsafe "toString" toString# :: Object# a -> String

foreign import java unsafe "@static eta.base.Utils.convertInstanceOfObject"
  castObject :: (t <: Object, o <: Object) => o -> JClass t -> Maybe t

foreign import java unsafe "@static eta.base.Utils.instanceOf"
  instanceOf :: (o <: Object) => o -> JClass t -> Bool

{-# INLINE safeDowncast #-}
safeDowncast :: forall a b. (Class a, Class b) => a -> Maybe b
safeDowncast x = castObject x (getClass (Proxy :: Proxy b))

-- Start java.lang.Void

data {-# CLASS "java.lang.Void" #-} Void = Void (Object# Void)
  deriving Class

-- End java.lang.Void

-- Start java.util.Comparator

data {-# CLASS "java.util.Comparator" #-} Comparator t = Comparator (Object# (Comparator t))
  deriving Class

foreign import java unsafe "@interface compare"
  compare :: (t <: Object, b <: (Comparator t)) => t -> t -> Java b Int

foreign import java unsafe "@interface equals"
  equalsComparator :: (t <: Object, b <: (Comparator t)) => Object -> Java b Bool

-- End java.util.Comparator

-- Start java.lang.Enum

data {-# CLASS "java.lang.Enum" #-} Enum e = Enum (Object# (Enum e))
  deriving Class

type instance Inherits (Enum e) = '[Object, Comparable e]

foreign import java unsafe getDeclaringClass :: (e <: Enum e) => Java e (JClass e)

foreign import java unsafe name :: (e <: Enum e) => Java e String

foreign import java unsafe ordinal :: (e <: Enum e) => Java e Int

-- End java.lang.Enum
