{-# LANGUAGE NoImplicitPrelude, MagicHash, ScopedTypeVariables, KindSignatures,
             UnboxedTuples, FlexibleContexts, UnliftedFFITypes #-}
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
  ( JClass, getClass, toString, equals, classObject, hashCode, Proxy(..), eqObject#, toString#)
where

import GHC.Base
import Java.String
import Data.Proxy

data {-# CLASS "java.lang.Class" #-} JClass a = JClass (Object# (JClass a))
  deriving Class

{-# INLINE getClass #-}
getClass :: forall (a :: *). Proxy a -> JClass a
getClass _ = JClass (getClass# (proxy# :: Proxy# a))

foreign import java unsafe classObject :: (Extends a Object) => a -> JClass a
foreign import java unsafe toString    :: (Extends a Object) => a -> JString
foreign import java unsafe hashCode    :: (Extends a Object) => a -> Int

foreign import java unsafe equals :: (Extends a Object, Extends b Object)
                                  => a -> b -> Bool

foreign import java unsafe "equals" eqObject# :: Object# a -> Object# b -> Bool
foreign import java unsafe "toString" toString# :: Object# a -> String
