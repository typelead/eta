{-# LANGUAGE NoImplicitPrelude, MagicHash, ScopedTypeVariables, KindSignatures,
             UnboxedTuples #-}
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
  ( JClass, getClass )
where

import GHC.Base
import Data.Proxy

data {-# CLASS "java.lang.Class" #-} JClass a = JClass (Object# (JClass a))
  deriving Class

{-# INLINE getClass #-}
getClass :: forall (a :: *). Proxy a -> JClass a
getClass _ = JClass (getClass# (proxy# :: Proxy# a))
