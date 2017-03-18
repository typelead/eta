{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Concurrent
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Concurrent utilities
--
-----------------------------------------------------------------------------

module Java.Concurrent where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Primitive
import Java.Utils

-- Start java.util.concurrent.Executor

data {-# CLASS "java.concurrent.Executor" #-} Executor = Executor (Object# Executor)
  deriving Class

foreign import java unsafe "@interface" execute :: (a <: Executor) => Runnable -> Java a ()

-- End java.util.concurrent.Executor

-- Start java.lang.Runnable

data {-# CLASS "java.lang.Runnable" #-} Runnable = Runnable (Object# Runnable)
  deriving Class

foreign import java unsafe "@interface" run :: Java Runnable ()

-- End java.lang.Runnable
