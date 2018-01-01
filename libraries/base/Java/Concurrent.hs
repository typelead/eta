{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, AllowAmbiguousTypes #-}
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
import Java.PrimitiveBase
import Java.Utils

-- Start java.util.concurrent.Executor

data {-# CLASS "java.util.concurrent.Executor" #-} Executor = Executor (Object# Executor)
  deriving Class

foreign import java unsafe "@interface" execute :: (a <: Executor) => Runnable -> Java a ()

-- End java.util.concurrent.Executor

-- Start java.lang.Runnable

data {-# CLASS "java.lang.Runnable" #-} Runnable = Runnable (Object# Runnable)
  deriving Class

foreign import java unsafe "@interface" run :: Java Runnable ()

-- End java.lang.Runnable

-- Start java.util.concurrent.Future

data {-# CLASS "java.util.concurrent.Future" #-} Future v = Future (Object# (Future v))
  deriving Class

foreign import java unsafe "@interface cancel" cancel :: (v <: Object, b <: (Future v)) => Bool -> Java b Bool

foreign import java unsafe "@interface get" get :: (v <: Object, b <: (Future v)) => Java b v

foreign import java unsafe "@interface get" getTimeUnit :: (v <: Object, b <: (Future v)) => Int64 -> TimeUnit -> Java b v

foreign import java unsafe "@interface isCancelled" isCancelled :: (v <: Object, b <: (Future v)) => Java b Bool

foreign import java unsafe "@interface isDone" isDone :: (v <: Object, b <: (Future v)) => Java b Bool

-- End java.util.concurrent.Future

-- Start java.util.concurrent.TimeUnit

data {-# CLASS "java.util.concurrent.TimeUnit" #-} TimeUnit = TimeUnit (Object# TimeUnit)
  deriving Class

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.DAYS"
  timeUnitDAYS :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.HOURS"
  timeUnitHOURS :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.MICROSECONDS"
  timeUnitMICROSECONDS :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.MILLISECONDS"
  timeUnitMILLISECONDS :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.MINUTES"
  timeUnitMINUTES :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.NANOSECONDS"
  timeUnitNANOSECONDS :: TimeUnit

foreign import java unsafe "@static @field java.util.concurrent.TimeUnit.SECONDS"
  timeUnitSECONDS :: TimeUnit

-- End java.util.concurrent.TimeUnit

-- Start java.util.concurrent.ExecutorService

data ExecutorService = ExecutorService @java.util.concurrent.ExecutorService
  deriving Class

-- End java.util.concurrent.ExecutorService
