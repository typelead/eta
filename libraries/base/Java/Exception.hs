{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
             DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses,
             ScopedTypeVariables, FlexibleInstances, UndecidableInstances,
             BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Execption
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Execption utilities
--
-----------------------------------------------------------------------------

module Java.Exception where

import GHC.Base
import GHC.Int
import Java
import Java.Array
import GHC.Show
import GHC.Exception
import Data.Typeable (Typeable, cast)

data {-# CLASS "java.lang.StackTraceElement[]" #-} StackTraceElementArray = StackTraceElementArray (Object# StackTraceElementArray)
  deriving Class

instance JArray StackTraceElement StackTraceElementArray

-- Start java.lang.Throwable

data {-# CLASS "java.lang.Throwable" #-} Throwable = Throwable (Object# Throwable)
  deriving Class

foreign import java unsafe fillInStackTrace :: Java Throwable Throwable

foreign import java unsafe getCause :: Java Throwable Throwable

foreign import java unsafe getLocalizedMessage :: Java Throwable String

foreign import java unsafe getMessage :: Java Throwable String

foreign import java unsafe getStackTrace :: Java Throwable StackTraceElementArray

foreign import java unsafe initCause :: Throwable -> Java Throwable Throwable

foreign import java unsafe printStackTrace :: Java Throwable ()

foreign import java unsafe setStackTrace :: StackTraceElementArray -> Java Throwable ()

-- End java.lang.Throwable

-- Start java.lang.StackTraceElement

data {-# CLASS "java.lang.StackTraceElement" #-} StackTraceElement = StackTraceElement (Object# StackTraceElement)
  deriving Class

foreign import java unsafe getClassName :: Java StackTraceElement String

foreign import java unsafe getFileName :: Java StackTraceElement String

foreign import java unsafe getLineNumber :: Java StackTraceElement Int

foreign import java unsafe getMethodName :: Java StackTraceElement String

foreign import java unsafe isNativeMethod :: Java StackTraceElement Bool

-- End java.lang.StackTraceElement

-- Start java.lang.Exception

data {-# CLASS "java.lang.Exception" #-} JException = JException (Object# JException)
  deriving (Class, Show, Typeable)

type instance Inherits JException = '[Throwable]

instance Exception JException

-- End java.lang.Exception

-- Start java.lang.Error

data {-# CLASS "java.lang.Error" #-} Error = Error (Object# Error)
  deriving Class

type instance Inherits Error = '[Throwable]

-- End java.lang.Error

-- Start java.lang.VirtualMachineError

data {-# CLASS "java.lang.VirtualMachineError" #-}
  VirtualMachineError = VirtualMachineError (Object# VirtualMachineError)
  deriving Class

type instance Inherits VirtualMachineError = '[Error]

-- End java.lang.VirtualMachineError

-- Start java.lang.InternalError

data {-# CLASS "java.lang.InternalError" #-}
  InternalError = InternalError (Object# InternalError)
  deriving Class

type instance Inherits InternalError = '[VirtualMachineError]

-- End java.lang.InternalError

-- Start java.io.IOException

data {-# CLASS "java.io.IOException" #-} IOException = IOException (Object# IOException)
  deriving (Class, Show, Typeable)

type instance Inherits IOException = '[JException]

-- End java.io.IOException

instance {-# OVERLAPPABLE #-} (Show a, Typeable a, a <: JException)
  => Exception a where
  toException x = SomeException (JException (unsafeCoerce# (unobj x)))
  fromException e = do
    jexception :: JException <- fromException e
    safeDowncast jexception
  {-# INLINE fromException #-}

showException :: SomeException -> Object# JString
showException e = s#
  where !(JS# s#) = toJString (displayException e)

  -- Start java.lang.ReflectiveOperationException

data ReflectiveOperationException = ReflectiveOperationException @java.lang.ReflectiveOperationException
  deriving Class

type instance Inherits ReflectiveOperationException = '[JException]

  -- End java.lang.ReflectiveOperationException
