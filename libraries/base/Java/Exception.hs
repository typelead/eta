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
import qualified System.IO.Error as SysIOErr
import Data.Typeable (Typeable, cast)
import Data.List (any)

data {-# CLASS "java.lang.StackTraceElement[]" #-} StackTraceElementArray = StackTraceElementArray (Object# StackTraceElementArray)
  deriving Class

instance JArray StackTraceElement StackTraceElementArray

-- Start java.lang.Throwable

data {-# CLASS "java.lang.Throwable" #-} Throwable = Throwable (Object# Throwable)
  deriving Class

foreign import java unsafe fillInStackTrace :: (a <: Throwable) => Java a a

foreign import java unsafe getCause :: (a <: Throwable, b <:Throwable) => Java a b

foreign import java unsafe getLocalizedMessage :: (a <: Throwable) => Java a String

foreign import java unsafe getMessage :: (a <: Throwable) => Java a String

foreign import java unsafe getStackTrace ::  (a <: Throwable) => Java a StackTraceElementArray

foreign import java unsafe initCause :: (a <: Throwable, b <:Throwable) => a -> Java b b

foreign import java unsafe printStackTrace ::  (a <: Throwable) => Java a ()

foreign import java unsafe setStackTrace ::  (a <: Throwable) => StackTraceElementArray -> Java a ()

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

data {-# CLASS "java.nio.file.FileSystemException" #-} FileSystemException = FileSystemException (Object# FileSystemException)
  deriving (Class, Show, Typeable)

type instance Inherits FileSystemException = '[IOException]

data {-# CLASS "java.io.FileNotFoundException" #-} FileNotFoundException =
  FileNotFoundException (Object# FileNotFoundException)

type instance Inherits FileNotFoundException = '[IOException]

data {-# CLASS "java.nio.file.NotSuchFileException" #-} NotSuchFileException =
  NotSuchFileException (Object# NotSuchFileException)

type instance Inherits NotSuchFileException = '[FileSystemException]

data {-# CLASS "java.io.EOFException" #-} EOFException =
  EOFException (Object# EOFException)

type instance Inherits EOFException = '[IOException]

toIOError :: IOException -> SysIOErr.IOError
toIOError jioex =  SysIOErr.ioeSetErrorString ioErr msg
  where ioErr = SysIOErr.mkIOError type' "" Nothing Nothing
        type' | isDoesNotExistError = SysIOErr.doesNotExistErrorType
              | jioex `instanceOf` (undefined JClass EOFException) =
                  SysIOErr.eofErrorType
              | otherwise = SysIOErr.userErrorType
        clazz = classObject jioex
        msg   = unsafePerformJavaWith jioex getMessage
        isDoesNotExistError =
          jioex `instanceOf` (undefined :: JClass FileNotFoundException) ||
          jioex `instanceOf` (undefined :: JClass NotSuchFileException) ||
          msg == "The system cannot find the path specified"

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
  deriving (Class, Typeable)

type instance Inherits ReflectiveOperationException = '[JException]

-- End java.lang.ReflectiveOperationException

-- Start java.lang.RuntimeException

data RuntimeException = RuntimeException @java.lang.RuntimeException
  deriving (Class, Typeable)

type instance Inherits RuntimeException = '[JException]

  -- End java.lang.RuntimeException
