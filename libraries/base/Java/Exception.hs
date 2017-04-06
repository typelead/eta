{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
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
import Java.IO

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

foreign import java unsafe "printStackTrace"
  printStackTracePrintStream :: PrintStream -> Java Throwable ()

foreign import java unsafe "printStackTrace"
  printStackTracePrintWriter :: PrintStream -> Java Throwable ()

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

data {-# CLASS "java.lang.Exception" #-} Exception = Exception (Object# Exception)
  deriving Class

type instance Inherits Exception = '[Throwable]

-- End java.lang.Exception

-- Start java.lang.Error

data {-# CLASS "java.lang.Error" #-} Error = Error (Object# Error)
  deriving Class

type instance Inherits Error = '[Throwable]

-- End java.lang.Error
