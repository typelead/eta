{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.IO
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Input/Output utilities
--
-----------------------------------------------------------------------------

module Java.IO where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Primitive

-- Start java.io.Closeable

data {-# CLASS "java.io.Closeable" #-} Closeable = Closeable (Object# Closeable)
  deriving Class

foreign import java unsafe close :: (a <: Closeable) => Java a ()

-- End java.io.Closeable

-- Start java.io.Reader

data {-# CLASS "java.io.Reader" #-} Reader = Reader (Object# Reader)
  deriving Class

type instance Inherits Reader = '[Object, Closeable]

foreign import java unsafe mark :: (a <: Reader) => Int -> Java a ()

foreign import java unsafe markSupported :: (a <: Reader) => Java a Bool

foreign import java unsafe read :: (a <: Reader) => Java a Int

foreign import java unsafe "read" readArray :: (a <: Reader) => JCharArray -> Java a Int

foreign import java unsafe "read"
  readSubArrray :: (a <: Reader) => JCharArray -> Int -> Int -> Java a Int

-- TODO: Complete when buffers are imported
-- foreign import java unsafe "read" readBuffer :: (a <: Reader) => CharBuffer -> Java a Int

foreign import java unsafe ready :: (a <: Reader) => Java a Bool

foreign import java unsafe reset :: (a <: Reader) => Java a ()

foreign import java unsafe skip :: (a <: Reader) => Int64 -> Java a Int64

-- end java.io.Reader

-- start java.io.Writer

data {-# CLASS "java.io.Writer" #-} Writer = Writer (Object# Writer)
  deriving Class

type instance Inherits Writer = '[Object, Closeable]

foreign import java unsafe append :: (a <: Writer) => JChar -> Java a Writer

foreign import java unsafe "append"
  appendSequence :: (a <: Writer, b <: CharSequence) => b -> Java a Writer

foreign import java unsafe "append"
  appendSubSequence :: (a <: Writer, b <: CharSequence) => b -> Int -> Int -> Java a Writer

foreign import java unsafe flush :: (a <: Writer) => Java a ()

foreign import java unsafe "write" writeArray :: (a <: Writer) => JCharArray -> Java a ()

foreign import java unsafe "write"
  writeSubArray :: (a <: Writer) => JCharArray -> Int -> Int -> Java a ()

foreign import java unsafe "write" write :: (a <: Writer) => Int -> Java a ()

foreign import java unsafe "write" writeString :: (a <: Writer) => String -> Java a ()

foreign import java unsafe "write"
  writeSubString :: (a <: Writer) => String -> Int -> Int -> Java a ()

-- End java.io.Writer

-- Start java.io.InputStream

data {-# CLASS "java.io.InputStream" #-} InputStream = InputStream (Object# InputStream)
  deriving Class

type instance Inherits InputStream = '[Object, Closeable]

foreign import java unsafe available :: (a <: InputStream) => Java a Int

foreign import java unsafe "mark" markInputStream :: (a <: InputStream) => Int -> Java a Int

foreign import java unsafe "markSupported"
  markSupportedInputStream :: (a <: InputStream) => Java a Bool

foreign import java unsafe "read" readInputStream :: (a <: InputStream) => Java a Int

foreign import java unsafe "read"
  readByteInputStream :: (a <: InputStream) => JByteArray -> Java a Int

foreign import java unsafe "read"
  readByteInputStream2 :: (a <: InputStream) => JByteArray -> Int -> Int -> Java a Int

foreign import java unsafe "reset" resetInputStream :: (a <: InputStream) => Java a ()

foreign import java unsafe "skip" skipInputStream :: (a <: InputStream) => Int64 -> Java a Int64

-- End java.io.InputStream

-- Start java.io.OutputStream

data {-# CLASS "java.io.OutputStream" #-} OutputStream = OutputStream (Object# OutputStream)
  deriving Class

type instance Inherits OutputStream = '[Object, Closeable]

foreign import java unsafe "flush" flushOutputStream :: (a <: OutputStream) => Java a ()

foreign import java unsafe "write"
  writeArrayOutputStream :: (a <: OutputStream) => JByteArray -> Java a ()

foreign import java unsafe "write"
  writeSubArrayOutputStream :: (a <: OutputStream) => JByteArray -> Int -> Int -> Java a ()

foreign import java unsafe "write"
  writeOutputStream :: (a <: OutputStream) => Int -> Java a ()

-- End java.io.OutputStream

-- Start java.io.BufferedInputStream

data {-# CLASS "java.io.BufferedInputStream" #-} BufferedInputStream = BufferedInputStream (Object# BufferedInputStream)
  deriving Class

type instance Inherits BufferedInputStream = '[Object, Closeable]

foreign import java unsafe "available"
  availableBufferedInputStream :: (a <: BufferedInputStream) => Java a Int

foreign import java unsafe "mark"
  markBufferedInputStream :: (a <: BufferedInputStream) => Int -> Java a Int

foreign import java unsafe "mark"
  markSupportedBufferedInputStream :: (a <: BufferedInputStream) => Java a Bool

foreign import java unsafe "read"
  readBufferedInputStream :: (a <: BufferedInputStream) => Java a Int

foreign import java unsafe "read"
  readBufferedInputStream2 :: (a <: BufferedInputStream) => JByteArray -> Int -> Int -> Java a Int

foreign import java unsafe "reset"
  resetBufferedInputStream :: (a <: BufferedInputStream) => Java a ()

foreign import java unsafe "skip"
  skipBufferedInputStream :: (a <: BufferedInputStream) => Int64 -> Java a Int64

-- End java.io.BufferedInputStream

-- Start java.io.BufferedOutputStream

data {-# CLASS "java.io.BufferedOutputStream" #-} BufferedOutputStream = BufferedOutputStream (Object# BufferedOutputStream)
  deriving Class

foreign import java unsafe "flush"
  flushBufferedOutputStream :: (a <: BufferedOutputStream) => Java a ()

foreign import java unsafe "write"
  writeBufferedOutputStream :: (a <: BufferedOutputStream) => JByteArray -> Int -> Int -> Java a ()

foreign import java unsafe "write"
  writeBufferedOutputStream2 :: (a <: BufferedOutputStream) => Int -> Java a ()

-- End java.io.BufferedOutputStream

-- Start java.io.BufferedReader

data {-# CLASS "java.io.BufferedReader" #-} BufferedReader = BufferedReader (Object# BufferedReader)
  deriving Class

type instance Inherits BufferedReader = '[Object, Closeable]

foreign import java unsafe "mark" markBufferedReader :: (a <: BufferedReader) => Int -> Java a ()

foreign import java unsafe "markSupported"
  markSupportedBufferedReader :: (a <: BufferedReader) => Java a Bool

foreign import java unsafe "read" readBufferedReader :: (a <: BufferedReader) => Java a Int

foreign import java unsafe "read"
  readBufferedReader2 :: (a <: BufferedReader) => JCharArray -> Int -> Int -> Java a Int

foreign import java unsafe readLine :: (a <: BufferedReader) => Java a String

foreign import java unsafe "ready" readyBufferedReader :: (a <: BufferedReader) => Java a Bool

foreign import java unsafe "reset" resetBufferedReader :: (a <: BufferedReader) => Java a ()

foreign import java unsafe "skip"
  skipBufferedReader :: (a <: BufferedReader) => Int64 -> Java a Int64

-- End java.io.BufferedReader
