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
import Java.NIO

-- Start java.io.Closeable

data {-# CLASS "java.io.Closeable" #-} Closeable = Closeable (Object# Closeable)
  deriving Class

foreign import java unsafe "@interface" close :: (a <: Closeable) => Java a ()

-- End java.io.Closeable

-- Start java.io.Flushable

data {-# CLASS "java.io.Flushable" #-} Flushable = Flushable (Object# Flushable)
  deriving Class

foreign import java unsafe "@interface" flush :: (a <: Flushable) => Java a ()

-- End java.io.Flushable

-- Start java.io.Readable

data {-# CLASS "java.io.Readable" #-} Readable = Readable (Object# Readable)
  deriving Class

foreign import java unsafe "@interface read" readBuffer :: (a <: Readable) => CharBuffer -> Java a Int

-- End java.io.Readable

-- Start java.io.Reader

data {-# CLASS "java.io.Reader" #-} Reader = Reader (Object# Reader)
  deriving Class

type instance Inherits Reader = '[Object, Closeable, Readable]

foreign import java unsafe mark :: (a <: Reader) => Int -> Java a ()

foreign import java unsafe markSupported :: (a <: Reader) => Java a Bool

foreign import java unsafe "read" readReader :: (a <: Reader) => Java a Int

foreign import java unsafe "read" readArray :: (a <: Reader) => JCharArray -> Java a Int

foreign import java unsafe "read"
  readSubArray :: (a <: Reader) => JCharArray -> Int -> Int -> Java a Int

foreign import java unsafe ready :: (a <: Reader) => Java a Bool

foreign import java unsafe reset :: (a <: Reader) => Java a ()

foreign import java unsafe skip :: (a <: Reader) => Int64 -> Java a Int64

-- end java.io.Reader

-- start java.io.Writer

data {-# CLASS "java.io.Writer" #-} Writer = Writer (Object# Writer)
  deriving Class

type instance Inherits Writer = '[Object, Closeable, Flushable]

foreign import java unsafe append :: (a <: Writer) => JChar -> Java a Writer

foreign import java unsafe "append"
  appendSequence :: (a <: Writer, b <: CharSequence) => b -> Java a Writer

foreign import java unsafe "append"
  appendSubSequence :: (a <: Writer, b <: CharSequence) => b -> Int -> Int -> Java a Writer

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
  readArrayInputStream :: (a <: InputStream) => JByteArray -> Java a Int

foreign import java unsafe "read"
  readSubArrayInputStream :: (a <: InputStream) => JByteArray -> Int -> Int -> Java a Int

foreign import java unsafe "reset" resetInputStream :: (a <: InputStream) => Java a ()

foreign import java unsafe "skip" skipInputStream :: (a <: InputStream) => Int64 -> Java a Int64

-- End java.io.InputStream

-- Start java.io.OutputStream

data {-# CLASS "java.io.OutputStream" #-} OutputStream = OutputStream (Object# OutputStream)
  deriving Class

type instance Inherits OutputStream = '[Object, Closeable, Flushable]

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

type instance Inherits BufferedInputStream = '[InputStream, Closeable]

-- End java.io.BufferedInputStream

-- Start java.io.BufferedOutputStream

data {-# CLASS "java.io.BufferedOutputStream" #-} BufferedOutputStream = BufferedOutputStream (Object# BufferedOutputStream)
  deriving Class

type instance Inherits BufferedOutputStream = '[OutputStream, Closeable, Flushable]

-- End java.io.BufferedOutputStream

-- Start java.io.BufferedReader

data {-# CLASS "java.io.BufferedReader" #-} BufferedReader = BufferedReader (Object# BufferedReader)
  deriving Class

type instance Inherits BufferedReader = '[Reader, Closeable]

-- End java.io.BufferedReader

-- Start java.io.BufferedWriter

data {-# CLASS "java.io.BufferedWriter" #-} BufferedWriter = BufferedWriter (Object# BufferedWriter)
  deriving Class

type instance Inherits BufferedWriter = '[Writer, Closeable, Flushable]

foreign import java unsafe newLine :: (a <: BufferedWriter) => Java a ()

-- End java.io.BufferedWriter

-- Start java.io.StringReader

data {-# CLASS "java.io.StringReader" #-} StringReader = StringReader (Object# StringReader)
  deriving Class

type instance Inherits StringReader = '[Reader, Closeable]

-- End java.io.StringReader

-- Start java.io.StringWriter

data {-# CLASS "java.io.StringWriter" #-} StringWriter = StringWriter (Object# StringWriter)
  deriving Class

type instance Inherits StringWriter = '[Writer, Closeable, Flushable]

-- End java.io.StringWriter

-- Start java.io.CharArrayReader

data {-# CLASS "java.io.CharArrayReader" #-} CharArrayReader = CharArrayReader (Object# CharArrayReader)
  deriving Class

type instance Inherits CharArrayReader = '[Reader, Closeable]

-- End java.io.CharArrayReader

-- Start java.io.CharArrayWriter

data {-# CLASS "java.io.CharArrayWriter" #-} CharArrayWriter = CharArrayWriter (Object# CharArrayWriter)
  deriving Class

type instance Inherits CharArrayWriter = '[Writer, Closeable, Flushable]

-- End java.io.CharArrayWriter
