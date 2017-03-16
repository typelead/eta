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

data {-# CLASS "java.io.Closeable" #-} Closeable = Closeable (Object# Closeable)
  deriving Class

foreign import java unsafe close :: (a <: Closeable) => Java a ()

-- start java.io.Reader

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
