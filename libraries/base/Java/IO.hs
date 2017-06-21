{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
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
import Java.Text

data {-# CLASS "java.io.File[]" #-}
  FileArray = FileArray (Object# FileArray)
  deriving Class

instance JArray File FileArray

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

data {-# CLASS "java.lang.Readable" #-} Readable = Readable (Object# Readable)
  deriving Class

foreign import java unsafe "@interface read" readBuffer :: (a <: Readable) => CharBuffer -> Java a Int

-- End java.lang.Readable

-- Start java.lang.Appendable

data {-# CLASS "java.lang.Appendable" #-} Appendable = Appendable (Object# Appendable)
  deriving Class

foreign import java unsafe "@interface append" append :: (a <: Appendable) => JChar -> Java a Appendable

foreign import java unsafe "@interface append"
  appendSequence :: (a <: Appendable, b <: CharSequence) => b -> Java a Appendable

foreign import java unsafe "@interface append"
  appendSubSequence :: (a <: Appendable, b <: CharSequence) => b -> Int -> Int -> Java a Appendable

-- End java.lang.Appendable

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

type instance Inherits Writer = '[Object, Closeable, Flushable, Appendable]

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

-- Start java.io.DataInput

data {-# CLASS "java.io.DataInput" #-} DataInput = DataInput (Object# DataInput)
  deriving Class

foreign import java unsafe "@interface" readBoolean :: (a <: DataInput) => Java a Bool

foreign import java unsafe "@interface" readByte :: (a <: DataInput) => Java a Byte

foreign import java unsafe "@interface" readChar :: (a <: DataInput) => Java a JChar

foreign import java unsafe "@interface" readDouble :: (a <: DataInput) => Java a Double

foreign import java unsafe "@interface" readFloat :: (a <: DataInput) => Java a Float

foreign import java unsafe "@interface" readFully :: (a <: DataInput) => JByteArray -> Java a ()

foreign import java unsafe "@interface readFully"
  readFullySub :: (a <: DataInput) => JByteArray -> Int -> Int -> Java a ()

foreign import java unsafe "@interface" readInt :: (a <: DataInput) => Java a Int

foreign import java unsafe "@interface" readLine :: (a <: DataInput) => Java a String

foreign import java unsafe "@interface" readLong :: (a <: DataInput) => Java a Int64

foreign import java unsafe "@interface" readShort :: (a <: DataInput) => Java a Short

foreign import java unsafe "@interface" readUnsignedByte :: (a <: DataInput) => Java a Int

foreign import java unsafe "@interface" readUnsignedShort :: (a <: DataInput) => Java a Int

foreign import java unsafe "@interface" readUTF :: (a <: DataInput) => Java a String

foreign import java unsafe "@interface" skipBytes :: (a <: DataInput) => Int -> Java a Int

-- End java.io.DataInput

-- Start java.io.DataOutput

data {-# CLASS "java.io.DataOutput" #-} DataOutput = DataOutput (Object# DataOutput)
  deriving Class

foreign import java unsafe "@interface write" writeArrayDO :: (a <: DataOutput) => JByteArray -> Java a ()

foreign import java unsafe "@interface write"
  writeSubArrayDO :: (a <: DataOutput) => JByteArray -> Int -> Int -> Java a ()

foreign import java unsafe "@interface write" writeDO :: (a <: DataOutput) => Int -> Java a ()

foreign import java unsafe "@interface" writeBoolean :: (a <: DataOutput) => Bool -> Java a ()

foreign import java unsafe "@interface" writeByte :: (a <: DataOutput) => Int -> Java a ()

foreign import java unsafe "@interface" writeBytes :: (a <: DataOutput) => String -> Java a ()

foreign import java unsafe "@interface" writeChar :: (a <: DataOutput) => Int -> Java a ()

foreign import java unsafe "@interface" writeChars :: (a <: DataOutput) => String -> Java a ()

foreign import java unsafe "@interface" writeDouble :: (a <: DataOutput) => Double -> Java a ()

foreign import java unsafe "@interface" writeFloat :: (a <: DataOutput) => Float -> Java a ()

foreign import java unsafe "@interface" writeInt :: (a <: DataOutput) => Int -> Java a ()

foreign import java unsafe "@interface" writeLong :: (a <: DataOutput) => Int64 -> Java a ()

foreign import java unsafe "@interface" writeshort :: (a <: DataOutput) => Short -> Java a ()

foreign import java unsafe "@interface" writeUTF :: (a <: DataOutput) => String -> Java a ()

-- End java.io.DataOutput

-- Start java.io.FilterOutputStream

data {-# CLASS "java.io.FilterOutputStream" #-} FilterOutputStream = FilterOutputStream (Object# FilterOutputStream)
  deriving Class

type instance Inherits FilterOutputStream = '[OutputStream, Closeable, Flushable]

-- End java.io.FilterOutputStream

-- Start java.io.PrintWriter

data {-# CLASS "java.io.PrintWriter" #-} PrintWriter = PrintWriter (Object# PrintWriter)
  deriving Class

type instance Inherits PrintWriter = '[Writer, Closeable, Flushable, Appendable]

foreign import java unsafe checkError :: Java PrintWriter Bool

foreign import java unsafe clearError :: Java PrintWriter ()

foreign import java unsafe "format" formatLocale :: Locale -> String -> JObjectArray -> Java PrintWriter ()

foreign import java unsafe format :: String -> JObjectArray -> Java PrintWriter ()

foreign import java unsafe "print" printBool :: Bool -> Java PrintWriter ()

foreign import java unsafe "print" printChar :: Char -> Java PrintWriter ()

foreign import java unsafe "print" printCharArray :: JCharArray -> Java PrintWriter ()

foreign import java unsafe "print" printDouble :: Double -> Java PrintWriter ()

foreign import java unsafe "print" printFloat :: Float -> Java PrintWriter ()

foreign import java unsafe "print" printInt :: Int -> Java PrintWriter ()

foreign import java unsafe "print" printLong :: Int64 -> Java PrintWriter ()

foreign import java unsafe "print" printObject :: Object -> Java PrintWriter ()

foreign import java unsafe "print" printString :: String -> Java PrintWriter ()

foreign import java unsafe "printf"
  printfLocale :: Locale -> String -> JObjectArray -> Java PrintWriter ()

foreign import java unsafe printf :: String -> JObjectArray -> Java PrintWriter ()

foreign import java unsafe println :: Java PrintWriter ()

foreign import java unsafe "println" printlnBool :: Bool -> Java PrintWriter ()

foreign import java unsafe "println" printlnChar :: Char -> Java PrintWriter ()

foreign import java unsafe "println" printlnCharArray :: JCharArray -> Java PrintWriter ()

foreign import java unsafe "println" printlnDouble :: Double -> Java PrintWriter ()

foreign import java unsafe "println" printlnFloat :: Float -> Java PrintWriter ()

foreign import java unsafe "println" printlnInt :: Int -> Java PrintWriter ()

foreign import java unsafe "println" printlnLong :: Int64 -> Java PrintWriter ()

foreign import java unsafe "println" printlnObject :: Object -> Java PrintWriter ()

foreign import java unsafe "println" printlnString :: String -> Java PrintWriter ()

foreign import java unsafe setError :: Java PrintWriter ()

-- End java.io.PrintWriter

-- Start java.io.PrintStream

data {-# CLASS "java.io.PrintStream" #-} PrintStream = PrintStream (Object# PrintStream)
  deriving Class

type instance Inherits PrintStream = '[FilterOutputStream, Closeable, Flushable, Appendable]

foreign import java unsafe "checkError" checkErrorPStream :: Java PrintStream Bool

foreign import java unsafe "clearError" clearErrorPStream :: Java PrintStream ()

foreign import java unsafe "format" formatLocalePStream :: Locale -> String -> JObjectArray -> Java PrintStream ()

foreign import java unsafe "format" formatPStream :: String -> JObjectArray -> Java PrintStream ()

foreign import java unsafe "print" printBoolPStream :: Bool -> Java PrintStream ()

foreign import java unsafe "print" printCharPStream :: Char -> Java PrintStream ()

foreign import java unsafe "print" printCharArrayPStream :: JCharArray -> Java PrintStream ()

foreign import java unsafe "print" printDoublePStream :: Double -> Java PrintStream ()

foreign import java unsafe "print" printFloatPStream :: Float -> Java PrintStream ()

foreign import java unsafe "print" printIntPStream :: Int -> Java PrintStream ()

foreign import java unsafe "print" printLongPStream :: Int64 -> Java PrintStream ()

foreign import java unsafe "print" printObjectPStream :: Object -> Java PrintStream ()

foreign import java unsafe "print" printStringPStream :: String -> Java PrintStream ()

foreign import java unsafe "printf"
  printfLocalePStream :: Locale -> String -> JObjectArray -> Java PrintStream ()

foreign import java unsafe "printf" printfPStream :: String -> JObjectArray -> Java PrintStream ()

foreign import java unsafe "println" printlnPStream :: Java PrintStream ()

foreign import java unsafe "println" printlnBoolPStream :: Bool -> Java PrintStream ()

foreign import java unsafe "println" printlnCharPStream :: Char -> Java PrintStream ()

foreign import java unsafe "println" printlnCharArrayPStream :: JCharArray -> Java PrintStream ()

foreign import java unsafe "println" printlnDoublePStream :: Double -> Java PrintStream ()

foreign import java unsafe "println" printlnFloatPStream :: Float -> Java PrintStream ()

foreign import java unsafe "println" printlnIntPStream :: Int -> Java PrintStream ()

foreign import java unsafe "println" printlnLongPStream :: Int64 -> Java PrintStream ()

foreign import java unsafe "println" printlnObjectPStream :: Object -> Java PrintStream ()

foreign import java unsafe "println" printlnStringPStream :: String -> Java PrintStream ()

foreign import java unsafe "setError" setErrorPStream :: Java PrintStream ()

-- End java.io.PrintStream

-- Start java.io.FilterInputStream

data {-# CLASS "java.io.FilterInputStream" #-}
  FilterInputStream = FilterInputStream (Object# FilterInputStream)
  deriving Class

type instance Inherits FilterInputStream = '[InputStream, Closeable]

-- End java.io.FilterInputStream

-- Start java.io.File

data {-# CLASS "java.io.File" #-}
  File = File (Object# File)
  deriving Class

type instance Inherits File = '[Object, (Comparable File)]

foreign import java unsafe canExecute :: Java File Bool

foreign import java unsafe canRead :: Java File Bool

foreign import java unsafe canWrite :: Java File Bool

foreign import java unsafe compareTo :: File -> Java File Int

foreign import java unsafe createNewFile :: Java File Bool

foreign import java unsafe delete :: Java File Bool

foreign import java unsafe deleteOnExit :: Java File ()

foreign import java unsafe exists :: Java File Bool

foreign import java unsafe getAbsoluteFile :: Java File File

foreign import java unsafe getAbsolutePath :: Java File String

foreign import java unsafe getCanonicalFile :: Java File File

foreign import java unsafe getCanonicalPath :: Java File String

foreign import java unsafe getFreeSpace :: Java File Int64

foreign import java unsafe getName :: Java File String

foreign import java unsafe getParent :: Java File String

foreign import java unsafe getParentFile :: Java File File

foreign import java unsafe getPath :: Java File String

foreign import java unsafe getTotalSpace :: Java File Int64

foreign import java unsafe getUsableSpace :: Java File Int64

foreign import java unsafe isAbsolute :: Java File Bool

foreign import java unsafe isDirectory :: Java File Bool

foreign import java unsafe isFile :: Java File Bool

foreign import java unsafe isHidden :: Java File Bool

foreign import java unsafe lastModified :: Java File Int64

foreign import java unsafe length :: Java File Int64

foreign import java unsafe list :: Java File JStringArray

foreign import java unsafe "list" listFilenameFilter :: FilenameFilter -> Java File JStringArray

foreign import java unsafe listFiles :: Java File FileArray

foreign import java unsafe "listFiles" listFilesFileFilter :: FileFilter -> Java File FileArray

foreign import java unsafe "listFiles" listFilesFilenameFilter :: FilenameFilter -> Java File FileArray

foreign import java unsafe mkdir :: Java File Bool

foreign import java unsafe mkdirs :: Java File Bool

foreign import java unsafe renameTo :: File -> Java File Bool

foreign import java unsafe setExecutable :: Bool -> Java File Bool

foreign import java unsafe "setExecutable" setExecutableOwner :: Bool -> Bool -> Java File Bool

foreign import java unsafe setLastModified :: Int64 -> Java File Bool

foreign import java unsafe setReadable :: Bool -> Java File Bool

foreign import java unsafe "setReadable" setReadableOwner :: Bool -> Bool -> Java File Bool

foreign import java unsafe setReadOnly :: Java File Bool

foreign import java unsafe setWritable :: Bool -> Java File Bool

foreign import java unsafe "setWritable" setWritableOwner :: Bool -> Bool -> Java File Bool

foreign import java unsafe toPath :: Java File Path

-- End java.io.File

-- Start java.io.FilenameFilter

data {-# CLASS "java.io.FilenameFilter" #-}
  FilenameFilter = FilenameFilter (Object# FilenameFilter)
  deriving Class

foreign import java unsafe "@interface"
  accept :: (b <: FilenameFilter) => File -> String -> Java b Bool

-- End java.io.FilenameFilter

-- Start java.io.FileFilter

data {-# CLASS "java.io.FileFilter" #-}
  FileFilter = FileFilter (Object# FileFilter)
  deriving Class

foreign import java unsafe "@interface"
  acceptFileFilter :: (b <: FileFilter) => File -> Java b Bool

-- End java.io.FileFilter
