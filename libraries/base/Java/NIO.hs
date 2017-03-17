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
-- Bindings for Java NIO utilities
--
-----------------------------------------------------------------------------

module Java.NIO where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Primitive

-- Start java.nio.Buffer

data {-# CLASS "java.nio.Buffer" #-} Buffer = Buffer (Object# Buffer)
  deriving Class

foreign import java unsafe array :: (a <: Buffer) => Java a Object

foreign import java unsafe arrayOffset :: (a <: Buffer) => Java a Int

foreign import java unsafe capacity :: (a <: Buffer) => Java a Int

foreign import java unsafe clear :: (a <: Buffer) => Java a Buffer

foreign import java unsafe flip :: (a <: Buffer) => Java a Buffer

foreign import java unsafe hasArray :: (a <: Buffer) => Java a Bool

foreign import java unsafe hasRemaining :: (a <: Buffer) => Java a Bool

foreign import java unsafe isDirect :: (a <: Buffer) => Java a Bool

foreign import java unsafe isReadOnly :: (a <: Buffer) => Java a Bool

foreign import java unsafe limit :: (a <: Buffer) => Java a Int

foreign import java unsafe "limit" limitInt :: (a <: Buffer) => Int -> Java a Buffer

foreign import java unsafe mark :: (a <: Buffer) => Java a Buffer

foreign import java unsafe position :: (a <: Buffer) => Java a Int

foreign import java unsafe "position" positionInt :: (a <: Buffer) => Int -> Java a Buffer

foreign import java unsafe remaining :: (a <: Buffer) => Java a Int

foreign import java unsafe reset :: (a <: Buffer) => Java a Buffer

foreign import java unsafe rewind :: (a <: Buffer) => Java a Buffer

-- End java.nio.Buffer

-- Start java.nio.ByteBuffer

data {-# CLASS "java.nio.ByteBuffer" #-} ByteBuffer = ByteBuffer (Object# ByteBuffer)
  deriving Class

foreign import java unsafe "array" arrayByteBuffer :: (a <: ByteBuffer) => Java a JByteArray

foreign import java unsafe "arrayOffset"
  arrayOffsetByteBuffer :: (a <: ByteBuffer) => Java a Int

foreign import java unsafe asCharBuffer :: (a <: ByteBuffer) => Java a CharBuffer

foreign import java unsafe asDoubleBuffer :: (a <: ByteBuffer) => Java a DoubleBuffer

foreign import java unsafe asFloatBuffer :: (a <: ByteBuffer) => Java a FloatBuffer

foreign import java unsafe asIntBuffer :: (a <: ByteBuffer) => Java a IntBuffer

foreign import java unsafe asLongBuffer :: (a <: ByteBuffer) => Java a LongBuffer

foreign import java unsafe asReadOnlyBuffer :: (a <: ByteBuffer) => Java a ByteBuffer

foreign import java unsafe asShortBuffer :: (a <: ByteBuffer) => Java a ShortBuffer

foreign import java unsafe compact :: (a <: ByteBuffer) => Java a ByteBuffer

foreign import java unsafe compareTo :: (a <: ByteBuffer) => ByteBuffer -> Java a Int

foreign import java unsafe duplicate :: (a <: ByteBuffer) => Java a ByteBuffer

foreign import java unsafe equals :: (a <: ByteBuffer) => Object -> Java a Bool

foreign import java unsafe get :: (a <: ByteBuffer) => Java a Byte

foreign import java unsafe "get" getByte :: (a <: ByteBuffer) => JByteArray -> Java a ByteBuffer

foreign import java unsafe "get"
  getByte2 :: (a <: ByteBuffer) => JByteArray -> Int -> Int -> Java a ByteBuffer

foreign import java unsafe "get" get2 :: (a <: ByteBuffer) => Int -> Java a Byte

foreign import java unsafe getChar :: (a <: ByteBuffer) => Java a Char

foreign import java unsafe "getChar" getCharInt :: (a <: ByteBuffer) => Int -> Java a Char

foreign import java unsafe getDouble :: (a <: ByteBuffer) => Java a Double

foreign import java unsafe "getDouble" getDoubleInt :: (a <: ByteBuffer) => Int -> Java a Double

foreign import java unsafe getFloat :: (a <: ByteBuffer) => Java a Float

foreign import java unsafe "getFloat" getFloatInt :: (a <: ByteBuffer) => Int -> Java a Float

foreign import java unsafe getInt :: (a <: ByteBuffer) => Java a Int

foreign import java unsafe "getInt" getIntInt :: (a <: ByteBuffer) => Int -> Java a Int

foreign import java unsafe getLong :: (a <: ByteBuffer) => Java a Int64

foreign import java unsafe "getLong" getLongInt :: (a <: ByteBuffer) => Int -> Java a Int64

foreign import java unsafe getShort :: (a <: ByteBuffer) => Java a Short

foreign import java unsafe "getShort" getShortInt :: (a <: ByteBuffer) => Int -> Java a Short

foreign import java unsafe "hasArray" hasArrayByteBuffer :: (a <: ByteBuffer) => Java a Bool

foreign import java unsafe hashCode :: (a <: ByteBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectByteBuffer :: (a <: ByteBuffer) => Java a Bool

foreign import java unsafe order :: (a <: ByteBuffer) => Java a ByteOrder

foreign import java unsafe "order" orderByte :: (a <: ByteBuffer) => ByteOrder -> Java a ByteBuffer

foreign import java unsafe put :: (a <: ByteBuffer) => Byte -> Java a ByteBuffer

foreign import java unsafe "put" putByte :: (a <: ByteBuffer) => JByteArray -> Java a ByteBuffer

foreign import java unsafe "put"
  putByte2 :: (a <: ByteBuffer) => JByteArray -> Int -> Int -> Java a ByteBuffer

foreign import java unsafe "put" putByte3 :: (a <: ByteBuffer) => ByteBuffer -> Java a ByteBuffer

foreign import java unsafe "put" putByte4 :: (a <: ByteBuffer) => Int -> Byte -> Java a ByteBuffer

foreign import java unsafe putChar :: (a <: ByteBuffer) => JChar -> Java a ByteBuffer

foreign import java unsafe "putChar" putChar2 :: (a <: ByteBuffer) => Int -> JChar -> Java a ByteBuffer

foreign import java unsafe putDouble :: (a <: ByteBuffer) => Double -> Java a ByteBuffer

foreign import java unsafe "putDouble"
  putDouble2 :: (a <: ByteBuffer) => Int -> Double -> Java a ByteBuffer

foreign import java unsafe putFloat :: (a <: ByteBuffer) => Float -> Java a ByteBuffer

foreign import java unsafe "putFloat"
  putFloat2 :: (a <: ByteBuffer) => Int -> Float -> Java a ByteBuffer

foreign import java unsafe putInt :: (a <: ByteBuffer) => Int -> Java a ByteBuffer

foreign import java unsafe "putInt" putInt2 :: (a <: ByteBuffer) => Int -> Int -> Java a ByteBuffer

foreign import java unsafe putLong :: (a <: ByteBuffer) => Int64 -> Java a ByteBuffer

foreign import java unsafe "putLong"
  putLong2 :: (a <: ByteBuffer) => Int -> Int64 -> Java a ByteBuffer

foreign import java unsafe putShort :: (a <: ByteBuffer) => Short -> Java a ByteBuffer

foreign import java unsafe "putShort"
  putShort2 :: (a <: ByteBuffer) => Int -> Short -> Java a ByteBuffer

foreign import java unsafe slice :: (a <: ByteBuffer) => Java a ByteBuffer

foreign import java unsafe toString :: (a <: ByteBuffer) => Java a String

-- End java.nio.ByteBuffer

-- Start java.nio.ByteOrder

data {-# CLASS "java.nio.ByteOrder" #-} ByteOrder = ByteOrder (Object# ByteOrder)
  deriving Class

foreign import java unsafe "toString" toStringByteOrder :: (a <: ByteOrder) => Java a String

-- End java.nio.ByteOrder

-- Start java.nio.CharBuffer

data {-# CLASS "java.nio.CharBuffer" #-} CharBuffer = CharBuffer (Object# CharBuffer)
  deriving Class

foreign import java unsafe append :: (a <: CharBuffer) => JChar -> Java a CharBuffer

foreign import java unsafe "append" append2 :: (a <: CharBuffer) => CharSequence -> Java a CharBuffer

foreign import java unsafe "append"
  append3 :: (a <: CharBuffer) => CharSequence -> Int -> Int -> Java a CharBuffer

foreign import java unsafe "array" arrayCharBuffer :: (a <: CharBuffer) => Java a JCharArray

foreign import java unsafe "arrayOffset"
  arrayOffsetCharBuffer :: (a <: CharBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferCharBuffer:: (a <: CharBuffer) => Java a CharBuffer

foreign import java unsafe charAt :: (a <: CharBuffer) => Int -> Java a JChar

foreign import java unsafe "compact" compactCharBuffer :: (a <: CharBuffer) => Java a CharBuffer

foreign import java unsafe "compareTo"
  compareToCharBuffer :: (a <: CharBuffer) => CharBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateCharBuffer :: (a <: CharBuffer) => Java a CharBuffer

foreign import java unsafe "equals" equalsCharBuffer :: (a <: CharBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getCharBuffer :: (a <: CharBuffer) => Java a JChar

foreign import java unsafe "get" getCharBuffer2 :: (a <: CharBuffer) => JCharArray -> Java a CharBuffer

foreign import java unsafe "get"
  getCharBuffer3 :: (a <: CharBuffer) => JCharArray -> Int -> Int -> Java a CharBuffer

foreign import java unsafe "get" getCharBuffer4 :: (a <: CharBuffer) => Int -> Java a JChar

foreign import java unsafe "hasArray" hasArrayCharBuffer :: (a <: CharBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeCharBuffer :: (a <: CharBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectCharBuffer :: (a <: CharBuffer) => Java a Bool

foreign import java unsafe length :: (a <: CharBuffer) => Java a Int

foreign import java unsafe "order" orderCharBuffer :: (a <: CharBuffer) => Java a ByteOrder

foreign import java unsafe "put" putCharBuffer :: (a <: CharBuffer) => JChar -> Java a CharBuffer

foreign import java unsafe "put" putCharBuffer2 :: (a <: CharBuffer) => JCharArray -> Java a CharBuffer

foreign import java unsafe "put"
  putCharBuffer3 :: (a <: CharBuffer) => JCharArray -> Int -> Int -> Java a CharBuffer

foreign import java unsafe "put" putCharBuffer4 :: (a <: CharBuffer) => CharBuffer -> Java a CharBuffer

foreign import java unsafe "put" putCharBuffer5 :: (a <: CharBuffer) => Int -> JChar -> Java a CharBuffer

foreign import java unsafe "put" putCharBuffer6 :: (a <: CharBuffer) => String -> Java a CharBuffer

foreign import java unsafe "put"
  putCharBuffer7 :: (a <: CharBuffer) => String -> Int -> Int -> Java a CharBuffer

foreign import java unsafe "read" readCharBuffer :: (a <: CharBuffer) => CharBuffer -> Java a Int

foreign import java unsafe "slice" sliceCharBuffer :: (a <: CharBuffer) => Java a CharBuffer

foreign import java unsafe subSequence :: (a <: CharBuffer) => Int -> Int -> Java a CharBuffer

foreign import java unsafe "toString" toStringCharBuffer :: (a <: CharBuffer) => Java a String

-- End java.nio.CharBuffer

-- Start java.nio.DoubleBuffer

data {-# CLASS "java.nio.DoubleBuffer" #-} DoubleBuffer = DoubleBuffer (Object# DoubleBuffer)
  deriving Class

foreign import java unsafe "array" arrayDoubleBuffer :: (a <: DoubleBuffer) => Java a JDoubleArray

foreign import java unsafe "arrayOffset"
  arrayOffsetDoubleBuffer :: (a <: DoubleBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferDoubleBuffer :: (a <: DoubleBuffer) => Java a DoubleBuffer

foreign import java unsafe "compact" compactDoubleBuffer :: (a <: DoubleBuffer) => Java a DoubleBuffer

foreign import java unsafe "compareTo"
  compareToDoubleBuffer :: (a <: DoubleBuffer) => DoubleBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateDoubleBuffer :: (a <: DoubleBuffer) => Java a DoubleBuffer

foreign import java unsafe "equals" equalsDoubleBuffer :: (a <: DoubleBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getDoubleBuffer :: (a <: DoubleBuffer) => Java a Double

foreign import java unsafe "get"
  getDoubleBuffer2 :: (a <: DoubleBuffer) => JDoubleArray -> Java a DoubleBuffer

foreign import java unsafe "get"
  getDoubleBuffer3 :: (a <: DoubleBuffer) => JDoubleArray -> Int -> Int -> Java a DoubleBuffer

foreign import java unsafe "get" getDoubleBuffer4 :: (a <: DoubleBuffer) => Int -> Java a Double

foreign import java unsafe "hasArray" hasArrayDoubleBuffer :: (a <: DoubleBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeDoubleBuffer :: (a <: DoubleBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectDoubleBuffer :: (a <: DoubleBuffer) => Java a Bool

foreign import java unsafe "order" orderDoubleBuffer :: (a <: DoubleBuffer) => Java a ByteOrder

foreign import java unsafe "put"
  putDoubleBuffer :: (a <: DoubleBuffer) => Double -> Java a DoubleBuffer

foreign import java unsafe "put"
  putDoubleBuffer2 :: (a <: DoubleBuffer) => JDoubleArray -> Java a DoubleBuffer

foreign import java unsafe "put"
  putDoubleBuffer3 :: (a <: DoubleBuffer) => JDoubleArray -> Int -> Int -> Java a DoubleBuffer

foreign import java unsafe "put"
  putDoubleBuffer4 :: (a <: DoubleBuffer) => DoubleBuffer -> Java a DoubleBuffer

foreign import java unsafe "put"
  putDoubleBuffer5 :: (a <: DoubleBuffer) => Int -> Double -> Java a DoubleBuffer

foreign import java unsafe "slice" sliceDoubleBuffer :: (a <: DoubleBuffer) => Java a DoubleBuffer

foreign import java unsafe "toString" toStringDoubleBuffer :: (a <: DoubleBuffer) => Java a String

-- End java.nio.DoubleBuffer

-- Start java.nio.FloatBuffer

data {-# CLASS "java.nio.FloatBuffer" #-} FloatBuffer = FloatBuffer (Object# FloatBuffer)
  deriving Class

foreign import java unsafe "array" arrayFloatBuffer :: (a <: FloatBuffer) => Java a JFloatArray

foreign import java unsafe "arrayOffset"
  arrayOffsetFloatBuffer :: (a <: FloatBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferFloatBuffer :: (a <: FloatBuffer) => Java a FloatBuffer

foreign import java unsafe "compact" compactFloatBuffer :: (a <: FloatBuffer) => Java a FloatBuffer

foreign import java unsafe "compareTo"
  compareToFloatBuffer :: (a <: FloatBuffer) => FloatBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateFloatBuffer :: (a <: FloatBuffer) => Java a FloatBuffer

foreign import java unsafe "equals" equalsFloatBuffer :: (a <: FloatBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getFloatBuffer :: (a <: FloatBuffer) => Java a Float

foreign import java unsafe "get"
  getFloatBuffer2 :: (a <: FloatBuffer) => JFloatArray -> Java a FloatBuffer

foreign import java unsafe "get"
  getFloatBuffer3 :: (a <: FloatBuffer) => JFloatArray -> Int -> Int -> Java a FloatBuffer

foreign import java unsafe "get" getFloatBuffer4 :: (a <: FloatBuffer) => Int -> Java a Float

foreign import java unsafe "hasArray" hasArrayFloatBuffer :: (a <: FloatBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeFloatBuffer :: (a <: FloatBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectFloatBuffer :: (a <: FloatBuffer) => Java a Bool

foreign import java unsafe "order" orderFloatBuffer :: (a <: FloatBuffer) => Java a ByteOrder

foreign import java unsafe "put"
  putFloatBuffer :: (a <: FloatBuffer) => Float -> Java a FloatBuffer

foreign import java unsafe "put"
  putFloatBuffer2 :: (a <: FloatBuffer) => JFloatArray -> Java a FloatBuffer

foreign import java unsafe "put"
  putFloatBuffer3 :: (a <: FloatBuffer) => JFloatArray -> Int -> Int -> Java a FloatBuffer

foreign import java unsafe "put"
  putFloatBuffer4 :: (a <: FloatBuffer) => FloatBuffer -> Java a FloatBuffer

foreign import java unsafe "put"
  putFloatBuffer5 :: (a <: FloatBuffer) => Int -> Float -> Java a FloatBuffer

foreign import java unsafe "slice" sliceFloatBuffer :: (a <: FloatBuffer) => Java a FloatBuffer

foreign import java unsafe "toString" toStringFloatBuffer :: (a <: FloatBuffer) => Java a String

-- End java.nio.FloatBuffer

-- Start java.nio.IntBuffer

data {-# CLASS "java.nio.IntBuffer" #-} IntBuffer = IntBuffer (Object# IntBuffer)
  deriving Class

foreign import java unsafe "array" arrayIntBuffer :: (a <: IntBuffer) => Java a JIntArray

foreign import java unsafe "arrayOffset"
  arrayOffsetIntBuffer :: (a <: IntBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferIntBuffer :: (a <: IntBuffer) => Java a IntBuffer

foreign import java unsafe "compact" compactIntBuffer :: (a <: IntBuffer) => Java a IntBuffer

foreign import java unsafe "compareTo"
  compareToIntBuffer :: (a <: IntBuffer) => IntBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateIntBuffer :: (a <: IntBuffer) => Java a IntBuffer

foreign import java unsafe "equals" equalsIntBuffer :: (a <: IntBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getIntBuffer :: (a <: IntBuffer) => Java a Int

foreign import java unsafe "get"
  getIntBuffer2 :: (a <: IntBuffer) => JIntArray -> Java a IntBuffer

foreign import java unsafe "get"
  getIntBuffer3 :: (a <: IntBuffer) => JIntArray -> Int -> Int -> Java a IntBuffer

foreign import java unsafe "get" getIntBuffer4 :: (a <: IntBuffer) => Int -> Java a Int

foreign import java unsafe "hasArray" hasArrayIntBuffer :: (a <: IntBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeIntBuffer :: (a <: IntBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectIntBuffer :: (a <: IntBuffer) => Java a Bool

foreign import java unsafe "order" orderIntBuffer :: (a <: IntBuffer) => Java a ByteOrder

foreign import java unsafe "put"
  putIntBuffer :: (a <: IntBuffer) => Int -> Java a IntBuffer

foreign import java unsafe "put"
  putIntBuffer2 :: (a <: IntBuffer) => JIntArray -> Java a IntBuffer

foreign import java unsafe "put"
  putIntBuffer3 :: (a <: IntBuffer) => JIntArray -> Int -> Int -> Java a IntBuffer

foreign import java unsafe "put"
  putIntBuffer4 :: (a <: IntBuffer) => IntBuffer -> Java a IntBuffer

foreign import java unsafe "put"
  putIntBuffer5 :: (a <: IntBuffer) => Int -> Int -> Java a IntBuffer

foreign import java unsafe "slice" sliceIntBuffer :: (a <: IntBuffer) => Java a IntBuffer

foreign import java unsafe "toString" toStringIntBuffer :: (a <: IntBuffer) => Java a String

-- End java.nio.IntBuffer

-- Start java.nio.LongBuffer

data {-# CLASS "java.nio.LongBuffer" #-} LongBuffer = LongBuffer (Object# LongBuffer)
  deriving Class

foreign import java unsafe "array" arrayLongBuffer :: (a <: LongBuffer) => Java a JLongArray

foreign import java unsafe "arrayOffset"
  arrayOffsetLongBuffer :: (a <: LongBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferLongBuffer :: (a <: LongBuffer) => Java a LongBuffer

foreign import java unsafe "compact" compactLongBuffer :: (a <: LongBuffer) => Java a LongBuffer

foreign import java unsafe "compareTo"
  compareToLongBuffer :: (a <: LongBuffer) => LongBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateLongBuffer :: (a <: LongBuffer) => Java a LongBuffer

foreign import java unsafe "equals" equalsLongBuffer :: (a <: LongBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getLongBuffer :: (a <: LongBuffer) => Java a Int64

foreign import java unsafe "get" getLongBuffer2 :: (a <: LongBuffer) => Int -> Java a Int64

foreign import java unsafe "get"
  getLongBuffer3 :: (a <: LongBuffer) => JLongArray -> Java a LongBuffer

foreign import java unsafe "get"
  getLongBuffer4 :: (a <: LongBuffer) => JLongArray -> Int -> Int -> Java a LongBuffer

foreign import java unsafe "hasArray" hasArrayLongBuffer :: (a <: LongBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeLongBuffer :: (a <: LongBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectLongBuffer :: (a <: LongBuffer) => Java a Bool

foreign import java unsafe "order" orderLongBuffer :: (a <: LongBuffer) => Java a ByteOrder

foreign import java unsafe "put"
  putLongBuffer :: (a <: LongBuffer) => Int -> Java a LongBuffer

foreign import java unsafe "put"
  putLongBuffer2 :: (a <: LongBuffer) => JLongArray -> Java a LongBuffer

foreign import java unsafe "put"
  putLongBuffer3 :: (a <: LongBuffer) => JLongArray -> Int -> Int -> Java a LongBuffer

foreign import java unsafe "put"
  putLongBuffer4 :: (a <: LongBuffer) => LongBuffer -> Java a LongBuffer

foreign import java unsafe "put"
  putLongBuffer5 :: (a <: LongBuffer) => Int -> Int -> Java a LongBuffer

foreign import java unsafe "slice" sliceLongBuffer :: (a <: LongBuffer) => Java a LongBuffer

foreign import java unsafe "toString" toStringLongBuffer :: (a <: LongBuffer) => Java a String

-- End java.nio.LongBuffer

-- Start java.nio.ShortBuffer

data {-# CLASS "java.nio.ShortBuffer" #-} ShortBuffer = ShortBuffer (Object# ShortBuffer)
  deriving Class

foreign import java unsafe "array" arrayShortBuffer :: (a <: ShortBuffer) => Java a JShortArray

foreign import java unsafe "arrayOffset"
  arrayOffsetShortBuffer :: (a <: ShortBuffer) => Java a Int

foreign import java unsafe "asReadOnlyBuffer"
  asReadOnlyBufferShortBuffer :: (a <: ShortBuffer) => Java a ShortBuffer

foreign import java unsafe "compact" compactShortBuffer :: (a <: ShortBuffer) => Java a ShortBuffer

foreign import java unsafe "compareTo"
  compareToShortBuffer :: (a <: ShortBuffer) => ShortBuffer -> Java a Int

foreign import java unsafe "duplicate" duplicateShortBuffer :: (a <: ShortBuffer) => Java a ShortBuffer

foreign import java unsafe "equals" equalsShortBuffer :: (a <: ShortBuffer) => Object -> Java a Bool

foreign import java unsafe "get" getShortBuffer :: (a <: ShortBuffer) => Java a Int64

foreign import java unsafe "get" getShortBuffer2 :: (a <: ShortBuffer) => Int -> Java a Int64

foreign import java unsafe "get"
  getShortBuffer3 :: (a <: ShortBuffer) => JShortArray -> Java a ShortBuffer

foreign import java unsafe "get"
  getShortBuffer4 :: (a <: ShortBuffer) => JShortArray -> Int -> Int -> Java a ShortBuffer

foreign import java unsafe "hasArray" hasArrayShortBuffer :: (a <: ShortBuffer) => Java a Bool

foreign import java unsafe "hashCode" hashCodeShortBuffer :: (a <: ShortBuffer) => Java a Int

foreign import java unsafe "isDirect" isDirectShortBuffer :: (a <: ShortBuffer) => Java a Bool

foreign import java unsafe "order" orderShortBuffer :: (a <: ShortBuffer) => Java a ByteOrder

foreign import java unsafe "put"
  putShortBuffer :: (a <: ShortBuffer) => Int -> Java a ShortBuffer

foreign import java unsafe "put"
  putShortBuffer2 :: (a <: ShortBuffer) => JShortArray -> Java a ShortBuffer

foreign import java unsafe "put"
  putShortBuffer3 :: (a <: ShortBuffer) => JShortArray -> Int -> Int -> Java a ShortBuffer

foreign import java unsafe "put"
  putShortBuffer4 :: (a <: ShortBuffer) => ShortBuffer -> Java a ShortBuffer

foreign import java unsafe "put"
  putShortBuffer5 :: (a <: ShortBuffer) => Int -> Int -> Java a ShortBuffer

foreign import java unsafe "slice" sliceShortBuffer :: (a <: ShortBuffer) => Java a ShortBuffer

foreign import java unsafe "toString" toStringShortBuffer :: (a <: ShortBuffer) => Java a String

-- End java.nio.ShortBuffer

-- Start java.nio.MappedByteBuffer

data {-# CLASS "java.nio.MappedByteBuffer" #-} MappedByteBuffer = MappedByteBuffer (Object# MappedByteBuffer)
  deriving Class

foreign import java unsafe force :: (a <: MappedByteBuffer) => Java a MappedByteBuffer

foreign import java unsafe isLoaded :: (a <: MappedByteBuffer) => Java a Bool

foreign import java unsafe load :: (a <: MappedByteBuffer) => Java a MappedByteBuffer

-- End java.nio.MappedByteBuffer
