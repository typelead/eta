{-# LANGUAGE FlexibleContexts #-}
module Codec.JVM.Internal
  ( module Data.Binary.Put,
    module Codec.JVM.Internal )
where

import Data.Binary.Put
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32, Word64)

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import qualified Data.ByteString as BS

packWord16be :: Word16 -> ByteString
packWord16be w = BS.pack
    [ fromIntegral (shiftR w 8) :: Word8
    , fromIntegral w          :: Word8 ]

packWord32be :: Word32 -> ByteString
packWord32be w = BS.pack
    [ fromIntegral (shiftR w 24) :: Word8
    , fromIntegral (shiftR w 16) :: Word8
    , fromIntegral (shiftR w  8) :: Word8
    , fromIntegral w               :: Word8 ]

packI16 :: Int -> ByteString
packI16 = packWord16be . fromIntegral

packI32 :: Int -> ByteString
packI32 = packWord32be . fromIntegral

putI16 :: Int -> Put
putI16 = putWord16be . fromIntegral

putI32 :: Int -> Put
putI32 = putWord32be . fromIntegral

-- TODO: Everything below is extracted from
--       binary-8.4.0: Data.Binary.Put, Data.Binary.FloatCast.
--       Due to stack supporting and older version in the current
--       lts-6.6, we are unable to use that package. The functions
--       below should be removed once Stackage LTS catches up.
------------------------------------------------------------------------
-- Floats/Doubles

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Float -> Put
putFloatbe = putWord32be . floatToWord
{-# INLINE putFloatbe #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Double -> Put
putDoublebe = putWord64be . doubleToWord
{-# INLINE putDoublebe #-}


-- | Reinterpret-casts a `Float` to a `Word32`.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINE floatToWord #-}

-- | Reinterpret-casts a `Double` to a `Word64`.
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)
{-# INLINE doubleToWord #-}

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

-- TODO: Currently not working
-- -- | Write a Int32 in big endian format
-- putInt32be :: Int32 -> Put
-- putInt32be = putBuilder . B.int32BE
-- {-# INLINE putInt32be #-}

-- -- | Write a Int64 in big endian format
-- putInt64be :: Int64 -> Put
-- putInt64be = putBuilder . B.int64BE
-- {-# INLINE putInt64be #-}
