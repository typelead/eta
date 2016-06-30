module Codec.JVM.Internal where

import Data.Binary.Put (Put, putWord16be, putWord32be)
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)

import qualified Data.ByteString as BS

packWord16be :: Word16 -> ByteString
packWord16be w = BS.pack
    [ fromIntegral (shiftR w 8) :: Word8
    , fromIntegral (w)          :: Word8 ]

packWord32be :: Word32 -> ByteString
packWord32be w = BS.pack
    [ fromIntegral (shiftR w 24) :: Word8
    , fromIntegral (shiftR w 16) :: Word8
    , fromIntegral (shiftR w  8) :: Word8
    , fromIntegral (w)               :: Word8 ]

packI16 :: Int -> ByteString
packI16 = packWord16be . fromIntegral

packI32 :: Int -> ByteString
packI32 = packWord32be . fromIntegral

putI16 :: Int -> Put
putI16 = putWord16be . fromIntegral

putI32 :: Int -> Put
putI32 = putWord32be . fromIntegral
