{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns, StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Primitive
-- Copyright   :  (c) Rahul Muttineni 2016-2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Dealing with native Java primitives.
--
-----------------------------------------------------------------------------

module Java.Primitive
  ( Byte(..)
  , Short(..)
  , JChar(..)
  , charToJChar
  , jcharToChar)
where

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.Show
import Java.PrimitiveBase

import Data.Data
import Data.Typeable

import Data.Char
import Prelude(maxBound)

deriving instance Typeable Byte

byteType :: DataType
byteType = mkIntType "Java.Primitive.Byte"

instance Data Byte where
  toConstr = mkIntegralConstr byteType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Byte."
  dataTypeOf _ = byteType

deriving instance Typeable Short

shortType :: DataType
shortType = mkIntType "Java.Primitive.Short"

instance Data Short where
  toConstr = mkIntegralConstr shortType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Short."
  dataTypeOf _ = shortType

deriving instance Typeable JChar

charToJChar :: Char -> Maybe JChar
charToJChar chr
    | chrVal <= maxJChar && not isSurrogate = Just $ fromIntegral chrVal
    | otherwise = Nothing
    where maxJChar = fromIntegral (maxBound :: JChar)
          chrVal = ord chr
          -- check if Char is reserved UTF-16 value - not a valid JChar
          isSurrogate = 0xD800 <= chrVal && chrVal <= 0xDFFF

jcharToChar :: JChar -> Maybe Char
jcharToChar jchr
  | not isSurrogate = Just $ chr jchrVal
  | otherwise = Nothing
  where jchrVal = fromIntegral jchr
        -- check if JChar is reserved UTF-16 value - not a valid character
        isSurrogate = 0xD800 <= jchrVal && jchrVal <= 0xDFFF

jcharType :: DataType
jcharType = mkIntType "Java.Primitive.JChar"

instance Data JChar where
  toConstr = mkIntegralConstr jcharType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type JChar."
  dataTypeOf _ = jcharType
