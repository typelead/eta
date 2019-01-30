{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Eta.BasicTypes.Interop
  ( JavaAnnotation(..)
  , AnnExpr(..)
  , LAnnExpr
  , JavaImportSpec(..)
  , encodeJavaImportSpec
  , decodeJavaImportSpec
  , GenImportSpec(..), MethodImportSpec, FieldImportSpec
  , Effect(..) )
where

import Data.Data
import Data.Bits
import Data.Char
import Data.List (sortOn)

import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString

-- This is guaranteed to be an AnnRecord
newtype JavaAnnotation id = JavaAnnotation { jaExpr :: LAnnExpr id }
  deriving Data

type LAnnExpr id = Located (AnnExpr id)

data AnnExpr id =
    AnnRecord (Located id) [(LString, LAnnExpr id)]
  | AnnApply (Located id) (LAnnExpr id)
  | AnnCharacter Char
  | AnnString FastString
  | AnnInteger Integer
  | AnnRational Rational
  | AnnList [(LAnnExpr id)]
  deriving Data

data JavaImportSpec =
  JavaImportSpec { jisMethods :: [MethodImportSpec]
                 , jisFields  :: [FieldImportSpec] }
  deriving (Eq, Ord)

instance Monoid JavaImportSpec where
  mempty = JavaImportSpec { jisMethods = mempty, jisFields = mempty }
  JavaImportSpec m1 f1 `mappend` JavaImportSpec m2 f2 =
    JavaImportSpec (m1 `mappend` m2) (f1 `mappend` f2)

encodeJavaImportSpec :: JavaImportSpec -> FastString
encodeJavaImportSpec JavaImportSpec { jisMethods, jisFields }
  | numMethods + numFields == 0 = nilFS
  | otherwise =
    mkFastString $ putImportSpecs sortedMethods
                ++ if numFields > 0
                   then putImportSpecs sortedFields
                   else []

  where sortedMethods = sortOn isIndex jisMethods
        sortedFields  = sortOn isIndex jisFields
        numMethods = length sortedMethods
        numFields  = length sortedFields
        toChar i
          | i >= 0xD800 && i <= 0xDFFF = chr $ (i - 0xD800) + 0x80000
          | otherwise = chr i
        putImportSpecs specs = toChar (length specs) : map putImportSpec specs
        putImportSpec (GenImportSpec { isIndex, isEffect, isNullable })
          = toChar $ (isIndex `shiftL` 3) .|. meta
          where meta = fromEnum isEffect .|. (fromEnum isNullable `shiftL` 2)

decodeJavaImportSpec :: String -> JavaImportSpec
decodeJavaImportSpec str
  | null str  = mempty
  | otherwise = JavaImportSpec { jisMethods = take numMethods impSpecs
                               , jisFields  = drop numMethods impSpecs }
  where ints       = map ord str
        impSpecs   = parseImportSpecs ints
        numMethods = head ints

        parseImportSpecs [] = []
        parseImportSpecs (i:is) = map parseImport (take i is)
                               ++ parseImportSpecs (drop i is)
          where parseImport i = GenImportSpec {..}
                  where isIndex    = i `shiftR` 3
                        isEffect   = toEnum $ i .&. 0x3
                        isNullable = toEnum $ (i .&. 0x4) `shiftR` 2

type MethodImportSpec = GenImportSpec
type FieldImportSpec = GenImportSpec

data GenImportSpec =
  GenImportSpec { isIndex    :: Int
                , isEffect   :: Effect
                , isNullable :: Bool }
  deriving (Eq, Ord)

data Effect = NoEffect | IOEffect | STEffect | JavaEffect
  deriving (Eq, Ord, Enum)
