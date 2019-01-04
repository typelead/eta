{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Eta.BasicTypes.JavaAnnotation
  ( JavaAnnotation(..)
  , AnnExpr(..)
  , LAnnExpr )
where

import Data.Data

import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString

-- This is guaranteed to be an AnnRecord
newtype JavaAnnotation id = JavaAnnotation { jaExpr :: LAnnExpr id }
  deriving Data

type LAnnExpr id = Located (AnnExpr id)

data AnnExpr id =
    AnnRecord (Located id) [(LString, LAnnExpr id)]
  | AnnApply (Located id) (Maybe (LAnnExpr id))
  | AnnCharacter Char
  | AnnString FastString
  | AnnInteger Integer
  | AnnRational Rational
  | AnnList [(LAnnExpr id)]
  deriving Data
