{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns,
             FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Java.String
-- Copyright   :  (c) Rahul Muttineni 2016
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The @JString@ type and associated operations.
--
-----------------------------------------------------------------------------

module Java.String (
   JString(..)
 , toJString
 , fromJString
 , JStringArray(..)
 , CharSequence(..)
 ) where

import GHC.Base
import Data.String
import GHC.Show (Show(..))
import Java.StringBase
import Java.Core
import Java.Array

instance IsString JString where
  fromString = toJString

data {-# CLASS "java.lang.String[]" #-} JStringArray = JStringArray (Object# JStringArray)
  deriving (Class, Show)

instance JArray JString JStringArray

instance JavaConverter [String] JStringArray where
  toJava ws = unsafePerformJava $ arrayFromList bytes
    where bytes = map toJava ws :: [JString]
  fromJava ba = map fromJava $ unsafePerformJavaWith ba arrayToList
