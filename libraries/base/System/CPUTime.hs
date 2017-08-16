{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) Rahul Muttineni 2016
--                (c) The University of Glasgow 2001
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

module System.CPUTime
( getCPUTime
, cpuTimePrecision )
where

import GHC.Base (fmap)
import GHC.Real (fromIntegral)
import Data.Ratio

import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- | Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.
getCPUTime :: IO Integer
getCPUTime = fmap fromIntegral getCPUTime'

-- | The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.
-- WARNING: This value is not accurate, so if you need an accurate value of
-- precision, you must go through JNI.
cpuTimePrecision :: Integer
cpuTimePrecision = 10000000000

foreign import java unsafe "@static eta.base.Utils.getCPUTime"
  getCPUTime' :: IO CLong
