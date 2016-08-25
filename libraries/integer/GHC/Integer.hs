{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) The University of Glasgow 1994-2008
-- License     :  see libraries/integer-gmp/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
--
-- This module exposes the /portable/ 'Integer' API.  See
-- "GHC.Integer.GMP.Internals" for the GMP-specific internal
-- representation of 'Integer' as well as optimized GMP-specific
-- operations.
-----------------------------------------------------------------------------

module GHC.Integer (
    Integer,

    -- * Construct 'Integer's
    mkInteger, smallInteger, wordToInteger,
    word64ToInteger, int64ToInteger,
    -- * Conversion to other integral types
    integerToWord, integerToInt,
    integerToWord64, integerToInt64,

    -- * Helpers for 'RealFloat' type-class operations
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,

    -- * Arithmetic operations
    plusInteger, minusInteger, timesInteger, negateInteger,
    absInteger, signumInteger,
    divModInteger, divInteger, modInteger,
    quotRemInteger, quotInteger, remInteger,

    -- * Comparison predicates
    eqInteger, neqInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    eqInteger#, neqInteger#,
    leInteger#, gtInteger#, ltInteger#, geInteger#,

    -- * Bit-operations
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,

    -- * Hashing
    hashInteger,
 ) where

import GHC.Integer.Type

default ()
