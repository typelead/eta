{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- ----------------------------------------------------------------------------

module Eta.Utils.Fingerprint (
        Fingerprint(..), fingerprint0,
        readHexFingerprint,
        fingerprintData,
        fingerprintString,
        getFileHash
   ) where

-- #include "md5.h"
-- #include "HsVersions.h"

import Numeric          ( readHex )
-- #if __GLASGOW_HASKELL__ < 707
-- -- Only needed for getFileHash below.
-- import Foreign
-- import Eta.Utils.Panic
-- import System.IO
-- import Control.Monad    ( when )
-- #endif

import GHC.Fingerprint

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)
