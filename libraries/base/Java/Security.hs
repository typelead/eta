{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Security
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Security
--
-----------------------------------------------------------------------------

module Java.Security where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Primitive
import Java.IO
import Java.Collections

-- Start java.security.SecureRandom

data {-# CLASS "java.security.SecureRandom" #-} SecureRandom = SecureRandom (Object# SecureRandom)
  deriving Class

foreign import java unsafe generateSeed :: Int -> Java SecureRandom JByteArray

foreign import java unsafe getAlgorithm :: Java SecureRandom String

foreign import java unsafe getProvider :: Int -> Java SecureRandom Provider

foreign import java unsafe next :: Int -> Java SecureRandom Int

foreign import java unsafe nextBytes :: JByteArray -> Java SecureRandom ()

foreign import java unsafe setSeed :: JByteArray -> Java SecureRandom ()

foreign import java unsafe "setSeed" setSeedLong :: Int64 -> Java SecureRandom ()

-- End java.security.SecureRandom

-- Start java.security.Provider

data {-# CLASS "java.security.Provider" #-} Provider = Provider (Object# Provider)
  deriving Class

type instance Inherits Provider = '[Properties]

-- End java.security.Provider
