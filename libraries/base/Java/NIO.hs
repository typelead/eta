{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.IO
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Input/Output utilities
--
-----------------------------------------------------------------------------

module Java.NIO where

import GHC.Base
import GHC.Int
import Java.Array
import Java.Primitive

-- Start java.nio.Buffer

data {-# CLASS "java.nio.Buffer" #-} Buffer = Buffer (Object# Buffer)
  deriving Class

foreign import java unsafe array :: (a <: Buffer) => Java a Object

foreign import java unsafe arrayOffset :: (a <: Buffer) => Java a Int

foreign import java unsafe capacity :: (a <: Buffer) => Java a Int

foreign import java unsafe clear :: (a <: Buffer) => Java a Buffer

foreign import java unsafe flip :: (a <: Buffer) => Java a Buffer

foreign import java unsafe hasArray :: (a <: Buffer) => Java a Bool

foreign import java unsafe hasRemaining :: (a <: Buffer) => Java a Bool

foreign import java unsafe isDirect :: (a <: Buffer) => Java a Bool

foreign import java unsafe isReadOnly :: (a <: Buffer) => Java a Bool

foreign import java unsafe limit :: (a <: Buffer) => Java a Int

foreign import java unsafe "limit" limitInt :: (a <: Buffer) => Int -> Java a Buffer

foreign import java unsafe mark :: (a <: Buffer) => Java a Buffer

foreign import java unsafe position :: (a <: Buffer) => Java a Int

foreign import java unsafe "position" positionInt :: (a <: Buffer) => Int -> Java a Buffer

foreign import java unsafe remaining :: (a <: Buffer) => Java a Int

foreign import java unsafe reset :: (a <: Buffer) => Java a Buffer

foreign import java unsafe rewind :: (a <: Buffer) => Java a Buffer

-- End java.nio.Buffer
