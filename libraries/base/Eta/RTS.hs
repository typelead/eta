{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Eta.RTS
-- Copyright   :  (c) Rahul Muttineni 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- T
--
-----------------------------------------------------------------------------

module Eta.RTS
  ( dumpMemoryManager
  , dumpMemoryManagerVerbose
  ) where

import GHC.Base

foreign import java unsafe "@static eta.runtime.io.MemoryManager.dumpMemoryManager"
  dumpMemoryManager :: IO ()

foreign import java unsafe "@static eta.runtime.io.MemoryManager.dumpMemoryManagerVerbose"
  dumpMemoryManagerVerbose :: IO ()
