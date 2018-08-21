{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, GHCForeignImportPrim,
             UnliftedFFITypes #-}
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
  , setKeepCAFs
  , showRaw
  , printRaw
  ) where

import GHC.Base
import Java.StringBase
import System.IO

foreign import java unsafe "@static eta.runtime.io.MemoryManager.dumpMemoryManager"
  dumpMemoryManager :: IO ()

foreign import java unsafe "@static eta.runtime.io.MemoryManager.dumpMemoryManagerVerbose"
  dumpMemoryManagerVerbose :: IO ()

foreign import java unsafe "@static eta.runtime.thunk.Thunk.setKeepCAFs"
  setKeepCAFs :: IO ()

showRaw :: a -> String
showRaw a = case showRaw# (unsafeCoerce# a) of
              (# js# #) -> fromJString (JS# js#)

foreign import prim "eta.base.Utils.showRaw" showRaw# :: Any -> (# Object# JString #)

printRaw :: a -> IO ()
printRaw a = putStrLn (showRaw a)
