{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.TopHandler
-- Copyright   :  (c) The University of Glasgow, 2001-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Support for catching exceptions raised during top-level computations
-- (e.g. @Main.main@, 'Control.Concurrent.forkIO', and foreign exports)
--
-----------------------------------------------------------------------------

module GHC.TopHandler (
        runMainIO--, runIO, runIOFastExit, runNonIO,
        -- topHandler, topHandlerFastExit,
        -- reportStackOverflow, reportError,
        -- flushStdHandles
    ) where

import GHC.Types

runMainIO :: IO a -> IO a
runMainIO x = x
