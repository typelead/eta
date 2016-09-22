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
        , flushStdHandles
    ) where

import Control.Exception
import Data.Maybe

import Foreign
import Foreign.C
import GHC.Base
import GHC.Conc hiding (throwTo)
import GHC.Real
import GHC.IO
import GHC.IO.Handle.FD
import GHC.IO.Handle
import GHC.IO.Exception
import GHC.Weak

-- TODO: Necessary?
import Data.Dynamic (toDyn)

runMainIO :: IO a -> IO a
runMainIO x = x

flushStdHandles :: IO ()
flushStdHandles = do
  hFlush stdout `catchAny` \_ -> return ()
  hFlush stderr `catchAny` \_ -> return ()
