{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-- @since 4.6.0.0
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath ( getExecutablePath ) where

import GHC.Err

-- -- The exported function is defined outside any if-guard to make sure
-- -- every OS implements it with the same type.

-- -- | Returns the absolute pathname of the current executable.
-- --
-- -- Note that for scripts and interactive sessions, this is the path to
-- -- the interpreter (e.g. ghci.)
-- --
-- -- @since 4.6.0.0
getExecutablePath :: IO FilePath
getExecutablePath = error "getExecutablePath: Not implemented yet."
