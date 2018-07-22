{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
        runMainIO, runIO, runNonIO,
        reportStackOverflow, reportError,
        flushStdHandles
    ) where

import Control.Exception
import Java.Exception
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

-- | 'runMainIO' is wrapped around 'Main.main' (or whatever main is
-- called in the program).  It catches otherwise uncaught exceptions,
-- and also flushes stdout\/stderr before exiting.
runMainIO :: IO a -> IO a
runMainIO main =
  catch main (\(e :: ExitCode) -> do
                 case e of
                   ExitSuccess   -> exit 0
                   ExitFailure n -> exit n
                 return (errorWithoutStackTrace "runMainIO: Exception raised."))

foreign import java unsafe "@static java.lang.System.exit"
  exit :: Int -> IO ()

-- | 'runIO' is wrapped around every @foreign export@ and @foreign
-- import \"wrapper\"@ to mop up any uncaught exceptions.  Thus, the
-- result of running 'System.Exit.exitWith' in a foreign-exported
-- function is the same as in the main thread: it terminates the
-- program.
--
runIO :: IO a -> IO a
runIO main = main

-- | The same as 'runIO', but for non-IO computations.  Used for
-- wrapping @foreign export@ and @foreign import \"wrapper\"@ when these
-- are used to export Haskell functions with non-IO types.
--
runNonIO :: a -> IO a
runNonIO a = a `seq` return a

-- try to flush stdout/stderr, but don't worry if we fail
-- (these handles might have errors, and we don't want to go into
-- an infinite loop).
flushStdHandles :: IO ()
flushStdHandles = do
  hFlush stdout `catchAny` \_ -> return ()
  hFlush stderr `catchAny` \_ -> return ()
