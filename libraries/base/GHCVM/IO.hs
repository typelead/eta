{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHCVM.IO
-- Copyright   :  (c) Rahul Muttineni 2016
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GHCVM's input/output operations
--
-----------------------------------------------------------------------------

module GHCVM.IO (
  printShow
 ) where

import GHC.Base
import GHC.Show
import GHCVM.JString

printShow :: (Show a) => a -> IO ()
printShow x = printStr stdout . mkJString $ show x

data {-# CLASS "java.io.PrintStream" #-} PrintStream = PrintStream (Object# PrintStream)

foreign import java unsafe "@static @field java.lang.System.out" stdout :: PrintStream

foreign import java unsafe "java.io.PrintStream.println" printStr :: PrintStream -> JString -> IO ()
