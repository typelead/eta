{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ETA.IO
-- Copyright   :  (c) Rahul Muttineni 2016
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- ETA's input/output operations
--
-----------------------------------------------------------------------------

module ETA.IO (
  printShow
 ) where

import GHC.Base
import GHC.Show
import ETA.JString

printShow :: (Show a) => a -> IO ()
printShow x = printStr stdout . mkJString $ show x

data {-# CLASS "java.io.PrintStream" #-} PrintStream = PrintStream (Object# PrintStream)

foreign import java unsafe "@static @field java.lang.System.out" stdout :: PrintStream

foreign import java unsafe "println" printStr :: PrintStream -> JString -> IO ()
