module GHCVM.Debug
  (module Outputable,
   str,
   debugIO)
where

import Outputable hiding ((<>))
import FastString
import Control.Monad.IO.Class

str :: String -> SDoc
str = ptext . sLit

debugIO :: (MonadIO m) => String -> m ()
debugIO string = liftIO . putStrLn $ "Debug: " ++ show string
