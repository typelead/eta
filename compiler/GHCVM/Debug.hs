module GHCVM.Debug
  (module Outputable,
   unsafePerformIO,
   str,
   debugIO)
where

import GHCVM.Utils.Outputable hiding ((<>))
import GHCVM.Utils.FastString
import Control.Monad.IO.Class
import System.IO.Unsafe(unsafePerformIO)

str :: String -> SDoc
str = ptext . sLit

debugIO :: (MonadIO m) => String -> m ()
debugIO string = liftIO . putStrLn $ "Debug: " ++ show string
