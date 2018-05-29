module Eta.Debug
  (module Eta.Utils.Outputable,
   unsafePerformIO,
   str,
   debugIO)
where

import Eta.Utils.Outputable hiding ((<>))
import Eta.Utils.FastString
import Control.Monad.IO.Class
import System.IO.Unsafe(unsafePerformIO)

str :: String -> SDoc
str = ptext . sLit

debugIO :: (MonadIO m) => String -> m ()
debugIO string = liftIO . putStrLn $ "Debug: " ++ show string
