module ETA.Debug
  (module ETA.Utils.Outputable,
   unsafePerformIO,
   str,
   debugIO)
where

import ETA.Utils.Outputable hiding ((<>))
import ETA.Utils.FastString
import Control.Monad.IO.Class
import System.IO.Unsafe(unsafePerformIO)

str :: String -> SDoc
str = ptext . sLit

debugIO :: (MonadIO m) => String -> m ()
debugIO string = liftIO . putStrLn $ "Debug: " ++ show string
