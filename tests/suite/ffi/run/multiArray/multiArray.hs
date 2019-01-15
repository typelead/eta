{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface #-}

import Java
import Control.Monad

data JIntIntArray = JIntIntArray @int[][]
  deriving Class

instance JArray JIntArray JIntIntArray where

foreign import java unsafe "@static Utils.getIntIntArray" getIntIntArray
  :: Java a JIntIntArray

main :: IO ()
main = do
  ints <- java $ do
    iiarr <- getIntIntArray
    iarrs <- iiarr <.> arrayToList
    forM iarrs $ \iarr -> iarr <.> arrayToList
  print ints
