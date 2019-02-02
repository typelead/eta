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
  intss <- java $ do
    iiarr <- getIntIntArray
    iarrs <- iiarr <.> arrayToList
    forM iarrs $ \iarr -> iarr <.> arrayToList
  print intss
  let arr = jints intss
  java $ forM_ [0.. (length intss - 1)] $ \i -> do
    ints <- arr <.> aget i
    io $ print (fromJava ints :: [Int])

jints :: [[Int]] -> JIntIntArray
jints intss = unsafePerformJava $ do
  arr <- anew (length intss)
  forM_ (zip [0..] intss) $ \(i, ints) ->
    arr <.> aset i (toJava ints)
  return arr

