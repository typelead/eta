{-# LANGUAGE ForeignFunctionInterface #-}
module Export where

import Java

data Hello = Hello @test.Hello
  deriving Class

foreign export java "hello" hello :: List JInteger -> Java Hello JByteArray

hello :: List JInteger -> Java Hello JByteArray
hello list = do
  let bytes = map fromJava (fromJava list :: [JInteger]) :: [Byte]
  arr <- anew (length bytes)
  arr <.> mapM_ (uncurry aset) (zip [0..] bytes)
  return arr
