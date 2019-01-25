{-# LANGUAGE DataKinds, FlexibleContexts #-}
import Java
import java "java.nio.ByteBuffer"

main :: IO ()
main = java $ do
  buf <- ByteBuffer.allocateDirect 10
  result <- buf <.> ByteBuffer.isDirect
  io $ print result
