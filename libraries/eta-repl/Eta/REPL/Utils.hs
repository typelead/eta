module Eta.REPL.Utils (ByteBuffer(..), toByteBuffer, toByteArray, fromByteArray) where

import Java
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import GHC.Exts
import System.IO.Unsafe

toByteBuffer :: ByteString -> ByteBuffer
toByteBuffer bs =
  unsafePerformIO $ B.unsafeUseAsCStringLen bs
                  $ return . uncurry byteStringToByteBuffer

data ByteBuffer = ByteBuffer @java.nio.ByteBuffer
  deriving Class

foreign import java unsafe "@static eta.repl.Utils.byteStringToByteBuffer"
  byteStringToByteBuffer :: Ptr a -> Int -> ByteBuffer

fromByteArray :: JByteArray -> IO ByteString
fromByteArray bytes = do
  len <- javaWith bytes alength
  if len > 0
    then return $ B.unsafeCreate len (bytesToPtr bytes)
    else return $ B.empty

foreign import java unsafe "@static eta.repl.Utils.bytesToPtr"
  bytesToPtr :: JByteArray -> Ptr a -> IO ()

toByteArray :: ByteString -> JByteArray
toByteArray bs =
  unsafePerformIO $ B.unsafeUseAsCStringLen bs
                  $ return . uncurry byteStringToBytes

foreign import java unsafe "@static eta.repl.Utils.byteStringToBytes"
  byteStringToBytes :: Ptr a -> Int -> JByteArray
