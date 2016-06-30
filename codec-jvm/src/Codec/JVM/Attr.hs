{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Attr where

import Data.ByteString (ByteString)
import Data.Binary.Put (Put, putByteString, putWord8, runPut)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Data.List (foldl')

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text

import Codec.JVM.ASM.Code.Types (Offset(..))
import Codec.JVM.Const (Const(CUTF8))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16, putI32)
import Codec.JVM.Types (PrimType(..), FieldType(..))

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString
    , codeAttrs :: [Attr] }
  | AStackMapTable [(Offset, StackMapFrame)]

instance Show Attr where
  show attr = "A" ++ (Text.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _ _)      = "Code"
attrName (AStackMapTable _)   = "StackMapTable"

unpackAttr :: Attr -> [Const]
unpackAttr attr@(ACode _ _ _ xs) = (CUTF8 $ attrName attr):(unpackAttr =<< xs)
unpackAttr attr = return . CUTF8 . attrName $ attr

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: ConstPool -> Attr -> Put
putAttrBody cp (ACode ms ls xs attrs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 $ length attrs
  mapM_ (putAttr cp) attrs
putAttrBody _ (AStackMapTable xs) = do
  putI16 $ length xs
  putStackMapFrames xs

-- | http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4
--
-- Offsets are absolute (the delta conversion happen during serialization)

data StackMapFrame
  = SameFrame
  | SameLocals VerifType
  | FullFrame [VerifType] [VerifType]
  deriving (Eq, Show)

putStackMapFrames :: [(Offset, StackMapFrame)] -> Put
putStackMapFrames xs = snd $ foldl' f ((0, return ())) xs where
  f (offset, put) (Offset frameOffset, frame) = (frameOffset, put *> putFrame frame) where
    delta = fromIntegral $ frameOffset - if offset == 0 then 0 else offset + 1
    putFrame SameFrame =
      putWord8 $ delta
    putFrame (SameLocals vt) = do
      putWord8 $ delta + 64
      putVerifType vt
    putFrame (FullFrame locals stack) = do
      putWord8 255
      putI16 $ fromIntegral delta
      putI16 $ length locals
      traverse_ putVerifType locals
      putI16 $ length stack
      traverse_ putVerifType stack

newtype VerifType = VerifType FieldType
  deriving (Eq, Show)

putVerifType :: VerifType -> Put
putVerifType (VerifType (BaseType JInt)) = putWord8 1
