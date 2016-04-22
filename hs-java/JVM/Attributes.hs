{-# LANGUAGE StandaloneDeriving, TypeFamilies, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module JVM.Attributes where

import Control.Monad
import Data.Default
import qualified Data.BinaryState as BS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import JVM.Assembler
import JVM.Types
import JVM.InvokeDynamic

data Attribute =
  InnerClasses
  | BootstrapMethods (MethodHandle Direct) [BootstrapArg Direct]
  | Code {
      codeStackSize :: Word16,
      codeMaxLocals :: Word16,
      codeLength :: Word32,
      codeInstructions :: [Instruction],
      codeExceptionsN :: Word16,
      codeExceptions :: [CodeException],
      codeAttrsN :: Word16,
      codeAttributes :: Attributes Direct }
  | StackMapTable
  | ConstantValue

deriving instance Show Attribute
deriving instance Eq Attribute

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data RawAttribute = RawAttribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: B.ByteString }
  deriving (Eq, Show)

instance Binary RawAttribute where
  put RawAttribute {..} = do
    put attributeName
    put attributeLength
    putLazyByteString attributeValue

  get = do
    name <- get
    len <- get
    value <- getLazyByteString (fromIntegral len)
    return $ RawAttribute name len value

-- | Object (class, method, field) attributes
data family Attributes stage

-- | At File stage, attributes are represented as list of Attribute structures.
data instance Attributes File = AP {attributesList :: [RawAttribute]}
  deriving (Eq, Show)

instance Default (Attributes File) where
  def = AP []

-- | At Direct stage, attributes are represented as a Map.
data instance Attributes Direct = AR (M.Map B.ByteString Attribute)
  deriving (Eq, Show)

instance Default (Attributes Direct) where
  def = AR M.empty

-- | Size of attributes set at Direct stage
arsize :: Attributes Direct -> Int
arsize (AR m) = M.size m

-- | Associative list of attributes at Direct stage
arlist :: Attributes Direct -> [(B.ByteString, Attribute)]
arlist (AR m) = M.assocs m

-- | Size of attributes set at File stage
apsize :: Attributes File -> Int
apsize (AP list) = length list

class HasAttributes a where
  attributes :: a stage -> Attributes stage

-- | Exception descriptor
data CodeException = CodeException {
    eStartPC :: Word16,
    eEndPC :: Word16,
    eHandlerPC :: Word16,
    eCatchType :: Word16 }
  deriving (Eq, Show)

instance Binary CodeException where
  put CodeException {..} = do
    put eStartPC
    put eEndPC
    put eHandlerPC
    put eCatchType

  get = CodeException <$> get <*> get <*> get <*> get

-- -- | Decode Java method
-- decodeMethod :: B.ByteString -> Code
-- decodeMethod = decodeS (0 :: Integer)

-- -- | Encode Java method
-- encodeMethod :: Code -> B.ByteString
-- encodeMethod = encodeS (0 :: Integer)
