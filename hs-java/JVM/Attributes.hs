{-# LANGUAGE StandaloneDeriving, TypeFamilies, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module JVM.Attributes where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import JVM.Assembler
import JVM.Types
import JVM.InvokeDynamic

data Attribute =
  InnerClasses
  | BootstrapMethods {
      bootstrapMethod :: MethodHandle Direct,
      bootstrapArgs :: [BootstrapArg Direct] }
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

attributeNameString :: Attribute -> B.ByteString
attributeNameString Code {} = "Code"
attributeNameString BootstrapMethods {} = "BootstrapMethods"
attributeNameString _ = error $ "Invalid attribute"

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
type family Attributes stage

-- | At File stage, attributes are represented as list of Attribute structures.
type instance Attributes File = [RawAttribute]

-- | At Direct stage, attributes are represented as a Map.
type instance Attributes Direct = M.Map B.ByteString Attribute

insertAttribute :: Attribute -> Attributes Direct -> Attributes Direct
insertAttribute attribute = M.insert (attributeNameString attribute) attribute

-- | Size of attributes set at Direct stage
arsize :: Attributes Direct -> Int
arsize = M.size

-- | Associative list of attributes at Direct stage
arlist :: Attributes Direct -> [(B.ByteString, Attribute)]
arlist = M.assocs

-- | Size of attributes set at File stage
apsize :: Attributes File -> Int
apsize = length

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
