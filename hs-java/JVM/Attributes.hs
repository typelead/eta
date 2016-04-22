{-# LANGUAGE TypeFamilies, RecordWildCards, FlexibleInstances #-}
module JVM.Attributes where

import Data.Default
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import JVM.Assembler
import JVM.Types
import JVM.InvokeDynamic

-- | Object (class, method, field) attributes
data family Attributes stage

-- | At File stage, attributes are represented as list of Attribute structures.
data instance Attributes File = AP {attributesList :: [RawAttribute]}
  deriving (Eq, Show)

instance Default (Attributes File) where
  def = AP []

-- | At Direct stage, attributes are represented as a Map.
data instance Attributes Direct = AR (M.Map B.ByteString B.ByteString)
  deriving (Eq, Show)

instance Default (Attributes Direct) where
  def = AR M.empty

-- | Size of attributes set at Direct stage
arsize :: Attributes Direct -> Int
arsize (AR m) = M.size m

-- | Associative list of attributes at Direct stage
arlist :: Attributes Direct -> [(B.ByteString, B.ByteString)]
arlist (AR m) = M.assocs m

-- | Size of attributes set at File stage
apsize :: Attributes File -> Int
apsize (AP list) = length list

data Attribute =
  InnerClasses
  | BootstrapMethods (MethodHandle Direct) [BootstrapArg Direct]
  | Code0 [Instruction]
  | StackMapTable
  | ConstantValue

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
    putWord32be attributeLength
    putLazyByteString attributeValue

  get = do
    offset <- bytesRead
    name <- getWord16be
    len <- getWord32be
    value <- getLazyByteString (fromIntegral len)
    return $ RawAttribute name len value

class HasAttributes a where
  attributes :: a stage -> Attributes stage

-- -- | Format of Code method attribute.
-- data Code = Code {
--     codeStackSize :: Word16,
--     codeMaxLocals :: Word16,
--     codeLength :: Word32,
--     codeInstructions :: [Instruction],
--     codeExceptionsN :: Word16,
--     codeExceptions :: [CodeException],
--     codeAttrsN :: Word16,
--     codeAttributes :: Attributes File }
--   deriving (Eq, Show)

-- -- | Exception descriptor
-- data CodeException = CodeException {
--     eStartPC :: Word16,
--     eEndPC :: Word16,
--     eHandlerPC :: Word16,
--     eCatchType :: Word16 }
--   deriving (Eq, Show)

-- instance BinaryState Integer CodeException where
--   put CodeException {..} = do
--     put eStartPC
--     put eEndPC
--     put eHandlerPC
--     put eCatchType

--   get = CodeException <$> get <*> get <*> get <*> get

-- instance BinaryState Integer RawAttribute where
--   put a = do
--     let sz = 6 + attributeLength a      -- full size of AttributeInfo structure
--     liftOffset (fromIntegral sz) Binary.put a

--   get = getZ

-- instance BinaryState Integer Code where
--   put Code {..} = do
--     put codeStackSize
--     put codeMaxLocals
--     put codeLength
--     forM_ codeInstructions put
--     put codeExceptionsN
--     forM_ codeExceptions put
--     put codeAttrsN
--     forM_ (attributesList codeAttributes) put

--   get = do
--     stackSz <- get
--     locals <- get
--     len <- get
--     bytes <- replicateM (fromIntegral len) get
--     let bytecode = B.pack bytes
--         code = decodeWith readInstructions 0 bytecode
--     excn <- get
--     excs <- replicateM (fromIntegral excn) get
--     nAttrs <- get
--     attrs <- replicateM (fromIntegral nAttrs) get
--     return $ Code stackSz locals len code excn excs nAttrs (AP attrs)

-- -- | Decode Java method
-- decodeMethod :: B.ByteString -> Code
-- decodeMethod = decodeS (0 :: Integer)

-- -- | Encode Java method
-- encodeMethod :: Code -> B.ByteString
-- encodeMethod = encodeS (0 :: Integer)
