{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
-- | This module declares (low-level) data types for Java .class files
-- structures, and Binary instances to read/write them.
module JVM.ClassFile
  (-- * About
   -- $about
   --
   --
   -- * Internal class file structures
   RawAttribute (..),
   FieldType (..),
   -- * Signatures
   FieldSignature, MethodSignature (..), ReturnSignature (..),
   ArgumentSignature (..),
   -- * Stage types
   File, Direct,
   -- * Staged structures
   Pool, Link,
   Method (..), Field (..), Class (..),
   MethodHandle (..),
   Constant (..),
   AccessFlag (..), AccessFlags,
   Attributes (..),
   defaultClass,
   -- * Misc
   HasSignature (..), HasAttributes (..),
   NameType (..),
   fieldNameType, methodNameType,
   lookupField, lookupMethod,
   long,
   toString,
   className,
   apsize, arsize, arlist
  )
  where

import Control.Monad
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as St
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.List
import Data.Default
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String hiding (encode, decode)

-- $about
--
-- Java .class file uses constants pool, which stores almost all source-code-level
-- constants (strings, integer literals etc), and also all identifiers (class,
-- method, field names etc). All other structures contain indexes of constants in
-- the pool instead of constants themselves.
--
-- It's not convient to use that indexes programmatically. So, .class file is represented
-- at two stages: File and Direct. At File stage, all data structures contain only indexes,
-- not constants theirself. When we read a class from a file, we get structure at File stage.
-- We only can write File stage structure to file.
--
-- At Direct stage, structures conain constants, not indexes. Convertion functions (File <-> Direct)
-- are located in the JVM.Converter module.
--

-- | Read one-byte Char
getChar8 :: Get Char
getChar8 = do
  x <- getWord8
  return $ chr (fromIntegral x)

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

-- | File stage
data File = File

-- | Direct representation stage
data Direct = Direct

-- | Link to some object
type family Link stage a

-- | At File stage, Link contain index of object in the constants pool.
type instance Link File a = Word16

-- | At Direct stage, Link contain object itself.
type instance Link Direct a = a

-- | Link to bootstrap method in the 'BootstrapMethods' attribute
type BootstrapLink stage a = Link stage a

-- | Object (class, method, field …) access flags
type family AccessFlags stage

-- | At File stage, access flags are represented as Word16
type instance AccessFlags File = Word16

-- | At Direct stage, access flags are represented as set of flags.
type instance AccessFlags Direct = S.Set AccessFlag

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

-- | Access flags. Used for classess, methods, variables.
data AccessFlag =
    ACC_PUBLIC        -- ^ 0x0001 Visible for all
  | ACC_PRIVATE       -- ^ 0x0002 Visible only for defined class
  | ACC_PROTECTED     -- ^ 0x0004 Visible only for subclasses
  | ACC_STATIC        -- ^ 0x0008 Static method or variable
  | ACC_FINAL         -- ^ 0x0010 No further subclassing or assignments
  | ACC_SYNCHRONIZED  -- ^ 0x0020 Uses monitors
  | ACC_VOLATILE      -- ^ 0x0040 Could not be cached
  | ACC_TRANSIENT     -- ^ 0x0080
  | ACC_NATIVE        -- ^ 0x0100 Implemented in other language
  | ACC_INTERFACE     -- ^ 0x0200 Class is interface
  | ACC_ABSTRACT      -- ^ 0x0400
  | ACC_STRICT        -- ^ 0x0800 Floating-point mode
  | ACC_SYNTHETIC     -- ^ 0x1000 Not present in source code
  | ACC_ANNOTATION    -- ^ 0x2000
  | ACC_ENUM          -- ^ 0x4000
  deriving (Eq, Show, Ord, Enum)

-- | Fields and methods have signatures.
class (Binary (Signature a), Show (Signature a), Eq (Signature a))
    => HasSignature a where
  type Signature a

instance HasSignature (Field Direct) where
  type Signature (Field Direct) = FieldSignature

instance HasSignature (Method Direct) where
  type Signature (Method Direct) = MethodSignature

-- | Name and signature pair. Used for methods and fields.
data NameType a = NameType {
  ntName :: B.ByteString,
  ntSignature :: Signature a }

instance (HasSignature a) => Show (NameType a) where
  show (NameType n t) = toString n ++ ": " ++ show t

deriving instance HasSignature a => Eq (NameType a)

instance HasSignature a => Binary (NameType a) where
  put (NameType n t) = putLazyByteString n >> put t

  get = NameType <$> get <*> get

-- | Constant pool item
data Constant stage =
    CClass (Link stage B.ByteString)
  | CField (Link stage B.ByteString) (Link stage (NameType (Field stage)))
  | CMethod (Link stage B.ByteString) (Link stage (NameType (Method stage)))
  | CIfaceMethod (Link stage B.ByteString) (Link stage (NameType (Method stage)))
  | CString (Link stage B.ByteString)
  | CInteger Word32
  | CFloat Float
  | CLong Word64
  | CDouble Double
  | CNameType (Link stage B.ByteString) (Link stage B.ByteString)
  | CUTF8 {getString :: B.ByteString}
  | CUnicode {getString :: B.ByteString} -- What is this for?
  | CMethodHandle (Link stage (MethodHandle stage))
  | CMethodType (Link stage B.ByteString)
  | CInvokeDynamic (BootstrapLink stage (BootstrapMethod stage))
                   (Link stage (NameType (Method stage)))

-- | Name of the CClass. Error on any other constant.
className ::  Constant Direct -> B.ByteString
className (CClass s) = s
className x = error $ "Not a class: " ++ show x

instance Show (Constant Direct) where
  show (CClass name) = "class " ++ toString name
  show (CField cls nt) = "field " ++ toString cls ++ "." ++ show nt
  show (CMethod cls nt) = "method " ++ toString cls ++ "." ++ show nt
  show (CIfaceMethod cls nt) = "interface method " ++ toString cls ++ "." ++ show nt
  show (CString s) = "String \"" ++ toString s ++ "\""
  show (CInteger x) = show x
  show (CFloat x) = show x
  show (CLong x) = show x
  show (CDouble x) = show x
  show (CNameType name tp) = toString name ++ ": " ++ toString tp
  show (CUTF8 s) = "UTF8 \"" ++ toString s ++ "\""
  show (CUnicode s) = "Unicode \"" ++ toString s ++ "\""

-- | Constant pool
type Pool stage = M.Map Word16 (Constant stage)

-- | Generic .class file format
data Class stage = Class {
  magic :: Word32,                         -- ^ Magic value: 0xCAFEBABE
  minorVersion :: Word16,
  majorVersion :: Word16,
  constsPoolSize :: Word16,                -- ^ Number of items in constants pool
  constsPool :: Pool stage,                -- ^ Constants pool itself
  accessFlags :: AccessFlags stage,        -- ^ See @JVM.Types.AccessFlag@
  thisClass :: Link stage B.ByteString,    -- ^ Constants pool item index for this class
  superClass :: Link stage B.ByteString,   -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount :: Word16,               -- ^ Number of implemented interfaces
  interfaces :: [Link stage B.ByteString], -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount :: Word16,              -- ^ Number of class fileds
  classFields :: [Field stage],            -- ^ Class fields
  classMethodsCount :: Word16,             -- ^ Number of class methods
  classMethods :: [Method stage],          -- ^ Class methods
  classAttributesCount :: Word16,          -- ^ Number of class attributes
  classAttributes :: Attributes stage      -- ^ Class attributes
  }

deriving instance Eq (Class File)
deriving instance Eq (Class Direct)
deriving instance Show (Class File)
deriving instance Show (Class Direct)

deriving instance Eq (Constant File)
deriving instance Eq (Constant Direct)
deriving instance Show (Constant File)

-- | Default (empty) class file definition.
defaultClass :: (Default (AccessFlags stage), Default (Link stage B.ByteString), Default (Attributes stage))
             => Class stage
defaultClass = Class {
  magic = 0xCAFEBABE,
  minorVersion = 0,
  majorVersion = 50,
  constsPoolSize = 0,
  constsPool = def,
  accessFlags = def,
  thisClass = def,
  superClass = def,
  interfacesCount = 0,
  interfaces = [],
  classFieldsCount = 0,
  classFields = [],
  classMethodsCount = 0,
  classMethods = [],
  classAttributesCount = 0,
  classAttributes = def }

instance Binary (Class File) where
  put Class {..} = do
    put magic
    put minorVersion
    put majorVersion
    putPool constsPool
    put accessFlags
    put thisClass
    put superClass
    put interfacesCount
    forM_ interfaces put
    put classFieldsCount
    forM_ classFields put
    put classMethodsCount
    forM_ classMethods put
    put classAttributesCount
    forM_ (attributesList classAttributes) put

  get = do
    magic <- get
    when (magic /= 0xCAFEBABE) $
      fail $ "Invalid .class file MAGIC value: " ++ show magic
    minor <- get
    major <- get
    when (major > 50) $
      fail $ "Too new .class file format: " ++ show major
    poolsize <- getWord16be
    pool <- getPool (poolsize - 1)
    af <-  get
    this <- get
    super <- get
    interfacesCount <- get
    ifaces <- replicateM (fromIntegral interfacesCount) get
    classFieldsCount <- getWord16be
    classFields <- replicateM (fromIntegral classFieldsCount) get
    classMethodsCount <- get
    classMethods <- replicateM (fromIntegral classMethodsCount) get
    asCount <- get
    as <- replicateM (fromIntegral asCount) get
    return $ Class magic minor major poolsize pool af this super
               interfacesCount ifaces classFieldsCount classFields
               classMethodsCount classMethods asCount (AP as)

-- | Field signature format
data FieldType =
    SignedByte -- ^ B
  | CharByte   -- ^ C
  | DoubleType -- ^ D
  | FloatType  -- ^ F
  | IntType    -- ^ I
  | LongInt    -- ^ J
  | ShortInt   -- ^ S
  | BoolType   -- ^ Z
  | ObjectType String -- ^ L @{class name}@
  | Array (Maybe Int) FieldType -- ^ @[{type}@
  deriving (Eq, Ord)

instance Show FieldType where
  show SignedByte = "byte"
  show CharByte = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show LongInt = "long"
  show ShortInt = "short"
  show BoolType = "bool"
  show (ObjectType s) = "Object " ++ s
  show (Array Nothing t) = show t ++ "[]"
  show (Array (Just n) t) = show t ++ "[" ++ show n ++ "]"

-- | Class field signature
type FieldSignature = FieldType

-- | Try to read integer value from decimal representation
getInt :: Get (Maybe Int)
getInt = do
    s <- getDigits
    if null s
      then return Nothing
      else return $ Just (read s)
  where
    getDigits :: Get String
    getDigits = do
      c <- lookAhead getChar8
      if isDigit c
        then do
             skip 1
             next <- getDigits
             return (c: next)
        else return []

putString :: String -> Put
putString str = forM_ str put

instance Binary FieldType where
  put SignedByte = put 'B'
  put CharByte   = put 'C'
  put DoubleType = put 'D'
  put FloatType  = put 'F'
  put IntType    = put 'I'
  put LongInt    = put 'J'
  put ShortInt   = put 'S'
  put BoolType   = put 'Z'
  put (ObjectType name) = put 'L' >> putString name >> put ';'
  put (Array Nothing sig) = put '[' >> put sig
  put (Array (Just n) sig) = put '[' >> put (show n) >> put sig

  get = do
    b <- getChar8
    case b of
      'B' -> return SignedByte
      'C' -> return CharByte
      'D' -> return DoubleType
      'F' -> return FloatType
      'I' -> return IntType
      'J' -> return LongInt
      'S' -> return ShortInt
      'Z' -> return BoolType
      'L' -> do
             name <- getToSemicolon
             return (ObjectType name)
      '[' -> do
             mbSize <- getInt
             sig <- get
             return (Array mbSize sig)
      _   -> fail $ "Unknown signature opening symbol: " ++ [b]

-- | Read string up to `;'
getToSemicolon :: Get String
getToSemicolon = do
  x <- get
  if x == ';'
    then return []
    else do
         next <- getToSemicolon
         return (x: next)

-- | Return value signature
data ReturnSignature =
    Returns FieldType
  | ReturnsVoid
  deriving (Eq, Ord)

instance Show ReturnSignature where
  show (Returns t) = show t
  show ReturnsVoid = "Void"

instance Binary ReturnSignature where
  put (Returns sig) = put sig
  put ReturnsVoid   = put 'V'

  get = do
    x <- lookAhead getChar8
    case x of
      'V' -> skip 1 >> return ReturnsVoid
      _   -> Returns <$> get

-- | Method argument signature
type ArgumentSignature = FieldType

-- | Class method argument signature
data MethodSignature =
    MethodSignature [ArgumentSignature] ReturnSignature
  deriving (Eq, Ord)

instance Show MethodSignature where
  show (MethodSignature args ret) = "(" ++ intercalate ", " (map show args) ++ ") returns " ++ show ret

instance Binary MethodSignature where
  put (MethodSignature args ret) = do
    put '('
    forM_ args put
    put ')'
    put ret

  get =  do
    x <- getChar8
    when (x /= '(') $
      fail "Cannot parse method signature: no starting `(' !"
    args <- getArgs
    y <- getChar8
    when (y /= ')') $
      fail "Internal error: method signature without `)' !?"
    ret <- get
    return (MethodSignature args ret)

-- | Read arguments signatures (up to `)')
getArgs :: Get [ArgumentSignature]
getArgs = whileJust getArg
  where
    getArg :: Get (Maybe ArgumentSignature)
    getArg = do
      x <- lookAhead getChar8
      if x == ')'
        then return Nothing
        else Just <$> get

whileJust :: (Monad m) => m (Maybe a) -> m [a]
whileJust m = do
  r <- m
  case r of
    Just x -> do
              next <- whileJust m
              return (x: next)
    Nothing -> return []

long :: Constant stage -> Bool
long (CLong _)   = True
long (CDouble _) = True
long _           = False

putPool :: Pool File -> Put
putPool pool = do
    let list = M.elems pool
        d = length $ filter long list
    putWord16be $ fromIntegral (M.size pool + d + 1)
    forM_ list putC
  where
    putC (CClass i) = putWord8 7 >> put i
    putC (CField i j) = putWord8 9 >> put i >> put j
    putC (CMethod i j) = putWord8 10 >> put i >> put j
    putC (CIfaceMethod i j) = putWord8 11 >> put i >> put j
    putC (CString i) = putWord8 8 >> put i
    putC (CInteger x) = putWord8 3 >> put x
    putC (CFloat x)   = putWord8 4 >> putFloat32be x
    putC (CLong x)    = putWord8 5 >> put x
    putC (CDouble x)  = putWord8 6 >> putFloat64be x
    putC (CNameType i j) = putWord8 12 >> put i >> put j
    putC (CUTF8 bs) = do
        putWord8 1
        put (fromIntegral (B.length bs) :: Word16)
        putLazyByteString bs
    putC (CUnicode bs) = do
        putWord8 2
        put (fromIntegral (B.length bs) :: Word16)
        putLazyByteString bs
    -- TODO: Fix these
    putC (CMethodHandle c) = putWord8 15 >> put c
    putC (CMethodType i) = putWord8 16 >> put i
    putC (CInvokeDynamic i j) = putWord8 18 >> put i >> put j

getPool :: Word16 -> Get (Pool File)
getPool n = do
    items <- St.evalStateT go 1
    return $ M.fromList items
  where
    go :: St.StateT Word16 Get [(Word16, Constant File)]
    go = do
      i <- St.get
      if i > n
        then return []
        else do
          c <- lift getC
          let i' = if long c
                      then i+2
                      else i+1
          St.put i'
          next <- go
          return $ (i,c): next

    getC = do
      !offset <- bytesRead
      tag <- getWord8
      case tag of
        1 -> do
          l <- get
          bs <- getLazyByteString (fromIntegral (l :: Word16))
          return $ CUTF8 bs
        2 -> do
          l <- get
          bs <- getLazyByteString (fromIntegral (l :: Word16))
          return $ CUnicode bs
        3  -> CInteger   <$> get
        4  -> CFloat     <$> getFloat32be
        5  -> CLong      <$> get
        6  -> CDouble    <$> getFloat64be
        7  -> CClass     <$> get
        8  -> CString    <$> get
        9  -> CField     <$> get <*> get
        10 -> CMethod    <$> get <*> get
        11 -> CIfaceMethod <$> get <*> get
        12 -> CNameType    <$> get <*> get
        15 -> CMethodHandle <$> get
        16 -> CMethodType <$> get
        18 -> CInvokeDynamic <$> get <*> get
        _  -> fail $ "Unknown constants pool entry tag: " ++ show tag

-- | Class field format
data Field stage = Field {
  fieldAccessFlags :: AccessFlags stage,
  fieldName :: Link stage B.ByteString,
  fieldSignature :: Link stage FieldSignature,
  fieldAttributesCount :: Word16,
  fieldAttributes :: Attributes stage }

deriving instance Eq (Field File)
deriving instance Eq (Field Direct)
deriving instance Show (Field File)
deriving instance Show (Field Direct)

lookupField :: B.ByteString -> Class Direct -> Maybe (Field Direct)
lookupField name cls = look (classFields cls)
  where
    look [] = Nothing
    look (f:fs)
      | fieldName f == name = Just f
      | otherwise           = look fs

fieldNameType :: Field Direct -> NameType (Field Direct)
fieldNameType f = NameType (fieldName f) (fieldSignature f)

instance Binary (Field File) where
  put Field {..} = do
    put fieldAccessFlags
    put fieldName
    put fieldSignature
    put fieldAttributesCount
    forM_ (attributesList fieldAttributes) put

  get = do
    af <- get
    ni <- getWord16be
    si <- get
    n <- getWord16be
    as <- replicateM (fromIntegral n) get
    return $ Field af ni si n (AP as)

-- | Class method format
data Method stage = Method {
  methodAccessFlags :: AccessFlags stage,
  methodName :: Link stage B.ByteString,
  methodSignature :: Link stage MethodSignature,
  methodAttributesCount :: Word16,
  methodAttributes :: Attributes stage }

deriving instance Eq (Method File)
deriving instance Eq (Method Direct)
deriving instance Show (Method File)
deriving instance Show (Method Direct)

methodNameType :: Method Direct -> NameType (Method Direct)
methodNameType m = NameType (methodName m) (methodSignature m)

lookupMethod :: B.ByteString -> Class Direct -> Maybe (Method Direct)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise           = look fs

instance Binary (Method File) where
  put Method {..} = do
    put methodAccessFlags
    put methodName
    put methodSignature
    put methodAttributesCount
    forM_ (attributesList methodAttributes) put

  get = do
    offset <- bytesRead
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return Method {
      methodAccessFlags = af,
      methodName = ni,
      methodSignature = si,
      methodAttributesCount = n,
      methodAttributes = AP as }

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data RawAttribute = RawAttribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: B.ByteString }
  deriving (Eq, Show)

data Attribute =
  InnerClasses
  | BootstrapMethods

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

instance HasAttributes Class where
  attributes = classAttributes

instance HasAttributes Field where
  attributes = fieldAttributes

instance HasAttributes Method where
  attributes = methodAttributes

data ReferenceKind =
  RefGetField
  | RefGetStatic
  | RefPutField
  | RefPutStatic
  | RefInvokeVirtual
  | RefInvokeStatic
  | RefInvokeSpecial
  | RefNewInvokeSpecial
  | RefInvokeInterface

data MethodHandle stage = MethodHandle {
  methodHandleRef :: Link stage (Constant stage) -- Should be a reference type
}

deriving instance Eq (MethodHandle File)
deriving instance Eq (MethodHandle Direct)

data BootstrapMethod stage = BootstrapMethod {
  bootstrapMethod :: Link stage (MethodHandle stage),
  bootstrapArgs :: [Link stage (Constant stage)] -- Restrict the type of constants
}

deriving instance Eq (BootstrapMethod File)
deriving instance Eq (BootstrapMethod Direct)
