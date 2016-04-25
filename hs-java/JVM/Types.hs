{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleContexts  #-}
module JVM.Types where

import JVM.Common

import Control.Monad
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S

-- $about
--
-- Java .class file uses constants pool, which stores almost all source-code-level
-- constants (strings, integer literals etc), and also all identifiers (class,
-- method, field names etc). All other structures contain indexes of constants in
-- the pool instead of constants themselves.
--
-- It's not convenient to use that indexes programmatically. So, .class file is represented
-- at two stages: File and Direct. At File stage, all data structures contain only indexes,
-- not constants theirself. When we read a class from a file, we get structure at File stage.
-- We only can write File stage structure to file.
--
-- At Direct stage, structures conain constants, not indexes. Convertion functions (File <-> Direct)
-- are located in the JVM.Converter module.
--

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
  | ObjectType String  -- ^ L @{class name}@
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


-- | Method argument signature
type ArgumentSignature = FieldType

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
