{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards, FlexibleInstances #-}
module JVM.DataTypes where

import Control.Monad
import JVM.Types
import JVM.Attributes
import JVM.InvokeDynamic
import JVM.Common
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B

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

deriving instance Eq (Constant File)
deriving instance Eq (Constant Direct)
deriving instance Show (Constant File)

-- | Name of the CClass. Error on any other constant.
className ::  Constant Direct -> B.ByteString
className (CClass s) = s
className x = error $ "Not a class: " ++ show x

long :: Constant stage -> Bool
long (CLong _)   = True
long (CDouble _) = True
long _           = False

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

instance HasAttributes Field where
  attributes = fieldAttributes

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

fieldNameType :: Field Direct -> NameType (Field Direct)
fieldNameType f = NameType (fieldName f) (fieldSignature f)

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

instance HasSignature (Field Direct) where
  type Signature (Field Direct) = FieldSignature

instance HasSignature (Method Direct) where
  type Signature (Method Direct) = MethodSignature

instance HasAttributes Method where
  attributes = methodAttributes

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

methodNameType :: Method Direct -> NameType (Method Direct)
methodNameType m = NameType (methodName m) (methodSignature m)


