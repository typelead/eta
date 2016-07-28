{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Types where

import Data.Binary.Put (Put, putWord16be)
import Data.Set (Set)
import Data.Word (Word16)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.String (IsString)

import qualified Data.Set as S
import qualified Data.Text as Text

data PrimType
  = JByte
  | JChar
  | JDouble
  | JFloat
  | JInt
  | JLong
  | JShort
  | JBool
  deriving (Eq, Ord, Show)

jbyte, jchar, jdouble, jfloat, jint, jlong, jshort, jbool, jobject :: FieldType
jbyte = BaseType JByte
jchar = BaseType JChar
jdouble = BaseType JDouble
jfloat = BaseType JFloat
jint = BaseType JInt
jlong = BaseType JLong
jshort = BaseType JShort
jbool = BaseType JBool
jobject = ObjectType jlObject

jarray :: FieldType -> FieldType
jarray = ArrayType

baseType :: FieldType -> PrimType
baseType (BaseType pt) = pt
baseType _ = error "baseType: Not base type!"

jstring :: FieldType
jstring = ObjectType jlString

jstringC :: Text
jstringC = "java/lang/String"

jlObject :: IClassName
jlObject = IClassName "java/lang/Object"

jlString :: IClassName
jlString = IClassName jstringC

-- | Binary class names in their internal form.
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.2.1
newtype IClassName = IClassName Text
  deriving (Eq, Ord, Show, IsString)

-- | Unqualified name
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms4.html#jvms-4.2.2
newtype UName = UName Text
  deriving (Eq, Ord, Show, IsString)

-- | Field descriptor
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.2
newtype FieldDesc = FieldDesc Text
  deriving (Eq, Ord, Show)

data FieldType = BaseType PrimType | ObjectType  IClassName | ArrayType FieldType
  deriving (Eq, Ord, Show)

isCategory2 :: FieldType -> Bool
isCategory2 (BaseType JLong)   = True
isCategory2 (BaseType JDouble) = True
isCategory2 _                   = False

mkFieldDesc :: FieldType -> FieldDesc
mkFieldDesc ft = FieldDesc $ mkFieldDesc' ft where

mkFieldDesc' :: FieldType -> Text
mkFieldDesc' ft = case ft of
  BaseType JByte              -> "B"
  BaseType JChar              -> "C"
  BaseType JDouble            -> "D"
  BaseType JFloat             -> "F"
  BaseType JInt               -> "I"
  BaseType JLong              -> "J"
  BaseType JShort             -> "S"
  BaseType JBool              -> "Z"
  ObjectType (IClassName cn)  -> fold ["L", cn, ";"]
  ArrayType ft'               -> Text.concat ["[", mkFieldDesc' ft']

fieldSize :: FieldType -> Int
fieldSize (BaseType JLong)    = 2
fieldSize (BaseType JDouble)  = 2
fieldSize _                   = 1

prim :: PrimType -> FieldType
prim = BaseType

obj :: Text -> FieldType
obj = ObjectType . IClassName

arr :: FieldType -> FieldType
arr = ArrayType

data MethodType = MethodType [FieldType] ReturnType

type ReturnType = Maybe FieldType

void :: ReturnType
void = Nothing

ret :: FieldType -> ReturnType
ret = Just

-- | Method descriptor
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.3
data MethodDesc = MethodDesc Text
  deriving (Eq, Ord, Show)

mkMethodDesc :: [FieldType] -> ReturnType -> MethodDesc
mkMethodDesc fts rt = MethodDesc (mkMethodDesc' fts rt)

mkMethodDesc' :: [FieldType] -> ReturnType -> Text
mkMethodDesc' fts rt = Text.concat ["(", args, ")", result] where
  args = Text.concat $ mkFieldDesc' <$> fts
  result  = maybe "V" mkFieldDesc' rt

-- | Field or method reference
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.2

data FieldRef = FieldRef IClassName UName FieldType
  deriving (Eq, Ord, Show)

mkFieldRef :: Text -> Text -> FieldType -> FieldRef
mkFieldRef cn un ft = FieldRef (IClassName cn) (UName un) ft

data MethodRef = MethodRef IClassName UName [FieldType] ReturnType
  deriving (Eq, Ord, Show)

mkMethodRef :: Text -> Text -> [FieldType] -> ReturnType -> MethodRef
mkMethodRef cn un fts rt = MethodRef (IClassName cn) (UName un) fts rt

data NameAndDesc = NameAndDesc UName Desc
  deriving (Eq, Ord, Show)

-- | Field or method descriptor
newtype Desc = Desc Text
  deriving (Eq, Ord, Show)

data Version = Version
  { versionMaj :: Int
  , versionMin :: Int }
  deriving (Eq, Ord, Show)

java8 :: Version
java8 = Version 52 0

java7 :: Version
java7 = Version 51 0

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6-200-A.1
data AccessFlag
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Super
  | Synchronized
  | Volatile
  | Bridge
  | VarArgs
  | Transient
  | Native
  | Interface
  | Abstract
  | Strict
  | Synthetic
  | Annotation
  | Enum
  deriving (Eq, Ord, Show)

accessFlagValue :: AccessFlag -> Word16
accessFlagValue Public        = 0x0001
accessFlagValue Private       = 0x0002
accessFlagValue Protected     = 0x0004
accessFlagValue Static        = 0x0008
accessFlagValue Final         = 0x0010
accessFlagValue Super         = 0x0020
accessFlagValue Synchronized  = 0x0020
accessFlagValue Volatile      = 0x0040
accessFlagValue Bridge        = 0x0040
accessFlagValue VarArgs       = 0x0080
accessFlagValue Transient     = 0x0080
accessFlagValue Native        = 0x0100
accessFlagValue Interface     = 0x0200
accessFlagValue Abstract      = 0x0400
accessFlagValue Strict        = 0x0800
accessFlagValue Synthetic     = 0x1000
accessFlagValue Annotation    = 0x2000
accessFlagValue Enum          = 0x4000

putAccessFlags :: Set AccessFlag -> Put
putAccessFlags accessFlags = putWord16be $ sum (accessFlagValue <$> (S.toList accessFlags))

newtype Label = Label Int

mkLabel :: Int -> Label
mkLabel = Label
