{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Types where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.String (IsString)

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

jInt :: FieldType
jInt = BaseType JInt

jString :: FieldType
jString = ObjectType jlString

jlObject :: IClassName
jlObject = IClassName "java/lang/Object"

jlString :: IClassName
jlString = IClassName "java/lang/String"

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
fieldSize (BaseType JFloat)   = 2
fieldSize (ObjectType _ )     = 2
fieldSize (ArrayType _)       = 2
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

-- | Method descriptor
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.3
data MethodDesc = MethodDesc Text Int -- number of arguments
  deriving (Eq, Ord, Show)

mkMethodDesc :: [FieldType] -> ReturnType -> MethodDesc
mkMethodDesc fts rt = MethodDesc (mkMethodDesc' fts rt) (length fts)

mkMethodDesc' :: [FieldType] -> ReturnType -> Text
mkMethodDesc' fts rt = Text.concat ["(", args, ")", ret] where
  args = Text.concat $ mkFieldDesc' <$> fts
  ret  = maybe "V" mkFieldDesc' rt

-- | Field or method reference
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.2

data FieldRef = FieldRef IClassName UName FieldType
  deriving (Eq, Ord, Show)

mkFieldRef :: IClassName -> UName -> FieldType -> FieldRef
mkFieldRef cn un ft = FieldRef cn un ft

data MethodRef = MethodRef IClassName UName [FieldType] ReturnType
  deriving (Eq, Ord, Show)

mkMethodRef :: IClassName -> UName -> [FieldType] -> ReturnType -> MethodRef
mkMethodRef cn un fts rt = MethodRef cn un fts rt

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

