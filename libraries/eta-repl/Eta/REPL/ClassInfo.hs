{-# LANGUAGE DeriveGeneric #-}
module Eta.REPL.ClassInfo where

import GHC.Generics
import Data.Binary

import Eta.REPL.Map

data ClassInfo =
  ClassInfo { ciName      :: String
            , ciSignature :: ClassSignature
            , ciType      :: ClassType
            , ciModifier  :: Maybe ClassModifier
            , ciMethods   :: Map String MethodInfo
            , ciFields    :: Map String FieldInfo }
  deriving (Generic, Eq, Show)

instance Binary ClassInfo

data MethodInfo =
  MethodInfo { miName      :: String
             , miSignature :: MethodSignature
             , miModifier  :: Maybe MethodModifier }
  deriving (Generic, Eq, Show)

instance Binary MethodInfo

data FieldInfo =
  FieldInfo { fiName      :: String
            , fiSignature :: FieldSignature }
  deriving (Generic, Eq, Show)

instance Binary FieldInfo

data ClassType = ClassType | InterfaceType | AnnotationType | EnumType
  deriving (Generic, Eq, Show)

instance Binary ClassType

data ClassModifier = AbstractClass | FinalClass
  deriving (Generic, Eq, Show)

instance Binary ClassModifier

data MethodModifier = AbstractMethod | FinalMethod
  deriving (Generic, Eq, Show)

instance Binary MethodModifier

-- Taken from codec-jvm

data PrimType
  = ByteType
  | CharType
  | DoubleType
  | FloatType
  | IntType
  | LongType
  | ShortType
  | BoolType
  deriving (Generic, Eq, Show)

instance Binary PrimType

-- | JavaTypeSignature
data Parameter
  = ReferenceParameter ReferenceParameter -- ^ ReferenceTypeSignature
  | PrimitiveParameter PrimType           -- ^ BaseType
  deriving (Generic, Eq, Show)

instance Binary Parameter

type ObjectType = String

-- | ReferenceTypeSignature
data ReferenceParameter
  = -- | ClassTypeSignature
    GenericReferenceParameter
      ObjectType                     -- PackageSpecifier & SimpleClassTypeSignature
      [TypeParameter]                -- TypeArguments
      [ReferenceParameter]           -- ClassTypeSignatureSuffix
    -- | TypeVariableSignature
  | VariableReferenceParameter
    -- | ArrayTypeSignature
  | ArrayReferenceParameter Parameter
  deriving (Generic, Eq, Show)

instance Binary ReferenceParameter

-- | TypeArgument, TypeParameter
data TypeParameter
  = WildcardTypeParameter Bound -- <?> <? extends A> <? super A>
  | SimpleTypeParameter ReferenceParameter
  deriving (Generic, Eq, Show)

instance Binary TypeParameter

data Bound
  = NotBounded
  | ExtendsBound ReferenceParameter
  | SuperBound   ReferenceParameter
  deriving (Generic, Eq, Show)

instance Binary Bound

-- TypeParameters
type TypeVariableDeclarations = [TypeVariableDeclaration]

data TypeVariableDeclaration = TypeVariableDeclaration String [Bound]
  deriving (Generic, Eq, Show)

instance Binary TypeVariableDeclaration

-- | ** ClassSignature **
data ClassSignature
  = ClassSignature
      TypeVariableDeclarations    -- TypeParameters
      [ClassParameter]            -- SuperclassSignature & SuperinterfaceSignature
  deriving (Generic, Eq, Show)

instance Binary ClassSignature

type ClassParameter = ReferenceParameter

-- | ** MethodSignature **
data MethodSignature =
  MethodSignature
    TypeVariableDeclarations  -- TypeParameters
    [MethodParameter]           -- JavaTypeSignature
    MethodReturn              -- Result
    ThrowsExceptions          -- ThrowsSignature
  deriving (Generic, Eq, Show)

instance Binary MethodSignature

-- | JavaTypeSignature
type MethodParameter = Parameter

-- | Result
type MethodReturn = Maybe Parameter

-- | ThrowsSignature
type ThrowsExceptions = [ReferenceParameter]

-- |  ** FieldSignature **
data FieldSignature = FieldSignature FieldParameter
  deriving (Generic, Eq, Show)

instance Binary FieldSignature

type FieldParameter = ReferenceParameter
