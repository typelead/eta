{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Eta.REPL.ClassInfo where

import GHC.Generics
import Data.Binary
import Data.Maybe

import Eta.REPL.Map

data ClassInfo =
  ClassInfo { ciName              :: String
            , ciSignature         :: ClassSignature
            , ciType              :: ClassType
            , ciModifier          :: Maybe ClassModifier
            , ciInnerClasses      :: [String]
            , ciOtherInnerClasses :: [String]
            , ciConstructors      :: [MethodSignature]
            , ciMethods           :: Map String MethodInfo
            , ciOtherMethods      :: [String]
            , ciFields            :: Map String FieldInfo
            , ciOtherFields       :: [String]
            , ciParentClass       :: Maybe String }
  deriving (Generic, Eq, Show)

classInfoClasses :: ClassInfo -> [String]
classInfoClasses ClassInfo {..} =
  csSimpleClasses ciSignature ++ maybeToList ciParentClass ++ ciInnerClasses

instance Binary ClassInfo

data MethodInfo =
  MethodInfo { miName      :: String
             , miSignature :: MethodSignature
             , miModifier  :: Maybe MethodModifier }
  deriving (Generic, Eq, Show)

instance Binary MethodInfo

data FieldInfo =
  FieldInfo { fiName      :: String
            , fiSignature :: FieldSignature
            , fiModifier  :: Maybe FieldModifier }
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

data FieldModifier = FinalField
  deriving (Generic, Eq, Show)

instance Binary FieldModifier

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
  = ReferenceParameter { paramRef  :: ReferenceParameter }
  | PrimitiveParameter { paramPrim :: PrimType }
  deriving (Generic, Eq, Show)

mkRefParam :: ReferenceParameter -> Parameter
mkRefParam refParam = ReferenceParameter { paramRef = refParam }

mkPrimParam :: PrimType -> Parameter
mkPrimParam primType = PrimitiveParameter { paramPrim = primType }

instance Binary Parameter

type ObjectType = String

data ReferenceParameter
  = GenericReferenceParameter  { rpObjectType :: ObjectType
                               , rpTyParams   :: [TypeParameter] }
  | VariableReferenceParameter { rpVariable :: String }
  | ArrayReferenceParameter    { rpArrayComponent :: Parameter}
  deriving (Generic, Eq, Show)

mkGenericRefParam :: ObjectType -> [TypeParameter] -> ReferenceParameter
mkGenericRefParam cls params =
  GenericReferenceParameter { rpObjectType = cls, rpTyParams = params }

mkVarRefParam :: String -> ReferenceParameter
mkVarRefParam name = VariableReferenceParameter { rpVariable = name }

mkArrayRefParam :: Parameter -> ReferenceParameter
mkArrayRefParam param = ArrayReferenceParameter { rpArrayComponent = param }

rfSimpleClasses :: ReferenceParameter -> [String]
rfSimpleClasses rp = case rp of
  GenericReferenceParameter {..} -> [rpObjectType]
  _ -> []

instance Binary ReferenceParameter

-- | TypeArgument, TypeParameter
data TypeParameter
  = WildcardTypeParameter { tpBound :: Bound } -- <?> <? extends A> <? super A>
  | SimpleTypeParameter   { tpParam :: ReferenceParameter }
  deriving (Generic, Eq, Show)

instance Binary TypeParameter

mkWildcardTyParam :: Bound -> TypeParameter
mkWildcardTyParam bound = WildcardTypeParameter { tpBound = bound }

mkRefTyParam :: ReferenceParameter -> TypeParameter
mkRefTyParam refParam = SimpleTypeParameter { tpParam = refParam }

data Bound
  = NotBounded
  | ExtendsBound { boundParam :: [ReferenceParameter] }
  | SuperBound   { boundParam :: [ReferenceParameter] }
  deriving (Generic, Eq, Show)

mkWildcardBound :: [ReferenceParameter] -> [ReferenceParameter] -> Bound
mkWildcardBound lowerBounds upperBounds
  | null lowerBounds = ExtendsBound { boundParam = upperBounds }
  | otherwise = SuperBound { boundParam = lowerBounds }

instance Binary Bound

-- TypeParameters
type TypeVariableDeclarations = [TypeVariableDeclaration]

data TypeVariableDeclaration =
  TypeVariableDeclaration  { tvdName   :: String
                           , tvdBounds :: [ReferenceParameter] }
  deriving (Generic, Eq, Show)

instance Binary TypeVariableDeclaration

mkTyVarDecl :: String -> [ReferenceParameter] -> TypeVariableDeclaration
mkTyVarDecl name rps = TypeVariableDeclaration { tvdName = name, tvdBounds = rps }

-- | ** ClassSignature **
data ClassSignature
  = ClassSignature { csTyVarDecls :: TypeVariableDeclarations
                   , csParams     :: [ClassParameter] }
  deriving (Generic, Eq, Show)

type ClassParameter = ReferenceParameter

instance Binary ClassSignature

csSimpleClasses :: ClassSignature -> [String]
csSimpleClasses ClassSignature {..} = concatMap rfSimpleClasses csParams

-- | ** MethodSignature **
data MethodSignature =
  MethodSignature { msTyVarDecls :: TypeVariableDeclarations
                  , msParams     :: [MethodParameter]
                  , msReturn     :: MethodReturn
                  , msExceptions :: ThrowsExceptions }
  deriving (Generic, Eq, Show)

instance Binary MethodSignature

-- | JavaTypeSignature
type MethodParameter = Parameter

-- | Result
type MethodReturn = Maybe Parameter

-- | ThrowsSignature
type ThrowsExceptions = [ReferenceParameter]

-- |  ** FieldSignature **
data FieldSignature = FieldSignature { fsParam :: FieldParameter }
  deriving (Generic, Eq, Show)

instance Binary FieldSignature

type FieldParameter = Parameter
