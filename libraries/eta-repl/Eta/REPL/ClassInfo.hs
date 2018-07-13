{-# LANGUAGE DeriveGeneric, RecordWildCards, StandaloneDeriving, FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Eta.REPL.ClassInfo where

import GHC.Generics
import Data.Binary
import Data.Maybe

data ClassInfo f =
  ClassInfo { ciName              :: String
            , ciSimpleName        :: String
            , ciSignature         :: ClassSignature
            , ciType              :: ClassType
            , ciInnerClasses      :: [String]
            , ciOtherInnerClasses :: [String]
            , ciConstructors      :: [MethodSignature]
            , ciMethods           :: f MethodInfo
            , ciOtherMethods      :: [String]
            , ciFields            :: f FieldInfo
            , ciOtherFields       :: [String]
            , ciParentClass       :: Maybe String
            , ciFinal             :: Bool }

type PreClassInfo = ClassInfo []

deriving instance Eq PreClassInfo
deriving instance Show PreClassInfo
deriving instance (f ~ []) => Generic (ClassInfo f)

instance Binary PreClassInfo

classInfoClasses :: ClassInfo f -> [String]
classInfoClasses ClassInfo {..} =
  csSimpleClasses ciSignature ++ maybeToList ciParentClass ++ ciInnerClasses

data MethodInfo =
  MethodInfo { miName      :: String
             , miSignature :: MethodSignature
             , miAbstract  :: Bool
             , miFinal     :: Bool
             , miStatic    :: Bool }
  deriving (Generic, Eq, Show)

instance Binary MethodInfo

data FieldInfo =
  FieldInfo { fiName      :: String
            , fiSignature :: FieldSignature
            , fiFinal     :: Bool }
  deriving (Generic, Eq, Show)

instance Binary FieldInfo

data ClassType = ClassType | AbstractClassType | InterfaceType | AnnotationType | EnumType
  deriving (Generic, Eq, Show)

instance Binary ClassType

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

tyVarDeclsTyVars :: TypeVariableDeclarations -> [String]
tyVarDeclsTyVars = map tvdName

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
