{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, MultiWayIf, DataKinds, TypeOperators,
             RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Eta.Serv.ClassQuery (setClassInfoPath, getClassInfo) where

import Java
import Control.Exception
import Data.Maybe
import Data.List

import Eta.Serv.Common
import Eta.REPL.Message
import Eta.REPL.ClassInfo
import qualified Eta.REPL.Map as Map

setClassInfoPath :: [String] -> IO ()
setClassInfoPath = j_setClassInfoPath . toJStringArray

foreign import java unsafe "@static eta.serv.REPLClassLoader.setClassInfoPath"
  j_setClassInfoPath :: JStringArray -> IO ()

getClassInfo :: [String] -> IO (JResult ([String], [PreClassInfo]))
getClassInfo classes = handle (return . JException . handleExceptionAsString) $
  fmap JDone $ processClassInfos classes

foreign import java unsafe "@static eta.serv.Utils.getClassInfo"
  j_getClassInfo :: JStringArray -> IO QueryResult

data QueryResult = QueryResult @eta.serv.Utils$QueryResult

foreign import java unsafe "@field classInfos" getClassInfos :: QueryResult -> List Cls
foreign import java unsafe "@field notFounds" getNotFounds :: QueryResult -> List JString

processClassInfos :: [String] -> IO ([String], [PreClassInfo])
processClassInfos strs = goM Map.empty strs [] []
  where go m (cls:clss) res more = go m' clss (classInfo:res) (moreClss ++ more)
          where classInfo = processClassInfo cls
                m' = Map.insert (ciName classInfo) True m
                moreClss = notInMap m' (classInfoClasses classInfo)
        go m [] res more = (res, more, m)
        goM _ [] nfs cis = return (nfs, cis)
        goM m strs0 nfs cis = do
          let cs = notInMap m strs0
          qr <- j_getClassInfo (toJStringArray cs)
          let clss = fromJava (getClassInfos qr)
              notFounds = map fromJava (fromJava (getNotFounds qr) :: [JString])
              (res, more, m') = go Map.empty clss [] []
          goM m' more (notFounds ++ nfs) (res ++ cis)
        notInMap m xs = filter (isNothing . flip Map.lookup m) xs

processClassInfo :: Cls -> PreClassInfo
processClassInfo cls = ClassInfo {..}
  where ciName = getClassName cls
        ciSimpleName = getSimpleClassName cls
        ciSignature = getClassSignature cls
        ciType = getClassType cls
        (ciInnerClasses, ciOtherInnerClasses) =
          (\(a,b) -> (map getClassName a,
                      map getSimpleClassName b))
          $ partition (isPublic . getClassModifiers)
          $ fromJava (getDeclaredClasses cls)
        ciConstructors =
            map getConstructorSignature
          $ filter (isPublic . getModifiers)
          $ fromJava (getDeclaredConstructors cls)
        (ciMethods, ciOtherMethods) =
            (\(a,b) -> (map getMethodInfo a, map getName b))
          $ partition (isPublic . getModifiers)
          $ fromJava (getDeclaredMethods cls)
        (ciFields, ciOtherFields) =
          (\(a,b) -> (map getFieldInfo a, map getName b))
          $ partition (isPublic . getModifiers)
          $ fromJava (getDeclaredFields cls)
        ciParentClass = fmap getClassName (getEnclosingClass cls)
        ciFinal = isFinal (getClassModifiers cls)

getClassSignature :: Cls -> ClassSignature
getClassSignature cls = ClassSignature {..}
  where csTyVarDecls = map (\tv -> mkTyVarDecl (tvName tv)
                            (map getReferenceParameter $ fromJava (tvBounds tv)))
                     $ fromJava (getTypeParams cls)
        csParams = maybeToList (fmap getReferenceParameter (getGenericSuperclass cls))
                ++ map getReferenceParameter (fromJava (getGenericInterfaces cls))

getConstructorSignature :: Constructor -> MethodSignature
getConstructorSignature c = MethodSignature {..}
  where msTyVarDecls = map (\tv -> mkTyVarDecl (tvName tv)
                            (map getReferenceParameter $ fromJava (tvBounds tv)))
                     $ fromJava (getTypeParams c)
        msParams = map getParameter (fromJava (getConstructorGenParamTypes c))
        msReturn = Nothing
        msExceptions = map getReferenceParameter (fromJava (getConstructorExceptionTypes c))

getMethodInfo :: Method -> MethodInfo
getMethodInfo m = MethodInfo {..}
  where miName      = getName m
        miSignature = getMethodSignature m
        miAbstract  = isAbstract modifier
        miFinal     = isFinal modifier
        miStatic    = isStatic modifier
        modifier    = getModifiers m

getFieldInfo :: Field -> FieldInfo
getFieldInfo f = FieldInfo {..}
  where fiName      = getName f
        fiSignature = getFieldSignature f
        fiFinal     = isFinal (getModifiers f)

getMethodSignature :: Method -> MethodSignature
getMethodSignature m = MethodSignature {..}
  where msTyVarDecls = map (\tv -> mkTyVarDecl (tvName tv)
                            (map getReferenceParameter $ fromJava (tvBounds tv)))
                     $ fromJava (getTypeParams m)
        msParams = map getParameter (fromJava (getMethodGenParamTypes m))
        msReturn
          | equals ret voidClass = Nothing
          | otherwise = Just (getParameter ret)
          where ret = getMethodReturnType m
        msExceptions = map getReferenceParameter (fromJava (getMethodExceptionTypes m))

getFieldSignature :: Field -> FieldSignature
getFieldSignature f = FieldSignature { fsParam = getParameter (getFieldGenericType f) }

getClassType :: Cls -> ClassType
getClassType cls
  | isAnnotation cls = AnnotationType
  | isEnum cls       = EnumType
  | isInterface cls  = InterfaceType
  | isAbstract (getClassModifiers cls) = AbstractClassType
  | otherwise        = ClassType

-- Translating to ClassInfo

getReferenceParameter :: Type -> ReferenceParameter
getReferenceParameter t
  | t `instanceOf` getClass (Proxy :: Proxy Cls)
  = mkGenericRefParam (getClassName (unsafeCast t)) []
  | t `instanceOf` getClass (Proxy :: Proxy ParameterizedType)
  , let t' = unsafeCast t
  = mkGenericRefParam (rpObjectType (getReferenceParameter (ptRawType t')))
                      (map getTypeParameter (fromJava (ptTypeArgs t')))
  | t `instanceOf` getClass (Proxy :: Proxy TypeVariable)
  = mkVarRefParam (tvName (unsafeCast t))
  | t `instanceOf` getClass (Proxy :: Proxy GenericArrayType)
  = mkArrayRefParam (getParameter (gatComponentType (unsafeCast t)))
  | otherwise
  = error $ "getReferenceParameter: unexpected type: " ++ show t

getTypeParameter :: Type -> TypeParameter
getTypeParameter t
  | t `instanceOf` getClass (Proxy :: Proxy WildcardType) =
    mkWildcardTyParam (getWildcardBound (unsafeCast t))
  | otherwise = mkRefTyParam (getReferenceParameter t)

getWildcardBound :: WildcardType -> Bound
getWildcardBound wt = mkWildcardBound lowerBounds upperBounds
  where lowerBounds = map getReferenceParameter $ fromJava (wtLowerBounds wt)
        upperBounds = map getReferenceParameter $ fromJava (wtUpperBounds wt)

getParameter :: Type -> Parameter
getParameter t
  | t `instanceOf` getClass (Proxy :: Proxy Cls)
  , let cls = unsafeCast t :: Cls
  = if | equals cls byteClass   -> mkPrimParam ByteType
       | equals cls charClass   -> mkPrimParam CharType
       | equals cls doubleClass -> mkPrimParam DoubleType
       | equals cls floatClass  -> mkPrimParam FloatType
       | equals cls intClass    -> mkPrimParam IntType
       | equals cls longClass   -> mkPrimParam LongType
       | equals cls shortClass  -> mkPrimParam ShortType
       | equals cls boolClass   -> mkPrimParam BoolType
       | otherwise              -> refParam
  | otherwise = refParam
  where refParam = mkRefParam (getReferenceParameter t)

-- Reflection API Bindings

data Cls = Cls @java.lang.Class
  deriving (Class, Eq)

type instance Inherits Cls = '[Object, Type, GenericDeclaration]

data ClsArray = ClsArray @java.lang.Class[]
  deriving Class

instance JArray Cls ClsArray

foreign import java unsafe "@static @field java.lang.Void.TYPE" voidClass :: Cls
foreign import java unsafe "@static @field java.lang.Byte.TYPE" byteClass :: Cls
foreign import java unsafe "@static @field java.lang.Character.TYPE" charClass :: Cls
foreign import java unsafe "@static @field java.lang.Double.TYPE" doubleClass :: Cls
foreign import java unsafe "@static @field java.lang.Float.TYPE" floatClass :: Cls
foreign import java unsafe "@static @field java.lang.Integer.TYPE" intClass :: Cls
foreign import java unsafe "@static @field java.lang.Long.TYPE" longClass :: Cls
foreign import java unsafe "@static @field java.lang.Short.TYPE" shortClass :: Cls
foreign import java unsafe "@static @field java.lang.Boolean.TYPE" boolClass :: Cls

foreign import java unsafe "getName" getClassName :: Cls -> String
foreign import java unsafe "getSimpleName" getSimpleClassName :: Cls -> String
foreign import java unsafe getGenericSuperclass :: Cls -> Maybe Type
foreign import java unsafe getGenericInterfaces :: Cls -> TypeArray
foreign import java unsafe getEnclosingClass :: Cls -> Maybe Cls
foreign import java unsafe "getModifiers" getClassModifiers :: Cls -> Modifier
foreign import java unsafe isAnnotation :: Cls -> Bool
foreign import java unsafe isInterface :: Cls -> Bool
foreign import java unsafe isEnum :: Cls -> Bool
foreign import java unsafe getDeclaredClasses :: Cls -> ClsArray
foreign import java unsafe getDeclaredConstructors :: Cls -> ConstructorArray
foreign import java unsafe getDeclaredMethods :: Cls -> MethodArray
foreign import java unsafe getDeclaredFields :: Cls -> FieldArray

newtype Modifier = Modifier Int

foreign import java unsafe "@static java.lang.reflect.Modifier.isAbstract"
  isAbstract :: Modifier -> Bool

foreign import java unsafe "@static java.lang.reflect.Modifier.isFinal"
  isFinal :: Modifier -> Bool

foreign import java unsafe "@static java.lang.reflect.Modifier.isPublic"
  isPublic :: Modifier -> Bool

foreign import java unsafe "@static java.lang.reflect.Modifier.isStatic"
  isStatic :: Modifier -> Bool

data Type = Type @java.lang.reflect.Type
  deriving (Class, Show)

data TypeArray = TypeArray @java.lang.reflect.Type[]
  deriving (Class, Show)

instance JArray Type TypeArray

data GenericArrayType = GenericArrayType @java.lang.reflect.GenericArrayType
  deriving Class

type instance Inherits GenericArrayType = '[Type]

foreign import java unsafe "@interface getGenericComponentType"
  gatComponentType :: GenericArrayType -> Type

data ParameterizedType = ParameterizedType @java.lang.reflect.ParameterizedType
  deriving Class

type instance Inherits ParameterizedType = '[Type]

foreign import java unsafe "@interface getRawType"
  ptRawType :: ParameterizedType -> Type

foreign import java unsafe "@interface getActualTypeArguments"
  ptTypeArgs :: ParameterizedType -> TypeArray

data TypeVariable = TypeVariable @java.lang.reflect.TypeVariable
  deriving Class

type instance Inherits TypeVariable = '[Type]

foreign import java unsafe "@interface getName"   tvName   :: TypeVariable -> String
foreign import java unsafe "@interface getBounds" tvBounds :: TypeVariable -> TypeArray

data TypeVariableArray = TypeVariableArray @java.lang.reflect.TypeVariable[]
  deriving Class

instance JArray TypeVariable TypeVariableArray

data WildcardType = WildcardType @java.lang.reflect.WildcardType
  deriving Class

type instance Inherits WildcardType = '[Type]

foreign import java unsafe "@interface getLowerBounds"
  wtLowerBounds :: WildcardType -> TypeArray

foreign import java unsafe "@interface getUpperBounds"
  wtUpperBounds :: WildcardType -> TypeArray

data Member = Member @java.lang.reflect.Member
  deriving Class

foreign import java unsafe "@interface getName"
  getName :: (a <: Member) => a -> String

foreign import java unsafe "@interface getModifiers"
  getModifiers :: (a <: Member) => a -> Modifier

data GenericDeclaration = GenericDeclaration @java.lang.reflect.GenericDeclaration
  deriving Class

foreign import java unsafe "@interface getTypeParameters"
  getTypeParams :: (a <: GenericDeclaration) => a -> TypeVariableArray

data Constructor = Constructor @java.lang.reflect.Constructor
  deriving Class

type instance Inherits Constructor = '[Member, GenericDeclaration]

foreign import java unsafe "getGenericParameterTypes"
  getConstructorGenParamTypes :: Constructor -> TypeArray

foreign import java unsafe "getGenericExceptionTypes"
  getConstructorExceptionTypes :: Constructor -> TypeArray

data ConstructorArray = ConstructorArray @java.lang.reflect.Constructor[]
  deriving Class

instance JArray Constructor ConstructorArray

data Method = Method @java.lang.reflect.Method
  deriving Class

foreign import java unsafe "getGenericParameterTypes"
  getMethodGenParamTypes :: Method -> TypeArray

foreign import java unsafe "getGenericReturnType"
  getMethodReturnType :: Method -> Type

foreign import java unsafe "getGenericExceptionTypes"
  getMethodExceptionTypes :: Method -> TypeArray

type instance Inherits Method = '[Member, GenericDeclaration]

data MethodArray = MethodArray @java.lang.reflect.Method[]
  deriving Class

instance JArray Method MethodArray

data Field = Field @java.lang.reflect.Field
  deriving Class

type instance Inherits Field = '[Member]

foreign import java unsafe "getGenericType"
  getFieldGenericType :: Field -> Type

data FieldArray = FieldArray @java.lang.reflect.Field[]
  deriving Class

instance JArray Field FieldArray
