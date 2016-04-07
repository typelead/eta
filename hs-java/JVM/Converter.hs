{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, RecordWildCards, OverloadedStrings #-}
-- | Functions to convert from low-level .class format representation and
-- high-level Java classes, methods etc representation
module JVM.Converter
  (parseClass, parseClassFile,
   classFile2Direct, classDirect2File,
   encodeClass,
   methodByName,
   attrByName,
   methodCode
  )
  where

import Control.Monad.Exception
import Data.List
import Data.Word
import Data.Bits
import Data.Binary
import Data.Default () -- import instances only
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 ()
import qualified Data.Set as S
import qualified Data.Map as M

import JVM.ClassFile
import JVM.Common
import JVM.Exceptions

-- | Parse .class file data
parseClass :: B.ByteString -> Class Direct
parseClass bstr = classFile2Direct $ decode bstr

-- | Parse class data from file
parseClassFile :: FilePath -> IO (Class Direct)
parseClassFile path = classFile2Direct `fmap` decodeFile path

encodeClass :: (Class Direct) -> B.ByteString
encodeClass cls = encode $ classDirect2File cls

classFile2Direct :: Class File -> Class Direct
classFile2Direct (Class {..}) =
  let pool = poolFile2Direct constsPool
      superName = className $ pool ! superClass
      d = defaultClass :: Class Direct
  in d {
      constsPoolSize = fromIntegral (M.size pool),
      constsPool = pool,
      accessFlags = accessFile2Direct accessFlags,
      thisClass = className $ pool ! thisClass,
      superClass = if superClass == 0 then "" else superName,
      interfacesCount = interfacesCount,
      interfaces = map (\i -> className $ pool ! i) interfaces,
      classFieldsCount = classFieldsCount,
      classFields = map (fieldFile2Direct pool) classFields,
      classMethodsCount = classMethodsCount,
      classMethods = map (methodFile2Direct pool) classMethods,
      classAttributesCount = classAttributesCount,
      classAttributes = attributesFile2Direct pool classAttributes }

classDirect2File :: Class Direct -> Class File
classDirect2File (Class {..}) =
  let d = defaultClass :: Class File
  in d {
    constsPoolSize = fromIntegral (M.size poolInfo + 1),
    constsPool = poolInfo,
    accessFlags = accessDirect2File accessFlags,
    thisClass = force "this" $ poolClassIndex poolInfo thisClass,
    superClass = force "super" $ poolClassIndex poolInfo superClass,
    interfacesCount = fromIntegral (length interfaces),
    interfaces = map (force "ifaces" . poolClassIndex poolInfo) interfaces,
    classFieldsCount = fromIntegral (length classFields),
    classFields = map (fieldDirect2File poolInfo) classFields,
    classMethodsCount = fromIntegral (length classMethods),
    classMethods = map (methodDirect2File poolInfo) classMethods,
    classAttributesCount = fromIntegral $ arsize classAttributes,
    classAttributes = to (arlist classAttributes) }
  where
    poolInfo = poolDirect2File constsPool
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo poolInfo) pairs)

poolDirect2File :: Pool Direct -> Pool File
poolDirect2File pool = result
  where
    result = M.map cpInfo pool

    cpInfo :: Constant Direct -> Constant File
    cpInfo (CClass name) = CClass (force "class" $ poolIndex result name)
    cpInfo (CField cls name) =
      CField (force "field a" $ poolClassIndex result cls) (force "field b" $ poolNTIndex result name)
    cpInfo (CMethod cls name) =
      CMethod (force "method a" $ poolClassIndex result cls) (force ("method b: " ++ show name) $ poolNTIndex result name)
    cpInfo (CIfaceMethod cls name) =
      CIfaceMethod (force "iface method a" $ poolIndex result cls) (force "iface method b" $ poolNTIndex result name)
    cpInfo (CString s) = CString (force "string" $ poolIndex result s)
    cpInfo (CInteger x) = CInteger x
    cpInfo (CFloat x) = CFloat x
    cpInfo (CLong x) = CLong (fromIntegral x)
    cpInfo (CDouble x) = CDouble x
    cpInfo (CNameType n t) =
      CNameType (force "name" $ poolIndex result n) (force "type" $ poolIndex result t)
    cpInfo (CUTF8 s) = CUTF8 s
    cpInfo (CUnicode s) = CUnicode s

-- | Find index of given string in the list of constants
poolIndex :: (Throws NoItemInPool e) => Pool File -> B.ByteString -> EM e Word16
poolIndex list name = case mapFindIndex test list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  return $ fromIntegral i
  where
    test (CUTF8 s)    | s == name = True
    test (CUnicode s) | s == name = True
    test _                                  = False

-- | Find index of given string in the list of constants
poolClassIndex :: (Throws NoItemInPool e) => Pool File -> B.ByteString -> EM e Word16
poolClassIndex list name = case mapFindIndex checkString list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  case mapFindIndex (checkClass $ fromIntegral i) list of
                                     Nothing -> throw (NoItemInPool i)
                                     Just j  -> return $ fromIntegral j
  where
    checkString (CUTF8 s)    | s == name = True
    checkString (CUnicode s) | s == name = True
    checkString _                                  = False

    checkClass i (CClass x) | i == x = True
    checkClass _ _                           = False

poolNTIndex list x@(NameType n t) = do
    ni <- poolIndex list n
    ti <- poolIndex list (byteString t)
    case mapFindIndex (check ni ti) list of
      Nothing -> throw (NoItemInPool x)
      Just i  -> return $ fromIntegral i
  where
    check ni ti (CNameType n' t')
      | (ni == n') && (ti == t') = True
    check _ _ _                  = False

fieldDirect2File :: Pool File -> Field Direct -> Field File
fieldDirect2File pool (Field {..}) = Field {
    fieldAccessFlags = accessDirect2File fieldAccessFlags,
    fieldName = force "field name" $ poolIndex pool fieldName,
    fieldSignature = force "signature" $ poolIndex pool (encode fieldSignature),
    fieldAttributesCount = fromIntegral (arsize fieldAttributes),
    fieldAttributes = to (arlist fieldAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

methodDirect2File :: Pool File -> Method Direct -> Method File
methodDirect2File pool (Method {..}) = Method {
    methodAccessFlags = accessDirect2File methodAccessFlags,
    methodName = force "method name" $ poolIndex pool methodName,
    methodSignature = force "method sig" $ poolIndex pool (encode methodSignature),
    methodAttributesCount = fromIntegral (arsize methodAttributes),
    methodAttributes = to (arlist methodAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

attrInfo :: Pool File -> (B.ByteString, B.ByteString) -> Attribute
attrInfo pool (name, value) = Attribute {
  attributeName = force "attr name" $ poolIndex pool name,
  attributeLength = fromIntegral (B.length value),
  attributeValue = value }

poolFile2Direct :: Pool File -> Pool Direct
poolFile2Direct ps = pool
  where
    pool :: Pool Direct
    pool = M.map convert ps

    n = fromIntegral $ M.size ps

    convertNameType :: (HasSignature a) => Word16 -> NameType a
    convertNameType i =
      case pool ! i of
        CNameType n s -> NameType n (decode s)
        x -> error $ "Unexpected: " ++ show i

    convert (CClass i) = case pool ! i of
                          CUTF8 name -> CClass name
                          x -> error $ "Unexpected class name: " ++ show x ++ " at " ++ show i
    convert (CField i j) = CField (className $ pool ! i) (convertNameType j)
    convert (CMethod i j) = CMethod (className $ pool ! i) (convertNameType j)
    convert (CIfaceMethod i j) = CIfaceMethod (className $ pool ! i) (convertNameType j)
    convert (CString i) = CString $ getString $ pool ! i
    convert (CInteger x) = CInteger x
    convert (CFloat x)   = CFloat x
    convert (CLong x)    = CLong (fromIntegral x)
    convert (CDouble x)  = CDouble x
    convert (CNameType i j) = CNameType (getString $ pool ! i) (getString $ pool ! j)
    convert (CUTF8 bs) = CUTF8 bs
    convert (CUnicode bs) = CUnicode bs

accessFile2Direct :: AccessFlags File -> AccessFlags Direct
accessFile2Direct w = S.fromList $ concat $ zipWith (\i f -> if testBit w i then [f] else []) [0..] $ [
   ACC_PUBLIC,
   ACC_PRIVATE,
   ACC_PROTECTED,
   ACC_STATIC,
   ACC_FINAL,
   ACC_SYNCHRONIZED,
   ACC_VOLATILE,
   ACC_TRANSIENT,
   ACC_NATIVE,
   ACC_INTERFACE,
   ACC_ABSTRACT ]

accessDirect2File :: AccessFlags Direct -> AccessFlags File
accessDirect2File fs = bitsOr $ map toBit $ S.toList fs
  where
    bitsOr = foldl (.|.) 0
    toBit f = 1 `shiftL` (fromIntegral $ fromEnum f)

fieldFile2Direct :: Pool Direct -> Field File -> Field Direct
fieldFile2Direct pool (Field {..}) = Field {
  fieldAccessFlags = accessFile2Direct fieldAccessFlags,
  fieldName = getString $ pool ! fieldName,
  fieldSignature = decode $ getString $ pool ! fieldSignature,
  fieldAttributesCount = fromIntegral (apsize fieldAttributes),
  fieldAttributes = attributesFile2Direct pool fieldAttributes }

methodFile2Direct :: Pool Direct -> Method File -> Method Direct
methodFile2Direct pool (Method {..}) = Method {
  methodAccessFlags = accessFile2Direct methodAccessFlags,
  methodName = getString $ pool ! methodName,
  methodSignature = decode $ getString $ pool ! methodSignature,
  methodAttributesCount = fromIntegral (apsize methodAttributes),
  methodAttributes = attributesFile2Direct pool methodAttributes }

attributesFile2Direct :: Pool Direct -> Attributes File -> Attributes Direct
attributesFile2Direct pool (AP attrs) = AR (M.fromList $ map go attrs)
  where
    go :: Attribute -> (B.ByteString, B.ByteString)
    go (Attribute {..}) = (getString $ pool ! attributeName,
                           attributeValue)

-- | Try to get class method by name
methodByName :: Class Direct -> B.ByteString -> Maybe (Method Direct)
methodByName cls name =
  find (\m -> methodName m == name) (classMethods cls)

-- | Try to get object attribute by name
attrByName :: (HasAttributes a) => a Direct -> B.ByteString -> Maybe B.ByteString
attrByName x name =
  let (AR m) = attributes x
  in  M.lookup name m

-- | Try to get Code for class method (no Code for interface methods)
methodCode :: Class Direct
           -> B.ByteString       -- ^ Method name
           -> Maybe B.ByteString
methodCode cls name = do
  method <- methodByName cls name
  attrByName method "Code"

