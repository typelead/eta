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


import JVM.Common
import JVM.DataTypes
import JVM.Attributes
import JVM.Types

import Control.Monad
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.Map as M
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Binary.Put
import Data.Default
import qualified Data.ByteString.Lazy as B

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
  classFieldsCount :: Word16,              -- ^ Number of class fields
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

instance HasAttributes Class where
  attributes = classAttributes

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

lookupMethod :: B.ByteString -> Class Direct -> Maybe (Method Direct)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise           = look fs

lookupField :: B.ByteString -> Class Direct -> Maybe (Field Direct)
lookupField name cls = look (classFields cls)
  where
    look [] = Nothing
    look (f:fs)
      | fieldName f == name = Just f
      | otherwise           = look fs

-- | Constant pool
type Pool stage = M.Map Word16 (Constant stage)

poolSize :: Pool stage -> Int
poolSize = M.size

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
