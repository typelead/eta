{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Codec.JVM.Class where

import Data.Map.Strict (Map)
import Data.ByteString.Base16 (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)

import qualified Data.List as L

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const (Const, cclass)
import Codec.JVM.ConstPool (ConstPool, putConstPool, putIx)
import Codec.JVM.Field (FieldInfo, putFieldInfo)
import Codec.JVM.Internal
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types (AccessFlag, putAccessFlags, Version, IClassName, jlObject, versionMaj, versionMin)
import qualified Codec.JVM.ConstPool as CP

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
data ClassFile = ClassFile
  { constants   :: [Const]
  , version     :: Version
  , accessFlags :: Set AccessFlag
  , thisClass   :: IClassName
  , superClass  :: Maybe IClassName
  , interfaces  :: [IClassName]
  , fields      :: [FieldInfo]
  , methods     :: [MethodInfo]
  , attributes  :: Map Text Attr }
  deriving Show

magic :: ByteString
magic = fst . decode $ "CAFEBABE"

putClassFile :: ClassFile -> Put
putClassFile ClassFile {..} = do
  putByteString magic
  putI16 . versionMin $ version
  putI16 . versionMaj $ version
  putI16 . (+) 1 . CP.size $ cp
  putConstPool cp
  putAccessFlags accessFlags
  putIx cp . cclass $ thisClass
  putIx cp . cclass . fromMaybe jlObject $ superClass
  putI16 0 -- TODO Interfaces
  putFields
  putMethods
  putI16 . L.length $ attributes
  mapM_ (putAttr cp) attributes
  return () where
    cp = CP.mkConstPool constants
    putMethods = do
      putI16 . L.length $ methods
      mapM_ (putMethodInfo cp) methods
    putFields = do
      putI16 . L.length $ fields
      mapM_ (putFieldInfo cp) fields

classFileBS :: ClassFile -> ByteString
classFileBS = toStrict . runPut . putClassFile
