{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Class where

import Data.ByteString.Base16 (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Binary.Put (Put, runPut, putByteString, putWord16be)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Word (Word16)

import qualified Data.List as L
import qualified Data.Set as S

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const (Const(CClass))
import Codec.JVM.ConstPool (ConstPool, putConstPool, putIx)
import Codec.JVM.Field (FieldInfo, putFieldInfo)
import Codec.JVM.Internal (putI16)
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types (AccessFlag, putAccessFlags, Version, IClassName, jlObject, versionMaj, versionMin)
import qualified Codec.JVM.ConstPool as CP

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
data ClassFile = ClassFile
  { constPool :: ConstPool
  , version :: Version
  , accessFlags :: Set AccessFlag
  , thisClass :: IClassName
  , superClass :: Maybe IClassName
  , interfaces :: [IClassName]
  , fields :: [FieldInfo]
  , methods :: [MethodInfo]
  , attributes :: [Attr] }
  deriving Show

magic :: ByteString
magic = fst . decode $ "CAFEBABE"

putClassFile :: ClassFile -> Put
putClassFile cf = do
  putByteString magic
  putI16 . versionMin . version $ cf
  putI16 . versionMaj . version $ cf
  putI16 . (+) 1 . CP.size  . constPool $ cf
  putConstPool cp
  putAccessFlags . accessFlags $ cf
  putIx cp $ CClass $ thisClass cf
  putIx cp $ CClass $ fromMaybe jlObject $ superClass cf
  putI16 0 -- TODO Interfaces
  putFields
  putMethods
  putI16 . L.length $ attributes cf
  mapM_ (putAttr cp) $ attributes cf
  return () where
    cp = constPool cf
    putMethods = do
      putI16 . L.length $ methods cf
      mapM_ (putMethodInfo cp) $ methods cf
    putFields = do
      putI16 . L.length $ fields cf
      mapM_ (putFieldInfo cp) $ fields cf

classFileBS :: ClassFile -> ByteString
classFileBS = toStrict . runPut . putClassFile
