{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Class where

import Data.ByteString.Base16 (decode)
import Data.ByteString (ByteString)
import Data.Binary.Put (Put, putByteString, putWord16be)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Word (Word16)

import qualified Data.List as L
import qualified Data.Set as S

import Codec.JVM.Attr (Attr)
import Codec.JVM.Const (Const(CClass))
import Codec.JVM.ConstPool (ConstPool, putConstPool, putIx)
import Codec.JVM.Field (FieldInfo)
import Codec.JVM.Internal (putI16)
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types (Version, IClassName, jlObject, versionMaj, versionMin)
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

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1-200-E.1
data AccessFlag
  = Public
  | Final
  | Super
  | Interface
  | Abstract
  | Synthetic
  | Annotation
  | Enum
  deriving (Eq, Ord, Show)

accessFlagValue :: AccessFlag -> Word16
accessFlagValue Public      = 0x0001
accessFlagValue Final       = 0x0010
accessFlagValue Super       = 0x0020
accessFlagValue Interface   = 0x0200
accessFlagValue Abstract    = 0x0400
accessFlagValue Synthetic   = 0x1000
accessFlagValue Annotation  = 0x2000
accessFlagValue Enum        = 0x4000

magic :: ByteString
magic = fst . decode $ "CAFEBABE"

putClassFile :: ClassFile -> Put
putClassFile cf = do
  putByteString magic
  putI16 . versionMin . version $ cf
  putI16 . versionMaj . version $ cf
  putI16 . (+) 1 . CP.size  . constPool $ cf
  putConstPool cp
  putWord16be $ foldr (+) 0 (accessFlagValue <$> (S.toList $ accessFlags cf))
  putIx cp $ CClass $ thisClass cf
  putIx cp $ CClass $ fromMaybe jlObject $ superClass cf
  putI16 0 -- TODO Interfaces
  putI16 0 -- TODO Fields
  putMethods
  putI16 0 -- TODO Attributes
  return () where
    cp = constPool cf
    putMethods = do
      putI16 . L.length $ methods cf
      mapM_ (putMethodInfo cp) $ methods cf

