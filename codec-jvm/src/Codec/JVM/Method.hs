module Codec.JVM.Method where

import Data.Binary.Put (Put, putWord16be)
import Data.Set (Set)
import Data.Word (Word16)

import qualified Data.List as L
import qualified Data.Set as S

import Codec.JVM.Attr (Attr, putAttr, unpackAttr)
import Codec.JVM.Const (Const(CUTF8))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16)
import Codec.JVM.Types


-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6

data MethodInfo = MethodInfo
  { accessFlags :: Set AccessFlag
  , name :: UName
  , descriptor :: Desc
  , attributes :: [Attr] }
  deriving Show

unpackMethodInfo :: MethodInfo -> [Const]
unpackMethodInfo mi = unpackAttr =<< attributes mi

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6-200-A.1
data AccessFlag
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Synchronized
  | Bridge
  | VarArgs
  | Native
  | Abstract
  | Strict
  | Synthetic
  deriving (Eq, Ord, Show)

accessFlagValue :: AccessFlag -> Word16
accessFlagValue Public        = 0x0001
accessFlagValue Private       = 0x0002
accessFlagValue Protected     = 0x0004
accessFlagValue Static        = 0x0008
accessFlagValue Final         = 0x0010
accessFlagValue Synchronized  = 0x0020
accessFlagValue Bridge        = 0x0040
accessFlagValue VarArgs       = 0x0080
accessFlagValue Native        = 0x0100
accessFlagValue Abstract      = 0x0400
accessFlagValue Strict        = 0x0800
accessFlagValue Synthetic     = 0x1000

putMethodInfo :: ConstPool -> MethodInfo -> Put
putMethodInfo cp mi = do
  putWord16be $ foldr (+) 0 (accessFlagValue <$> (S.toList $ accessFlags mi))
  case name mi of UName n       -> putIx cp $ CUTF8 n
  case descriptor mi of Desc d  -> putIx cp $ CUTF8 d
  putI16 . L.length $ attributes mi
  mapM_ (putAttr cp) $ attributes mi
