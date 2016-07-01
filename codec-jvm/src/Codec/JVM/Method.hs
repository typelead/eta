module Codec.JVM.Method where

import Data.Binary.Put (Put)
import Data.Set (Set)

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

putMethodInfo :: ConstPool -> MethodInfo -> Put
putMethodInfo cp mi = do
  putAccessFlags $ accessFlags mi
  case name mi of UName n       -> putIx cp $ CUTF8 n
  case descriptor mi of Desc d  -> putIx cp $ CUTF8 d
  putI16 . L.length $ attributes mi
  mapM_ (putAttr cp) $ attributes mi
