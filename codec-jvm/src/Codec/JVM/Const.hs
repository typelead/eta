module Codec.JVM.Const where

import Data.Text (Text)
import Data.Word (Word8)

import Codec.JVM.Types (IClassName, FieldRef, FieldType(..), MethodRef, PrimType(..), NameAndDesc, jlString)

constTag :: Const -> Word8
constTag (CUTF8 _)              = 1
constTag (CValue (CInteger _))  = 3
constTag (CClass _)             = 7
constTag (CValue (CString _))   = 8
constTag (CFieldRef _)          = 9
constTag (CMethodRef _)         = 10
constTag (CNameAndType _)       = 12

-- | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data Const
  = CUTF8 Text
  | CValue ConstVal
  | CClass IClassName
  | CFieldRef FieldRef
  | CMethodRef MethodRef
  | CNameAndType NameAndDesc
  deriving (Eq, Ord, Show)

-- | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.2-300-C.1
data ConstVal
  = CInteger Int
  | CString Text
  deriving (Eq, Ord)

instance Show ConstVal where
  show (CInteger  x) = show x -- concat ["Int ", show x]
  show (CString   x) = show x -- concat ["String ", show x]

constValType :: ConstVal -> FieldType
constValType (CInteger _) = BaseType JInt
constValType (CString _)  = ObjectType jlString
