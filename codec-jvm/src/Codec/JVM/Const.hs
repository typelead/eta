module Codec.JVM.Const
  (Const(..),
   ConstVal(..),
   constTag,
   constValType,
   cclass,
   cstring,
   cint,
   clong,
   cfloat,
   cdouble)
where

import Data.Text (Text)
import Data.Word (Word8)
import Data.Int (Int32,Int64)

import Codec.JVM.Types (IClassName, FieldRef, FieldType(..), MethodRef, PrimType(..), NameAndDesc, jlString)

constTag :: Const -> Word8
constTag (CUTF8 _)              = 1
constTag (CValue (CInteger _))  = 3
constTag (CValue (CFloat _))    = 4
constTag (CValue (CLong _))     = 5
constTag (CValue (CDouble _))   = 6
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
  = CString Text
  | CInteger Int32
  | CLong Int64
  | CFloat Float
  | CDouble Double
  deriving (Eq, Ord, Show)

-- instance Show ConstVal where
--   show (CInteger  x) = show x -- concat ["Int ", show x]
--   show (CString   x) = show x -- concat ["String ", show x]
--   show (CLong x) show x
--   show (CFloat x) = show x

constValType :: ConstVal -> FieldType
constValType (CString _)  = ObjectType jlString
constValType (CInteger _) = BaseType JInt
constValType (CLong _)  = BaseType JLong
constValType (CFloat _)  = BaseType JFloat
constValType (CDouble _)  = BaseType JDouble

cclass :: IClassName -> Const
cclass = CClass

cstring :: Text -> Const
cstring = CValue . CString

clong :: Int64 -> Const
clong = CValue . CLong

cint :: Int32 -> Const
cint = CValue . CInteger

cfloat :: Float -> Const
cfloat = CValue . CFloat

cdouble :: Double -> Const
cdouble = CValue . CDouble

