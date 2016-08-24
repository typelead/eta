module GHCVM.CodeGen.Utils where

import GHCVM.Main.DynFlags
import GHCVM.Types.Type
import GHCVM.BasicTypes.Name
import GHCVM.Types.TyCon
import GHCVM.BasicTypes.Literal
import Codec.JVM
import Data.Char (ord)
import Control.Arrow(first)
import GHCVM.CodeGen.Name
import GHCVM.Debug
import Data.Text (Text)
import Data.Monoid ((<>))

cgLit :: Literal -> (FieldType, Code)
cgLit (MachChar c)          = (jint, iconst jint . fromIntegral $ ord c)
cgLit (MachInt i)           = (jint, iconst jint $ fromIntegral i)
cgLit (MachWord i)          = (jint, iconst jint $ fromIntegral i)
cgLit (MachInt64 i)         = (jlong, iconst jlong $ fromIntegral i)
cgLit (MachWord64 i)        = (jlong, iconst jlong $ fromIntegral i)
cgLit (MachFloat r)         = (jfloat, fconst $ fromRational r)
cgLit (MachDouble r)        = (jdouble, dconst $ fromRational r)
cgLit MachNullAddr          = (jobject, aconst_null)
cgLit (MachStr s)           = (jstring, sconst s)
-- TODO: Implement MachLabel
cgLit MachLabel {}          = error "cgLit: MachLabel"
cgLit other                 = pprPanic "mkSimpleLit" (ppr other)

litToInt :: Literal -> Int
litToInt (MachInt i)  = fromInteger i
litToInt (MachWord i) = fromInteger i
litToInt (MachChar c) = ord c
litToInt _            = error "litToInt: not integer"

intSwitch :: Code -> [(Int, Code)] -> Maybe Code -> Code
intSwitch = gswitch

litSwitch :: FieldType -> Code -> [(Literal, Code)] -> Code -> Code
litSwitch ft expr branches deflt
  -- | isObjectFt ft = deflt -- ASSERT (length branches == 0)
  -- TODO: When switching on an object, perform a checkcast
  -- TODO: When switching on long/float/double, use an if-else tree
  | null branches = deflt
  | ft /= jint = error $ "litSwitch[" ++ show ft ++ "]: " ++
                         "primitive cases not supported for non-integer values"
  | otherwise  = intSwitch expr intBranches (Just deflt)
  where intBranches = map (first litToInt) branches

tagToClosure :: DynFlags -> TyCon -> Code -> (FieldType, Code)
tagToClosure dflags tyCon loadArg = (elemFt, enumCode)
  where enumCode =  getstatic (mkFieldRef modClass fieldName arrayFt)
                 <> loadArg
                 <> gaload elemFt
        tyName = tyConName tyCon
        modClass = moduleJavaClass $ nameModule tyName
        fieldName = nameTypeTable dflags $ tyConName tyCon
        tyConCl = tyConClass dflags tyCon
        elemFt = obj tyConCl
        arrayFt = jarray elemFt
