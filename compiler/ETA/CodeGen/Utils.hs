module ETA.CodeGen.Utils where

import ETA.Main.DynFlags
import ETA.BasicTypes.Name
import ETA.Types.TyCon
import ETA.BasicTypes.Literal
import Codec.JVM
import Data.Char (ord)
import Control.Arrow(first)
import ETA.CodeGen.Name
import ETA.CodeGen.Rts
import ETA.Debug
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid
import Data.Foldable

cgLit :: Literal -> (FieldType, Code)
cgLit (MachChar c)          = (jint, iconst jint . fromIntegral $ ord c)
cgLit (MachInt i)           = (jint, iconst jint $ fromIntegral i)
cgLit (MachWord i)          = (jint, iconst jint $ fromIntegral i)
cgLit (MachInt64 i)         = (jlong, lconst $ fromIntegral i)
-- TODO: Verify that fromIntegral converts well
cgLit (MachWord64 i)        = (jlong, lconst $ fromIntegral i)
cgLit (MachFloat r)         = (jfloat, fconst $ fromRational r)
cgLit (MachDouble r)        = (jdouble, dconst $ fromRational r)
-- TODO: Remove this literal variant?
cgLit MachNullAddr          = (jobject, lconst 0)
cgLit MachNull              = (jobject, aconst_null jobject)
cgLit (MachStr s)           = (jstring, sconst $ decodeUtf8 s)
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
  | ft `notElem` [jint, jbool, jbyte, jshort, jchar] = error $ "litSwitch[" ++ show ft ++ "]: " ++
                 "primitive cases not supported for non-integer values"
  | otherwise  = intSwitch expr intBranches (Just deflt)
  where intBranches = map (first litToInt) branches

tagToClosure :: DynFlags -> TyCon -> Code -> (FieldType, Code)
tagToClosure dflags tyCon loadArg = (closureType, enumCode)
  where enumCode =  invokestatic (mkMethodRef modClass fieldName [] (Just arrayFt))
                 <> loadArg
                 <> gaload closureType
        tyName = tyConName tyCon
        modClass = moduleJavaClass $ nameModule tyName
        fieldName = nameTypeTable dflags $ tyConName tyCon
        arrayFt = jarray closureType

initCodeTemplate' :: FieldType -> Bool -> Text -> Text -> FieldRef -> Code -> MethodDef
initCodeTemplate' retFt synchronized modClass qClName field code =
  mkMethodDef modClass accessFlags qClName [] (Just retFt) $ fold
    [ getstatic field
    , ifnonnull mempty code
    , getstatic field
    , greturn retFt ]
  where accessFlags = [Public, Static] ++ (if synchronized then [Synchronized] else [])

initCodeTemplate :: Bool -> Text -> Text -> FieldRef -> Code -> MethodDef
initCodeTemplate synchronized modClass qClName field code =
  initCodeTemplate' closureType synchronized modClass qClName field code
