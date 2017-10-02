{-# LANGUAGE ScopedTypeVariables #-}
module ETA.CodeGen.Utils where

import ETA.Main.DynFlags
import ETA.BasicTypes.Name
import ETA.Types.TyCon
import ETA.BasicTypes.DataCon (DataCon)
import ETA.BasicTypes.Literal
import Codec.JVM
import Data.Char (ord)
import Control.Arrow(first)
import ETA.CodeGen.Name
import ETA.CodeGen.Rts
import ETA.Debug
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, decodeLatin1)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Foldable
import Control.Exception
import System.IO.Unsafe

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
cgLit MachNullAddr          = (jlong, lconst 0)
cgLit MachNull              = (jobject, aconst_null jobject)
cgLit (MachStr s)           = (jlong, sconst string <> loadString )
  where (string, isLatin1) =
          unsafeDupablePerformIO $
            catch (fmap (,False) $ evaluate $ decodeUtf8 s)
                  (\(e :: ErrorCall) -> fmap (,True) $ evaluate $ decodeLatin1 s)
        loadString
          | isLatin1  = loadStringLatin1
          | otherwise = loadStringUTF8

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

instanceofTree :: DynFlags -> Code -> [(DataCon, Code)] -> Maybe Code -> Code
instanceofTree _ _  []          (Just deflt) = deflt
instanceofTree _ _  [(_, code)] Nothing      = code
instanceofTree dflags x branches maybeDefault =
  foldr f def branches
  where def = fromMaybe mempty maybeDefault
        f (con, branchCode) code =
          x <> ginstanceof (obj (dataConClass dflags con)) <> ifeq code branchCode

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
    , ifnonnull mempty bodyCode
    , getstatic field
    , greturn retFt ]
  where accessFlags = [Public, Static]
        modFt = obj modClass
        bodyCode
          | synchronized = ftClassObject modFt
                        <> dup classFt
                        <> monitorenter classFt
                        <> getstatic field
                        <> ifnonnull mempty code
                        <> monitorexit classFt
          | otherwise = code

initCodeTemplate :: Bool -> Text -> Text -> FieldRef -> Code -> MethodDef
initCodeTemplate synchronized modClass qClName field code =
  initCodeTemplate' closureType synchronized modClass qClName field code
