{-# LANGUAGE ScopedTypeVariables #-}
module Eta.CodeGen.Utils where

import Eta.Main.DynFlags
import Eta.BasicTypes.Name
import Eta.Types.TyCon
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.DataCon (DataCon)
import Eta.BasicTypes.Id
import Eta.BasicTypes.Literal
import Eta.StgSyn.StgSyn
import Codec.JVM
import Codec.JVM.Encoding
import Data.Char (ord)
import Control.Arrow(first)
import Control.Monad(guard)
import Eta.CodeGen.Name
import Eta.CodeGen.Rts
import Eta.Debug
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeLatin1)
import Data.Int
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Foldable
import Control.Exception
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BL

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
cgLit (MachStr s)           = (jlong, genCode)
  where (string, isLatin1) =
          unsafeDupablePerformIO $
            catch (fmap (,False) $ evaluate $ decodeUtf8 s)
                  (\(_ :: SomeException) -> fmap (,True) $ evaluate $ decodeLatin1 s)

        strings :: [Text]
        strings
          | byteLen > 65535
          = splitIntoChunks byteLen string
          | otherwise = [string]
          where byteLen = BL.length (encodeModifiedUtf8 string)

        splitIntoChunks :: Int64 -> Text -> [Text]
        splitIntoChunks byteLen string = go string
          where charLen       = T.length string
                -- Conservative estimate
                bytesPerChar  = ceiling ((fromIntegral byteLen :: Double) / fromIntegral charLen) :: Int
                chunkSize     = 65535 `div` bytesPerChar
                go string
                  | T.null string = []
                  | otherwise     = chunk : go rest
                  where (chunk, rest) = T.splitAt chunkSize string

        genCode
          | numStrings > 1 =
               iconst jint (fromIntegral numStrings)
            <> new (jarray jstring)
            <> fold (map (\(i, str) -> dup (jarray jstring)
                                    <> iconst jint i
                                    <> sconst str
                                    <> gastore jstring) (zip [0..] strings))
            <> loadString True
          | otherwise      = sconst string <> loadString False
          where numStrings = length strings
        loadString arrayForm
          | isLatin1  = loadStringLatin1 arrayForm
          | otherwise = loadStringUTF8 arrayForm

-- TODO: Implement MachLabel
cgLit MachLabel {}          = error "cgLit: MachLabel"
cgLit other                 = pprPanic "mkSimpleLit" (ppr other)

simpleStringLiteral :: GenStgArg occ -> Maybe Text
simpleStringLiteral (StgLitArg (MachStr bs)) = do
  string <- unsafeDupablePerformIO $
     catch (fmap Just $ evaluate $ decodeUtf8 bs)
           (\(_ :: SomeException) -> return Nothing)
  guard (BL.length (encodeModifiedUtf8 string) < 65535)
  return string
simpleStringLiteral _ = Nothing

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
initCodeTemplate' retFt sync modClass qClName field code =
  mkMethodDef modClass accessFlags qClName [] (ret retFt) $ fold
    [ getstatic field
    , ifnonnull mempty bodyCode
    , getstatic field
    , greturn retFt ]
  where accessFlags = [Public, Static]
        modFt = obj modClass
        bodyCode
          | sync = synchronized 1 0 classFt (ftClassObject modFt) $
                     getstatic field <> ifnonnull mempty code
          | otherwise = code

initCodeTemplate :: Bool -> Text -> Text -> FieldRef -> Code -> MethodDef
initCodeTemplate synchronized modClass qClName field code =
  initCodeTemplate' closureType synchronized modClass qClName field code

idUsedOnce :: Id -> Bool
idUsedOnce id
  | OneOcc False True _ctxt <- idOccInfo id = True
  | otherwise = False
