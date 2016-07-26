module GHCVM.CodeGen.Name (
  qualifiedName,
  fastStringToText,
  nameText,
  nameTypeText,
  nameTypeTable,
  nameDataText,
  idNameText,
  idClassText,
  moduleJavaClass,
  dataConClass,
  tyConClass,
  moduleClass,
  closure,
  classFilePath,
  labelToMethod
  ) where

import DynFlags
import TyCon
import DataCon
import Module
import FastString
import Name
import Id

import Data.Char as C
import Data.Maybe
import Data.Text as T hiding (map, init, last, null)
import Data.Text.Encoding

import GHCVM.Debug
import Encoding

import Codec.JVM

qualifiedName :: Text -> Text -> Text
qualifiedName modClass className = append modClass . cons '$' $ className

closure :: Text -> Text
closure = flip append "_closure"

fastStringToText :: FastString -> Text
fastStringToText = decodeUtf8 . fastStringToByteString

nameTypeText :: DynFlags -> Name -> Text
nameTypeText dflags = flip snoc 'T' . nameText dflags

nameTypeTable :: DynFlags -> Name -> Text
nameTypeTable dflags = flip append "_table" . nameText dflags

nameDataText :: DynFlags -> Name -> Text
nameDataText dflags = flip snoc 'D' . nameText dflags

nameText :: DynFlags -> Name -> Text
nameText dflags = T.pack
                . zEncodeString
                . showPpr dflags

idNameText :: DynFlags -> Id -> Text
idNameText dflags = nameText dflags . idName

idClassText :: Id -> Text
idClassText id =
  case nameModule_maybe (idName id) of
    Just mod -> modNameText mod
    Nothing -> error $ "idClassText: "

zEncodeText :: FastString -> Text
zEncodeText fs = decodeUtf8
               . fastZStringToByteString
               . zEncodeFS
               $ fs

modNameText :: Module -> Text
modNameText mod = decodeUtf8
                . fastStringToByteString
                . moduleNameFS
                . moduleName
                $ mod

packageKeyText :: Module -> Text
packageKeyText mod = zEncodeText
                   . packageKeyFS
                   . modulePackageKey
                   $ mod


-- Codec.JVM.ASM -> "codec/jvm/ASM"
moduleJavaClass :: Module -> Text
moduleJavaClass mod = qClassName
  where
    mods = split (== '.') $ modNameText mod
    (parentMods, className') = (init mods, last mods)
    packageString = T.toLower . packageKeyText $ mod
    package = if null parentMods
                 then packageString
                 else   append packageString
                      . cons '/'
                      . T.toLower
                      . intercalate "/"
                      $ parentMods
    className = upperFirst className'
    qClassName = append package . cons '/' $ className

classFilePath :: ClassFile -> FilePath
classFilePath ClassFile {..} =
  case thisClass of
    IClassName name -> unpack . append name $ ".class"

upperFirst :: Text -> Text
upperFirst str = case uncons str of
  Nothing -> str
  Just (c, str') -> cons (C.toUpper c) str'

modClosure :: DynFlags -> Module -> Name -> (Text, Text)
modClosure dflags mod name = (moduleJavaClass mod, nameText dflags name)

moduleClass :: Name -> Text -> Text
moduleClass name = qualifiedName moduleClass
        -- TODO: Most likely this will fail for same module data cons
        -- Maybe externalize the data con name?
  where moduleClass = moduleJavaClass
                    . fromMaybe (error "Failed")
                    $ nameModule_maybe name

dataConClass :: DynFlags -> DataCon -> Text
dataConClass dflags dataCon = moduleClass dataName dataClass
  where dataName = dataConName dataCon
        dataClass = nameDataText dflags dataName

tyConClass :: DynFlags -> TyCon -> Text
tyConClass dflags tyCon = moduleClass typeName typeClass
  where typeName = tyConName tyCon
        typeClass = nameTypeText dflags typeName

labelToMethod :: FastString -> (Text, Text)
labelToMethod fs = ( T.dropEnd 1 $ T.dropWhileEnd (/= '.') label
                   , T.takeWhileEnd (/= '.') label )
  where label = fastStringToText fs
