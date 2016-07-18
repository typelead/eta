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
  classFilePath
  ) where

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

import Codec.JVM

qualifiedName :: Text -> Text -> Text
qualifiedName modClass className = append modClass . cons '$' $ className

closure :: Text -> Text
closure = flip append "_closure"

fastStringToText :: FastString -> Text
fastStringToText = decodeUtf8 . fastStringToByteString

nameTypeText :: Name -> Text
nameTypeText = flip snoc 'T' . nameText

nameTypeTable :: Name -> Text
nameTypeTable = flip append "_table" . nameText

nameDataText :: Name -> Text
nameDataText = flip snoc 'D' . nameText

nameText :: Name -> Text
nameText = zEncodeText
         . occNameFS
         . nameOccName

idNameText :: Id -> Text
idNameText = nameText
           . idName

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
  Nothing -> empty
  Just (c, str') -> cons (C.toUpper c) str'

modClosure :: Module -> Name -> (Text, Text)
modClosure mod name = (moduleJavaClass mod, nameText name)

moduleClass :: Name -> Text -> Text
moduleClass name = qualifiedName moduleClass
        -- TODO: Most likely this will fail for same module data cons
        -- Maybe externalize the data con name?
  where moduleClass = moduleJavaClass
                    . fromMaybe (error "Failed")
                    $ nameModule_maybe name

dataConClass :: DataCon -> Text
dataConClass dataCon = moduleClass dataName dataClass
  where dataName = dataConName dataCon
        dataClass = nameDataText dataName

tyConClass :: TyCon -> Text
tyConClass tyCon = moduleClass typeName typeClass
  where typeName = tyConName tyCon
        typeClass = nameTypeText typeName
