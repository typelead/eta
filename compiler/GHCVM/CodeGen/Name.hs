{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module GHCVM.CodeGen.Name (
  nameText,
  nameTypeText,
  idNameText,
  idClassText,
  generatePackageAndClass,
  classFilePath
  ) where

import Module
import FastString
import Name
import Id

import Data.Char as C
import Data.Maybe
import Data.Text as T hiding (map, init, last, null)
import Data.Text.Encoding

import Codec.JVM

nameTypeText :: Name -> Text
nameTypeText = flip snoc '$' . nameText

nameText :: Name -> Text
nameText = zEncodeText
         . occNameFS
         . nameOccName

idNameText :: Id -> Text
idNameText = nameText
           . idName

idClassText :: Id -> Text
idClassText = modNameText
            . fromJust
            . nameModule_maybe
            . idName

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


-- Codec.JVM.ASM -> ("codec/jvm", "ASM")
generatePackageAndClass :: Module -> (Text, Text)
generatePackageAndClass mod = (qClassName, className)
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
