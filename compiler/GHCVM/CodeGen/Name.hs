{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module GHCVM.CodeGen.Name (
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
import Data.Text as T hiding (map, init, last)
import Data.Text.Encoding

import Codec.JVM

idNameText :: Id -> Text
idNameText = zEncodeText
           . occNameFS
           . nameOccName
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

upperFirst :: Text -> Text
upperFirst str = case uncons str of
  Nothing -> empty
  Just (c, str') -> cons (C.toUpper c) str'

-- Codec.JVM.ASM -> ("codec/jvm", "ASM")
generatePackageAndClass :: Module -> (Text, Text)
generatePackageAndClass mod = (qClassName, className)
  where
    mods = split (== '.') $ modNameText mod
    (parentMods, className') = (init mods, last mods)
    packageString = packageKeyText mod
    package = append packageString
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
