{-# LANGUAGE MagicHash #-}
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

import GHCVM.Main.DynFlags
import TyCon
import DataCon
import Module
import FastString
import FastTypes
import Name hiding (pprOccName)
import Unique
import Id

import GHC.Base(indexCharOffAddr#, Char(..))
import Data.Char as C
import Data.Maybe
import Data.Text as T hiding (map, init, last, null)
import Data.Text.Encoding

import Outputable((<>))
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
                . showSDoc dflags
                . pprNameCI

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

-- Custom Outputable instance for Name,
-- since we need to be case-insensitive.
-- Take directly from GHC with modifications.
iToBase36 :: Int -> String
iToBase36 n_ = go (iUnbox n_) ""
  where
    go n cs
      | n <# 36# = case chooseChar36 n of { c -> c `seq` (c : cs) }
      | otherwise =  case quotRem (iBox n) 36 of
                       (q_, r_) -> case iUnbox q_ of
                         q -> case iUnbox r_ of
                           r -> case chooseChar36 r of
                             c -> c `seq` go q (c : cs)

    chooseChar36 :: FastInt -> Char
    {-# INLINE chooseChar36 #-}
    chooseChar36 n = C# (indexCharOffAddr# chars36 n)
    !chars36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"#

pprUniqueCI :: Unique -> SDoc
pprUniqueCI = text . showUniqueCI

showUniqueCI :: Unique -> String
showUniqueCI uniq
  = case unpkUnique uniq of
      (tag, u) -> finishShow tag u (iToBase36 u)

finishShow :: Char -> Int -> String -> String
finishShow 't' u _pp_u | u < 26
  = -- Special case to make v common tyvars, t1, t2, ...
    -- come out as a, b, ... (shorter, easier to read)
    [chr (ord 'a' + u)]
finishShow tag _ pp_u = tag : pp_u

-- pretty-print name, case-insensitive
pprNameCI :: Name -> SDoc
pprNameCI name
  = getPprStyle $ \ sty ->
      if isInternalName name then pprInternal sty uniq occ
      else if isSystemName name then pprSystem sty uniq occ
      else pprOccName occ
  where uniq = mkUniqueGrimily (iBox (getKeyFastInt u))
        u = nameUnique name
        occ = nameOccName name

pprExternal :: PprStyle -> Unique -> Module -> OccName -> Bool -> BuiltInSyntax -> SDoc
pprExternal sty uniq mod occ is_wired is_builtin = pprOccName occ

pprInternal :: PprStyle -> Unique -> OccName -> SDoc
pprInternal sty uniq occ = pprOccName occ <> pprUnderscoreUnique uniq

pprSystem :: PprStyle -> Unique -> OccName -> SDoc
pprSystem sty uniq occ = pprOccName occ <> pprUnderscoreUnique uniq

pprUnderscoreUnique :: Unique -> SDoc
-- Print an underscore separating the name from its unique
-- But suppress it if we aren't printing the uniques anyway
pprUnderscoreUnique uniq = char '_' <> pprUniqueCI uniq

pprOccName :: OccName -> SDoc
pprOccName  = ftext . occNameFS

pprZOccName :: OccName -> SDoc
pprZOccName = ztext . zEncodeFS . occNameFS
