{-# LANGUAGE MagicHash, OverloadedStrings #-}
module GHCVM.CodeGen.Name (
  qualifiedName,
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
  labelToMethod) where

import GHCVM.Main.DynFlags
import GHCVM.Types.TyCon
import GHCVM.BasicTypes.DataCon
import GHCVM.BasicTypes.Module
import GHCVM.Utils.FastString
import GHCVM.Utils.FastTypes
import GHCVM.BasicTypes.Name hiding (pprOccName)
import GHCVM.BasicTypes.Unique
import GHCVM.BasicTypes.Id

import GHC.Base(indexCharOffAddr#, Char(..))
import Data.Char as C
import Data.Maybe
import Data.Text as T hiding (map, init, last, null)
import Data.Text.Encoding

import GHCVM.Utils.Outputable((<>))
import GHCVM.Debug
import GHCVM.Utils.Encoding

import Codec.JVM

qualifiedName :: Text -> Text -> Text
qualifiedName modClass className = append modClass . cons '$' $ className

closure :: Text -> Text
closure = flip append "_closure"

nameTypeText :: DynFlags -> Name -> Text
nameTypeText dflags = flip snoc 'T' . nameText dflags False

nameTypeTable :: DynFlags -> Name -> Text
nameTypeTable dflags = flip append "_table" . nameText dflags False

nameDataText :: DynFlags -> Name -> Text
nameDataText dflags = flip snoc 'D' . nameText dflags False

nameText :: DynFlags -> Bool -> Name -> Text
nameText dflags caseEncode = T.pack
                           . maybeEncodeCase
                           . zEncodeString
                           . showSDoc dflags
                           . pprNameCI
  where maybeEncodeCase = if caseEncode then encodeCase else id

encodeCase :: String -> String
encodeCase str@(c:rest)
  | isUpper c = 'D':str
  | otherwise = str

idNameText :: DynFlags -> Id -> Text
idNameText dflags = nameText dflags True . idName

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
modClosure dflags mod name = (moduleJavaClass mod, nameText dflags True name)

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

labelToMethod :: String -> (Text, Text)
labelToMethod s = ( T.replace "." "/" . T.dropEnd 1 . T.dropWhileEnd (/= '.') $ label
                  , T.takeWhileEnd (/= '.') label )
  where label = T.pack s

-- Custom Outputable instance for Name,
-- since we need to be case-insensitive.
-- Take directly from GHC with modifications.
-- iToBase36 :: Int -> String
-- iToBase36 n_ = go (iUnbox n_) ""
--   where
--     go n cs
--       | n <# 36# = case chooseChar36 n of { c -> c `seq` (c : cs) }
--       | otherwise =  case quotRem (iBox n) 36 of
--                        (q_, r_) -> case iUnbox q_ of
--                          q -> case iUnbox r_ of
--                            r -> case chooseChar36 r of
--                              c -> c `seq` go q (c : cs)

--     chooseChar36 :: FastInt -> Char
--     {-# INLINE chooseChar36 #-}
--     chooseChar36 n = C# (indexCharOffAddr# chars36 n)
--     !chars36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"#

-- -- pretty-print name, case-insensitive
pprNameCI :: Name -> SDoc
pprNameCI name
  = getPprStyle $ \ sty ->
        -- pprZOccName occ <> pprUnderscoreUnique uniq
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
pprUnderscoreUnique uniq = char '_' <> ppr uniq

pprOccName :: OccName -> SDoc
pprOccName  = ftext . occNameFS

pprZOccName :: OccName -> SDoc
pprZOccName = ztext . zEncodeFS . occNameFS
