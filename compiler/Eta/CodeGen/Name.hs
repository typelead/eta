{-# LANGUAGE MagicHash, OverloadedStrings #-}
module Eta.CodeGen.Name (
  qualifiedName,
  fastStringText,
  fastZStringText,
  nameText,
  nameTypeText,
  nameTypeTable,
  idNameText,
  idClassText,
  moduleJavaClass,
  dataConClass,
  tyConClass,
  moduleClass,
  closure,
  classFilePath,
  labelToMethod) where

import Eta.Main.DynFlags
import Eta.Types.TyCon
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.Module
import Eta.Utils.FastString
import Eta.BasicTypes.Name hiding (pprOccName)
import Eta.BasicTypes.Unique
import Eta.BasicTypes.Id
-- import GHC.Base(indexCharOffAddr#, Char(..))
import Data.Char as C
import Data.Maybe
import qualified Data.List as L
import Data.Text as T hiding (map, init, last, null)
import Data.Text.Encoding

import Eta.Utils.Outputable((<>))
import Eta.Debug
import Eta.Utils.Encoding
import qualified Eta.Utils.Util as Split (split)

import Codec.JVM

qualifiedName :: Text -> Text -> Text
qualifiedName modClass className = append modClass . cons '$' $ className

subpackageName :: Text -> Text -> Text -> Text
subpackageName subpkg modClass className
  = T.concat [T.toLower modClass, "/", subpkg, "/", className]

-- TODO: Remove this entirely
closure :: Text -> Text
closure = id

nameTypeText :: DynFlags -> Name -> Text
nameTypeText dflags name
  | T.toLower res == "con" = T.concat ["Z", res, "Z"]
  | otherwise = res
  where res = nameText dflags False name

nameTypeTable :: DynFlags -> Name -> Text
nameTypeTable dflags = flip append "_table" . nameText dflags False

nameText :: DynFlags -> Bool -> Name -> Text
nameText dflags caseEncode = T.pack
                           . maybeEncodeCase
                           . zEncodeString
                           . showSDoc dflags
                           . pprNameCI
  where maybeEncodeCase = if caseEncode then encodeCase else id

encodeCase :: String -> String
encodeCase str@(c:_)
  | isUpper c = 'D':str
  | otherwise = str
encodeCase _ = ""

idNameText :: DynFlags -> Id -> Text
idNameText dflags = nameText dflags True . idName

idClassText :: Id -> Text
idClassText id =
  case nameModule_maybe (idName id) of
    Just mod -> modNameText mod
    Nothing -> error $ "idClassText: "

fastStringText :: FastString -> Text
fastStringText = decodeUtf8 . fastStringToByteString

fastZStringText :: FastZString -> Text
fastZStringText = decodeUtf8 . fastZStringToByteString

modNameText :: Module -> Text
modNameText = fastStringText . moduleNameFS . moduleName

unitIdText :: Module -> Text
unitIdText mod = T.pack
               . map C.toLower
               . L.intercalate "_"
               . L.takeWhile (not . L.all (\c -> C.isDigit c || c == '.'))
               $ Split.split '-' uid
  where uid = unitIdString $ moduleUnitId mod

-- Codec.JVM.ASM -> "codec/jvm/ASM"
moduleJavaClass :: Module -> Text
moduleJavaClass mod = qClassName
  where
    mods = split (== '.') $ modNameText mod
    (parentMods, className') = (init mods, last mods)
    packageString = T.toLower . unitIdText $ mod
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

moduleClass :: Name -> Text -> Text
moduleClass name = qualifiedName moduleClass
  where moduleClass = moduleJavaClass
                    . fromMaybe (error "Failed")
                    $ nameModule_maybe name

submoduleClass :: Text -> Name -> Text -> Text
submoduleClass subpkg name = subpackageName subpkg moduleClass
  where moduleClass = moduleJavaClass
                    . fromMaybe (error "Failed")
                    $ nameModule_maybe name

dataConClass :: DynFlags -> DataCon -> Text
dataConClass dflags dataCon = submoduleClass "datacons" dataName dataClass
  where dataName = dataConName dataCon
        dataClass = nameTypeText dflags dataName

tyConClass :: DynFlags -> TyCon -> Text
tyConClass dflags tyCon = submoduleClass "tycons" typeName typeClass
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
  where uniq = mkUniqueGrimily (getKey u)
        u = nameUnique name
        occ = nameOccName name

-- pprExternal :: PprStyle -> Unique -> Module -> OccName -> Bool -> BuiltInSyntax -> SDoc
-- pprExternal sty uniq mod occ is_wired is_builtin = pprOccName occ

pprInternal :: PprStyle -> Unique -> OccName -> SDoc
pprInternal _ uniq occ = pprOccName occ <> pprUnderscoreUnique uniq

pprSystem :: PprStyle -> Unique -> OccName -> SDoc
pprSystem _ uniq occ = pprOccName occ <> pprUnderscoreUnique uniq

pprUnderscoreUnique :: Unique -> SDoc
-- Print an underscore separating the name from its unique
-- But suppress it if we aren't printing the uniques anyway
pprUnderscoreUnique uniq = char '_' <> ppr uniq

pprOccName :: OccName -> SDoc
pprOccName  = ftext . occNameFS

-- pprZOccName :: OccName -> SDoc
-- pprZOccName = ztext . zEncodeFS . occNameFS
