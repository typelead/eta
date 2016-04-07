-- | This module declares functions and data types for
-- JAR meta-information classes, such as MANIFEST.MF etc.
module Java.META
  (module Java.META.Types,
   module Java.META.Parser,
   module Java.META.Spec,
   Manifest (..),
   ManifestEntry (..))
  where

import qualified Data.Map as M
import Data.Map ((!))

import Java.META.Types
import Java.META.Parser
import Java.META.Spec

-- | JAR MANIFEST.MF
data Manifest = Manifest {
  manifestVersion :: String,
  createdBy :: String,
  sealed :: Bool,
  signatureVersion :: Maybe String,
  classPath :: [String],
  mainClass :: Maybe String,
  manifestEntries :: [ManifestEntry]}
  deriving (Eq, Show)

-- | Manifest entry
data ManifestEntry = ManifestEntry {
  meName :: String,
  meSealed :: Bool,
  meContentType :: Maybe String,
  meBean :: Bool }
  deriving (Eq, Show)

instance MetaSpec Manifest where
  loadFirstSection s = Manifest {
    manifestVersion = s ! "Manifest-Version",
    createdBy = s ! "Created-By",
    sealed = case M.lookup "Sealed" s of
               Nothing -> False
               Just str -> string2bool str,
    signatureVersion = M.lookup "Signature-Version" s,
    classPath = case M.lookup "Class-Path" s of
                  Nothing -> []
                  Just str -> words str,
    mainClass = M.lookup "Main-Class" s,
    manifestEntries = []}

  loadOtherSection m s = m {manifestEntries = manifestEntries m ++ [entry]}
    where
      entry = ManifestEntry {
                meName = s ! "Name",
                meSealed = case M.lookup "Sealed" s of
                             Nothing -> sealed m
                             Just str -> string2bool str,
                meContentType = M.lookup "Content-Type" s,
                meBean = case M.lookup "Java-Bean" s of
                           Nothing -> False
                           Just str -> string2bool str }

  storeMeta m = first: map store (manifestEntries m)
    where
      first = M.fromList $ [
          ("Manifest-Version", manifestVersion m),
          ("Created-By", createdBy m)] ++
          lookupList "Signature-Version" (signatureVersion m) ++
          lookupList "Main-Class" (mainClass m) ++
          case classPath m of
            [] -> []
            list -> [("Class-Path", unwords list)]

      store e = M.fromList $ [
          ("Name", meName e),
          ("Sealed", bool2string $ meSealed e)] ++
          lookupList "Content-Type" (meContentType e) ++
          if meBean e
            then [("Java-Bean", "true")]
            else []

