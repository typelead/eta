
module Java.ClassPath.Types where

import Control.Monad.State
import Data.List

import JVM.ClassFile

-- | Directories tree
data Tree a =
    Directory FilePath [Tree a]
  | File a
  deriving (Eq)

instance Show a => Show (Tree a) where
  show (Directory dir forest) = dir ++ "/{" ++ intercalate ", " (map show forest) ++ "}"
  show (File a) = show a

-- | ClassPath entry
data CPEntry =
    NotLoaded FilePath                -- ^ Not loaded .class file
  | Loaded FilePath (Class Direct)    -- ^ Class loaded from .class file
  | NotLoadedJAR FilePath FilePath    -- ^ Not loaded .jar file
  | LoadedJAR FilePath (Class Direct) -- ^ Class loaded from .jar file
  deriving (Eq)

instance Show CPEntry where
  show (NotLoaded path) = "<Not loaded file: " ++ path ++ ">"
  show (Loaded path cls) = "<Loaded from " ++ path ++ ": " ++ toString (thisClass cls) ++ ">"
  show (NotLoadedJAR jar path) = "<Not loaded JAR: " ++ jar ++ ": " ++ path ++ ">"
  show (LoadedJAR path cls) = "<Read JAR: " ++ path ++ ": " ++ toString (thisClass cls) ++ ">"

-- | ClassPath monad
type ClassPath a = StateT [Tree CPEntry] IO a

