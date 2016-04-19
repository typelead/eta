module Java.ClassPath
  (module Java.ClassPath.Types,
   module Java.ClassPath.Common,
   appendPath, addDirectory, loadClass,
   runClassPath, execClassPath,
   getEntry
  ) where

import qualified Control.Monad.State as St
import Control.Monad.Trans (liftIO)
import System.FilePath.Glob hiding (glob)
import Data.String.Utils (split)

import JVM.ClassFile
import JVM.Converter
import Java.ClassPath.Types
import Java.ClassPath.Common
import Java.JAR.Archive

-- | For given list of glob masks, return list of matching files
glob :: FilePath -> [FilePath] -> IO [FilePath]
glob dir patterns = do
  (matches, _) <- globDir (map compile patterns) dir
  return $ concat matches

-- | Append one file to ClassPath forest
appendPath :: FilePath -> [Tree CPEntry] -> [Tree CPEntry]
appendPath path forest = merge $ forest ++ (mapF NotLoaded $ buildTree [path])

-- | Add one directory to current ClassPath
addDirectory :: FilePath -> ClassPath ()
addDirectory dir = do
  files <- liftIO $ glob dir ["*.class"]
  cp <- St.get
  let cp' = foldr appendPath cp files
  St.put cp'

-- | Run ClassPath monad
runClassPath :: ClassPath a -> IO a
runClassPath m = St.evalStateT m []

-- | Run ClassPath monad and return resulting ClassPath
execClassPath :: ClassPath () -> IO [Tree CPEntry]
execClassPath m = St.execStateT m []

-- | Load one class in current ClassPath
loadClass :: String -> ClassPath ()
loadClass path = do
    cp <- St.get
    cp' <- liftIO $ mapM (load xs) cp
    St.put cp'
  where
    xs = split "/" path

    load :: [String] -> Tree CPEntry -> IO (Tree CPEntry)
    load [] t = return t
    load (p:ps) t@(Directory dir forest)
      | p == dir  = Directory dir `fmap` mapM (load ps) forest
      | otherwise = return t
    load [p] t@(File (NotLoaded f))
      | (p ++ ".class") == f = do
                               cls <- parseClassFile (path ++ ".class")
                               return (File $ Loaded path cls)
      | otherwise = return t
    load [p] t@(File (NotLoadedJAR jarfile f))
      | (p ++ ".class") == f = do
                               cls <- readFromJAR jarfile (path ++ ".class")
                               return (File $ LoadedJAR jarfile cls)
      | otherwise = return t
    load ps (File _) = fail $ "Found file when expecting directory! " ++ show ps

-- | Get one ClassPath entry
getEntry :: [Tree CPEntry] -> String -> IO (Maybe CPEntry)
getEntry cp path = get cp (split "/" path)
  where
    get :: [Tree CPEntry] -> [String] -> IO (Maybe CPEntry)
    get _ [] = fail "Empty path for ClassPath.getEntry.get!"
    get [] _ = return Nothing
    get (Directory dir forest: es) (p:ps)
      | dir == p  = get forest ps
      | otherwise = get es (p:ps)
    get (File i@(NotLoaded f): es) [p]
      | (p ++ ".class" == f) = do
                               cls <- parseClassFile (path ++ ".class")
                               return $ Just (Loaded path cls)
      | otherwise = get es [p]
    get (File i@(NotLoadedJAR jarfile r): es) [p]
      | (p ++ ".class" == r) = do
                               cls <- readFromJAR jarfile (path ++ ".class")
                               return $ Just (LoadedJAR jarfile cls)
      | otherwise = get es [p]
    get (File i@(Loaded f c):es) [p]
      | f == p = return (Just i)
      | otherwise = get es [p]
    get (File i@(LoadedJAR f c):es) [p]
      | toString (thisClass c) == path = return (Just i)
      | otherwise = get es [p]
    get x y = fail $ "Unexpected arguments for ClassPath.getEntry.get: " ++ show x ++ ", " ++ show y

