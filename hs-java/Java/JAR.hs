module Java.JAR
  (readManifest,
   readJAR,
   readMainClass,
   addJAR
  ) where

import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as St
import Data.List
import qualified Codec.Archive.LibZip as Zip

import Java.ClassPath
import Java.JAR.Archive
import Java.META

readManifest :: Zip.Archive (Maybe Manifest)
readManifest = do
  let manifestPath = "META-INF/MANIFEST.MF"
  files <- Zip.fileNames []
  if manifestPath `elem` files
    then do
         content <- Zip.fileContents [] manifestPath
         case parseMeta content of
           Left e -> fail $ show e
           Right meta -> return $ Just (loadSpec meta)
    else return Nothing

readOne :: FilePath -> String -> Zip.Archive [Tree CPEntry]
readOne jarfile str = do
    files <- Zip.fileNames []
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good name = (str `isPrefixOf` name) && (".class" `isSuffixOf` name)

-- | Read MainClass Entry of a MANIFEST.MF file
readMainClass :: FilePath -> IO (Maybe String)
readMainClass jarfile = do
  Zip.withArchive [] jarfile $ do
    m <- readManifest
    case m of
      Nothing -> return Nothing
      Just mf -> return $ mainClass mf

-- | Read entries from JAR file, using MANIFEST.MF if it exists.
readJAR :: FilePath -> IO [Tree CPEntry]
readJAR jarfile = do
  r <- Zip.withArchive [] jarfile $ do
         m <- readManifest
         case m of
           Nothing -> return Nothing
           Just mf -> do
                      trees <- mapM (readOne jarfile) (map meName $ manifestEntries mf)
                      let forest = merge (concat trees)
                      return (Just forest)
  case r of
    Nothing -> readAllJAR jarfile
    Just [] -> readAllJAR jarfile
    Just f  -> return f

-- | Add given JAR file to ClassPath
addJAR :: FilePath -> ClassPath ()
addJAR jarfile = do
  classes <- liftIO $ readJAR jarfile
  cp <- St.get
  let cp' = merge $ cp ++ classes
  St.put cp'

