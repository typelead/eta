{-#LANGUAGE OverloadedStrings#-}

{-|
Module      : ETA.Utils.JAR
Description : Small utility functions for creating Jars from class files.
Copyright   : (c) Christopher Wells 2016
              (c) Rahul Muttineni 2016-2017
License     : MIT
This module provides utility functions for creating Jar archives from JVM class
files.
The general process used for creating a Jar archive is to first create an
empty Jar in the desired location using `createEmptyJar`. After the Jar has
been created, files can be added to it using the `addByteStringToJar` and
`addMultiByteStringsToJar` functions.
When adding multiple files to a Jar, be sure to use the
`addMultiByteStringsToJar` function, as it writes all of the file changes in
one action, while mapping over a list of files with `addByteStringToJar` would
perform the file write actions all seperately.
Here is a quick exampe of how to create a Jar and add a file into it.
@
-- Create the empty jar
let jarLocation = "build/Hello.jar"
createEmptyJar jarLocation
-- Add a single file to the jar
import Data.ByteString.Internal (packChars)
let fileLocation = "hellopackage/Main.class"
let fileContents = packChars "Hello, World!"
addByteStringToJar fileLocation fileContents jarLocation
@
-}
module ETA.Utils.JAR
  ( addMultiByteStringsToJar'
  , addByteStringToJar
  , createEmptyJar
  , getEntriesFromJar
  , mergeClassesAndJars
  , CompressionMethod
  , deflate
  , normal
  , bzip2
  , mkPath )
where

import Codec.Archive.Zip
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadCatch(..), MonadThrow)
import Data.ByteString.Internal (ByteString)
import Path
import Path.IO (copyFile)
import Data.Map.Lazy (keys)
import Data.Map.Strict (filterWithKey)
import Data.List (sortBy,isPrefixOf )
import System.Directory hiding (copyFile)
import Data.Text (Text)
import qualified Data.Text as T


type FileAndContents = (RelativeFile, ByteString)
type AbsoluteFile = Path Abs File
type RelativeFile = Path Rel File

deflate, normal, bzip2 :: CompressionMethod
deflate = Deflate
normal = Store
bzip2 = BZip2

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- __Throws__: 'PathParseException'
--
-- See 'createArchive' and 'parseRelFile' for more information.
--
-- For example, passing in "src\/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
--
-- @
-- createEmptyJar "src/Main.jar"
-- @
--
-- __Before__
--
-- @
-- .
-- └── src
-- @
--
-- __After__
--
-- @
-- .
-- └── src
--     └── Main.jar
-- @
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> m ()
createEmptyJar location = do
    p <- makeAbsoluteFilePath location
    createArchive p (return ())
  

-- | Adds the given ByteString as a file at the given location within the given
-- jar archive.
--
-- __Throws__: 'PathParseException', 'EntrySelectorException',
-- isAlreadyInUseError, isDoesNotExistError, isPermissionError, 'ParsingFailed'
--
-- See 'withArchive', 'mkEntrySelector', and 'parseRelFile' for more information.
--
-- For example, running the following would create a file named "Hello.class"
-- containing the string "Hello, World!" within the "src" directory in the jar
-- archive located at "build\/libs\/HelloWorld.jar".
--
-- @
-- let fileLocation = "src\/Hello.class"
-- let contents = packChars "Hello, World!"
-- let jarLocation = "build\/libs\/HelloWorld.jar"
-- addByteStringToJar fileLocation contents jarLocation
-- @
--
-- __Before__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
-- @
--
-- __After__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
--             └── src
--                 └── Hello.class
-- @
addByteStringToJar :: (MonadThrow m, MonadIO m)
  => FilePath      -- ^ Location of the new file within the jar
  -> CompressionMethod -- ^ Compression Method
  -> ByteString    -- ^ Contents of the new file to add
  -> FilePath      -- ^ Location of the jar to add the new file into
  -> m ()
addByteStringToJar fileLocation compress contents jarLocation = zipAction
  where zipAction = jarPath >>= flip withArchive zipChange
        zipChange = entrySel >>= addEntry compress contents
        entrySel  = filePath >>= mkEntrySelector
        jarPath   = parseRelFile jarLocation
        filePath  = parseRelFile fileLocation

-- | Adds the given files into the given jar. Each file is represented by a
-- tuple containing the location where the file will be added inside the jar,
-- and the contents of the file as a ByteString.
--
-- __Throws__: 'PathParseException', 'EntrySelectorException',
-- isAlreadyInUseError, isDoesNotExistError, isPermissionError, 'ParsingFailed'
--
-- See 'withArchive', 'mkEntrySelector', and 'parseRelFile' for more information.
--
-- For example, running the following would create two files within the jar
-- archive located at "build\/libs\/HelloWorld.jar". The first file would be
-- named "Hello.class" containing the string "Hello, World!" within the
-- "helloworld" directory in the jar archive. The second file would be named
-- \"MANIFEST.MF" containing the string "Manifest-Version: 1.0" within the
-- \"META-INF" directory in the jar archive.
--
-- @
-- let file1Location = "helloworld\/Hello.class"
-- let file1Contents = "Hello, World!"
-- let file1 = (file1Location, file1Contents)
--
-- let file2Location = "META-INF\/MANIFEST.MF"
-- let file2Contents = "Manifest-Version: 1.0"
-- let file2 = (file2Location, file2Contents)
--
-- let files = [file1, file2]
-- let jarLocation = "build\/libs\/HelloWorld.jar"
-- addMultiByteStringsToJar files jarLocation
-- @
--
-- __Before__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
-- @
--
-- __After__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
--             ├── helloworld
--             │   └── Hello.class
--             └── META-INF
--                 └── MANIFEST.MF
-- @
-- addMultiByteStringsToJar
--   :: (MonadThrow m, MonadIO m)
--   => [(FilePath, ByteString)]    -- ^ Filepaths and contents of files to add into the jar
--   -> FilePath                    -- ^ Location of the jar to add the new files into
--   -> m ()
-- addMultiByteStringsToJar files jarLocation = do
--   jarPath <- parseRelFile jarLocation
--   withArchive jarPath $
--     forM_ files $ \(path, contents) -> do
--       filePath <- parseRelFile path
--       entrySel <- mkEntrySelector filePath
--       addEntry Deflate contents entrySel

addMultiByteStringsToJar'
  :: (MonadThrow m, MonadIO m, MonadCatch m)

  => FilePath                    -- ^ Location of the jar to add the new files into
  -> CompressionMethod -- ^ Compression Method
  -> [(Path Rel File, ByteString)]    -- ^ Filepaths and contents of files to add into the jar
  -> m ()
addMultiByteStringsToJar' jarLocation compress files  = do
    p <- makeAbsoluteFilePath jarLocation
    createArchive p action
  where action =
          forM_ files $ \(path, contents) -> do
            entrySel <- mkEntrySelector path
            addEntry compress contents entrySel

-- getFilesFromJar
--   :: (MonadThrow m, MonadCatch m, MonadIO m)
--   => FilePath
--   -> m [(Path Rel File, ByteString)]
-- getFilesFromJar jarLocation =
--   withUnsafePath jarLocation (flip withArchive action) (flip withArchive action)
--   where action = do
--           entrySelectors <- keys <$> getEntries
--           forM entrySelectors $ \es -> do
--             contents <- getEntry es
--             return (unEntrySelector es, contents)

mkPath :: (MonadThrow m, MonadIO m) => FilePath -> m (Path Rel File)
mkPath = parseRelFile

makeAbsoluteFilePath :: (MonadIO m, MonadThrow m) => FilePath -> m (Path Abs File)
makeAbsoluteFilePath fp = do
  absPath <- liftIO $ makeAbsolute  fp
  let absPath'= removeDots [] absPath
  parseAbsFile absPath'
  where
  removeDots :: String -> String -> String
  removeDots s ""= s
  removeDots s "/"= s ++ "/"
  removeDots scanned  path=
      let (h,t) = break (\c -> c=='\\' || c == '/') path
      in if null t then scanned ++ h
         else if ".." `isPrefixOf` tail t
             then removeDots scanned $ drop 4 t
             else removeDots(scanned ++ h ++ [head t] ) $ tail t


getEntriesFromJar
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => FilePath
  -> m (AbsoluteFile, [EntrySelector])
getEntriesFromJar jarLocation = do
  p <- makeAbsoluteFilePath jarLocation
  fmap (p,) $ withArchive p $ keys <$> getEntries

mergeClassesAndJars :: (MonadIO m, MonadCatch m, MonadThrow m)
                    => FilePath
                    -> CompressionMethod -- ^ Compression Method
                    -> [FileAndContents]
                    -> [(AbsoluteFile, [EntrySelector])]
                    -> m ()
mergeClassesAndJars jarLocation compress fileAndContents jarSelectors = do
  let ((copy, _):selectors) = sortBy (\(_, e1) (_, e2) ->
                                  compare (length e2) (length e1)) jarSelectors
  exists <- liftIO $ doesFileExist jarLocation
  when (not exists) $
    liftIO $ writeFile jarLocation ""
  p <- makeAbsoluteFilePath jarLocation
  copyFile copy p
  withArchive p $ do
    existingEntries <- getEntries
    let invalidEntries = filterWithKey (\k _ -> invalidEntrySelector k)  existingEntries
    mapM_ deleteEntry (keys invalidEntries)
    forM_ selectors $ \(absFile, entries) -> do
      forM_ entries $ \entry -> do
        when (invalidEntrySelector entry /= True) $ copyEntry absFile entry entry
    forM_ fileAndContents $ \(relFile, contents) -> do
      entrySel <- mkEntrySelector relFile
      addEntry compress contents entrySel
  where
    invalidEntrySelector :: EntrySelector -> Bool
    invalidEntrySelector fname = any (endsWith (getEntryName fname)) filterFileExts
    filterFileExts = [".SF", ".DSA", ".RSA"]

    endsWith :: Text -> Text -> Bool
    endsWith txt ext = T.toUpper (T.takeEnd (T.length ext) txt) == (T.toUpper ext)
