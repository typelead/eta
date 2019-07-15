{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle.Shell
import Turtle.Line
import Turtle.Prelude hiding (die, sort, nub, sortBy)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getAppUserDataDirectory, getDirectoryContents, removeDirectoryRecursive)
import System.FilePath ((</>), dropExtension)
import qualified Data.ByteString.Lazy as BS

import Data.Monoid ((<>))
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Data.String
import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text],
      ignored :: [Text],
      ignoredVersions :: [Text],
      test :: [Text],
      testVersions :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) =
      Packages <$> v.: "patched"
               <*> v.: "vanilla"
               <*> v.: "ignored"
               <*> v.: "ignored-versions"
               <*> v.: "test"
               <*> v.: "test-versions"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO (Maybe Packages)
parsePackagesFile fname = do
  contents <- BS.readFile fname
  traverse modifyPatchedLibraries $ decode contents

packagesFilePath :: IO FilePath
packagesFilePath = (</> "patches" </> "packages.json") <$> getAppUserDataDirectory "etlas"

modifyPatchedLibraries :: Packages -> IO Packages
modifyPatchedLibraries pkgs = do
  patchesDir     <- fmap (</> "patches" </> "patches") $ getAppUserDataDirectory "etlas"
  packageListing <- getDirectoryContents patchesDir
  let packages = map T.pack
               . sort
               . nub
               . map dropExtension
               . filter (\p -> p `notElem` ["",".",".."])
               $ packageListing
  return $ pkgs { patched = filterLibraries pkgs packages }

-- These packages will not be verified
dontVerify :: [Text]
dontVerify = ["sbv"]

filterLibraries :: Packages -> [Text] -> [Text]
filterLibraries Packages { ignored         = ignoredPackages
                         , ignoredVersions = ignoredPackageVersions } set0 =
  recentVersions -- ++ remoteVersions ++ concat restVersions
  where (recentVersions, _restVersions) = unzip $ map findAndExtractMaximum
                                               $ groupBy grouping set1
        _remoteVersions = map actualName recentVersions
        set1 = filter (\s -> not ((any (== (actualName s)) ignoredPackages) ||
                                  (any (== s) ignoredPackageVersions))) set0
        grouping p1 p2 = actualName p1 == actualName p2

actualName :: Text -> Text
actualName = T.dropEnd 1 . T.dropWhileEnd (/= '-')

actualVersion :: Text -> [Int]
actualVersion = map (read . T.unpack) . T.split (== '.') . actualVersion'

actualVersion' :: Text -> Text
actualVersion' = T.takeWhileEnd (/= '-')

cmpVersion :: [Int] -> [Int] -> Ordering
cmpVersion xs ys
  | (x:_) <- dropWhile (== 0) $ map (uncurry (-)) $ zip xs ys
  = compare x 0
  | otherwise = compare (length xs) (length ys)

findAndExtractMaximum :: [Text] -> (Text, [Text])
findAndExtractMaximum g = (last pkgVersions, init pkgVersions)
  where pkgVersions = sortBy (\a b -> cmpVersion (actualVersion a) (actualVersion b)) g

verifyJar :: IO ()
verifyJar = sh verifyScript

procExitOnError :: Maybe String -> Text -> [Text] -> Shell Line -> Shell ()
procExitOnError mdir prog args shellm = do
  case mdir of
    Just dir -> cd (fromString dir)
    Nothing -> return ()
  exitCode <- proc prog args shellm
  case exitCode of
    ExitFailure code -> liftIO $ die ("ExitCode " ++ show code)
    ExitSuccess -> return ()
  when (isJust mdir) $ cd ".."

verifyScript :: Shell ()
verifyScript = do
  echo "Building the Verify script..."
  let verifyScriptPath = "utils" </> "class-verifier"
      verifyScriptCmd  = verifyScriptPath </> "Verify.java"
      testVerifyPath = "tests" </> "verify"
      outPath = testVerifyPath </> "build"
      outJar = outPath </> "Out.jar"
      mainSource = testVerifyPath </> "Main.hs"
  procExitOnError Nothing "javac" [T.pack verifyScriptCmd] mempty
  echo "Verify.class built successfully."
  echo "Compiling a simple program..."
  echo "=== Eta Compiler Output ==="
  exists <- testdir (fromString outPath)
  when (not exists) $ mkdir (fromString outPath)
  procExitOnError Nothing "eta" ["-fforce-recomp", "-o", T.pack outJar, T.pack mainSource] mempty
  echo "===                     ==="
  echo "Compiled succesfully."
  echo "Verifying the bytecode of compiled program..."
  echo "=== Verify Script Output ==="
  procExitOnError Nothing "java" ["-cp", T.pack verifyScriptPath, "Verify", T.pack outJar] mempty
  echo "===                      ==="
  echo "Bytecode looking good."
  echo "Running the simple program..."
  echo "=== Simple Program Output ==="
  procExitOnError Nothing "java" ["-cp", T.pack outJar, "eta.main"] mempty
  echo "===                       ==="
  echo "Done! Everything's looking good."

main :: IO ()
main = do
  verifyJar
  let vmUpdateCmd = "etlas update"
  _ <- shell vmUpdateCmd ""
  epmPkgs <- packagesFilePath
  pkg <- parsePackagesFile epmPkgs
  case pkg of
    Nothing -> die "Problem parsing your packages.json file"
    Just pkg' -> do
      let packages = patched pkg' <> vanilla pkg'
          tests = test pkg'
          extraTests = testVersions pkg'
          runPackageTest shouldTest package = do
            sh $ procExitOnError Nothing "etlas" ["get", package] empty
            let maybeVerify
                  | any (`T.isPrefixOf` package) dontVerify = []
                  | otherwise = ["--enable-verify"]
                command
                  | shouldTest = ["test"]
                  | otherwise  = ["build"]
            let projectFile = unlines ["packages: ."]
            writeFile (T.unpack package </> "cabal.project") projectFile
            sh $ procExitOnError (Just (T.unpack package)) "etlas" (command <> maybeVerify) empty
            removeDirectoryRecursive (T.unpack package)
      mapM_ (\p -> runPackageTest (p `elem` tests) p) packages
      mapM_ (runPackageTest True) extraTests