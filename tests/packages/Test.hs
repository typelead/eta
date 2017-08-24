{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle.Shell
import Turtle.Line
import Turtle.Prelude hiding (die)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getAppUserDataDirectory, getDirectoryContents)
import System.FilePath ((</>), dropExtension)
import qualified Data.ByteString.Lazy as BS

import Data.Monoid ((<>))
import Data.List
import Control.Applicative
import Control.Monad
import Data.String
import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) = Packages <$> v.: "patched" <*> v.: "vanilla"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO (Maybe Packages)
parsePackagesFile fname = do
  contents <- BS.readFile fname
  let packages = decode contents
  patched' <- patchedLibraries
  return $ fmap (\p -> p { patched = patched' }) packages

packagesFilePath :: IO FilePath
packagesFilePath = (</> "patches" </> "packages.json") <$> getAppUserDataDirectory "etlas"

patchedLibraries :: IO [Text]
patchedLibraries = do
  patchesDir     <- fmap (</> "patches" </> "patches") $ getAppUserDataDirectory "etlas"
  packageListing <- getDirectoryContents patchesDir
  let packages = map T.pack
               . sort
               . nub
               . map dropExtension
               . filter (\p -> p `notElem` ["",".",".."])
               $ packageListing
  return $ filterLibraries packages

-- These will not be built for various reasons.
ignoredPackages :: [Text]
ignoredPackages = ["singletons" ,"directory", "servant-docs"]

ignoredPackageVersions :: [Text]
ignoredPackageVersions = []

filterLibraries :: [Text] -> [Text]
filterLibraries set0 = recentVersions ++ remoteVersions ++ concat restVersions
  where (recentVersions, restVersions) = unzip $ map findAndExtractMaximum
                                               $ groupBy grouping set1
        remoteVersions = map actualName recentVersions
        set1 = filter (\s -> not ((any (== (actualName s)) ignoredPackages) ||
                                  (any (== s) ignoredPackageVersions))) set0
        grouping p1 p2 = actualName p1 == actualName p2

actualName :: Text -> Text
actualName = T.dropEnd 1 . T.dropWhileEnd (/= '-')

actualVersion :: Text -> [Int]
actualVersion = map (read . T.unpack) . T.split (== '.') .  T.takeWhileEnd (/= '-')

cmpVersion :: [Int] -> [Int] -> Ordering
cmpVersion xs ys
  | (x:_) <- dropWhile (== 0) $ map (uncurry (-)) $ zip xs ys
  = compare x 0
  | otherwise = compare (length xs) (length ys)

findAndExtractMaximum :: [Text] -> (Text, [Text])
findAndExtractMaximum g = (last pkgVersions, init pkgVersions)
  where pkgVersions = sortBy (\a b -> cmpVersion (actualVersion a) (actualVersion b)) g

buildPackage :: Text -> IO ()
buildPackage pkg = do
  let outString = "Installing package " ++ T.unpack pkg ++ "..."
      lenOutString = length outString
      dashes = replicate lenOutString '-'
  putStrLn dashes
  putStrLn outString
  putStrLn dashes
  sh $ procExitOnError "etlas" ["install", pkg] empty

verifyJar :: IO ()
verifyJar = sh verifyScript

procExitOnError :: Text -> [Text] -> Shell Line -> Shell ()
procExitOnError prog args shellm = do
  exitCode <- proc prog args shellm
  case exitCode of
    ExitFailure code -> liftIO $ die ("ExitCode " ++ show code)
    ExitSuccess -> return ()

verifyScript :: Shell ()
verifyScript = do
  echo "Building the Verify script..."
  let verifyScriptPath = "utils" </> "class-verifier"
      verifyScriptCmd  = verifyScriptPath </> "Verify.java"
      testVerifyPath = "tests" </> "verify"
      outPath = testVerifyPath </> "build"
      outJar = outPath </> "Out.jar"
      mainSource = testVerifyPath </> "Main.hs"
  procExitOnError "javac" [T.pack verifyScriptCmd] mempty
  echo "Verify.class built successfully."
  echo "Compiling a simple program..."
  echo "=== Eta Compiler Output ==="
  exists <- testdir (fromString outPath)
  when (not exists) $ mkdir (fromString outPath)
  procExitOnError "eta" ["-fforce-recomp", "-o", T.pack outJar, T.pack mainSource] mempty
  echo "===                     ==="
  echo "Compiled succesfully."
  echo "Verifying the bytecode of compiled program..."
  echo "=== Verify Script Output ==="
  procExitOnError "java" ["-cp", T.pack verifyScriptPath, "Verify", T.pack outJar] mempty
  echo "===                      ==="
  echo "Bytecode looking good."
  echo "Running the simple program..."
  echo "=== Simple Program Output ==="
  procExitOnError "java" ["-cp", T.pack outJar, "eta.main"] mempty
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
      let packages = (patched pkg') <> (vanilla pkg')
      mapM_ buildPackage packages
