{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.String
import Turtle.Shell
import Turtle.Line
import Turtle.Prelude hiding (die)
import qualified Data.ByteString.Lazy as BS
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import Data.Text (pack, Text)

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
  return packages

packagesFilePath :: IO FilePath
packagesFilePath = (</> "patches" </> "packages.json") <$> getAppUserDataDirectory "etlas"

buildPackage :: Text -> IO ()
buildPackage pkg = sh $ procExitOnError "etlas" ["install", pkg] empty

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
  procExitOnError "javac" [pack verifyScriptCmd] mempty
  echo "Verify.class built successfully."
  echo "Compiling a simple program..."
  echo "=== Eta Compiler Output ==="
  exists <- testdir (fromString outPath)
  when (not exists) $ mkdir (fromString outPath)
  procExitOnError "eta" ["-fforce-recomp", "-o", pack outJar, pack mainSource] mempty
  echo "===                     ==="
  echo "Compiled succesfully."
  echo "Verifying the bytecode of compiled program..."
  echo "=== Verify Script Output ==="
  procExitOnError "java" ["-cp", pack verifyScriptPath, "Verify", pack outJar] mempty
  echo "===                      ==="
  echo "Bytecode looking good."
  echo "Running the simple program..."
  echo "=== Simple Program Output ==="
  procExitOnError "java" ["-cp", pack outJar, "eta.main"] mempty
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
    Just pkg' ->
        let packages = (patched pkg') <> (vanilla pkg')
        in mapM_ buildPackage packages
