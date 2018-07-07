{-# LANGUAGE LambdaCase, CPP #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.FilePath
import System.Directory
import System.FilePath.Glob
import System.Process.Typed
import System.IO.Unsafe
import System.Exit
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC

import Test.Tasty
import Test.Tasty.Golden as G

main :: IO ()
main = do
  exists <- doesDirectoryExist buildRootDir
  when exists $ removeDirectoryRecursive buildRootDir
  suites <- createTestSuites rootDir
  defaultMain (testGroup "Eta Golden Tests" suites)

createTestSuites :: FilePath -> IO [TestTree]
createTestSuites rootDir = do
  contents <- getDirectoryContents rootDir
  suitePaths <- fmap (map (\d -> rootDir </> d)) $
                filterM (\d -> doesDirectoryExist (rootDir </> d)) contents
  forM suitePaths $ \suitePath -> do
    let suiteName = takeFileName suitePath
        isBackpack = "backpack" `isPrefixOf` suiteName
        (pattern, actionMode)
          | isBackpack = ("*.bkp", BackpackAction)
          | otherwise = ("*.hs", CompileAction)
        genTestGroup mode name ext = do
          let path = suitePath </> name
          testFiles <- globDir1 (compile pattern) path
          forM testFiles $ \testFile -> do
            let testName   = takeBaseName testFile
                builddir   = buildDir suiteName name testName
                emptyFile  = buildDir suiteName name "_empty"
                targetFile = builddir </> (testName <.> ext)
                maybeGoldenFile = testFile -<.> ext
            exists <- doesFileExist maybeGoldenFile
            let (goldenFile, mGoldenFile)
                  | exists    = (maybeGoldenFile, Nothing)
                  | otherwise = (emptyFile, Just emptyFile)

            return $ goldenVsFileDiff testName
                       (\ref new -> ["diff", "-u", ref, new]) goldenFile targetFile
                       (etaAction mode mGoldenFile builddir testFile targetFile)

    compileGroup <- genTestGroup (actionMode CompileMode) "compile" "stderr"
    failGroup    <- genTestGroup (actionMode FailMode)    "fail"    "stderr"
    runGroup     <- genTestGroup (actionMode RunMode)     "run"     "stdout"
    return $ testGroup suiteName
        [ testGroup "compile" compileGroup
        , testGroup "fail"    failGroup
        , testGroup "run"     runGroup
        ]

data ActionMode = CompileAction  { actionResult :: ResultMode }  -- Invokes eta --make
                | BackpackAction { actionResult :: ResultMode }  -- Invoke eta --backpack

data ResultMode = CompileMode
                | FailMode
                | RunMode

etaAction :: ActionMode ->  Maybe FilePath -> FilePath -> FilePath -> FilePath -> IO ()
etaAction mode mGoldenFile builddir srcFile outputFile = do
  createDirectoryIfMissing True builddir
  maybe (return ()) (flip BS.writeFile mempty) mGoldenFile
  let (specificOptions, expectedExitCode, shouldRun) = case actionResult mode of
        CompileMode -> (if isBackpack then [] else ["-staticlib"],
                        \case ExitSuccess   -> True
                              ExitFailure _ -> False,
                        False)

        FailMode    -> (["-staticlib"],
                        \case ExitSuccess   -> False
                              ExitFailure _ -> True,
                        False)
        RunMode     -> (["-shared"],
                        \case ExitSuccess   -> True
                              ExitFailure _ -> False,
                        True)
      outFlags = ["-o", outJar]
      (isBackpack, outputOptions)
        | BackpackAction {} <- mode = (True, outFlags)
        | otherwise = (False, outFlags)
      (modeOptions, extraModeOptions) = case mode of
        CompileAction {}  -> (["--make"], ["-v0"])
        BackpackAction {} -> (["--backpack"], [])
      outJar = builddir </> "Out.jar"
      options = modeOptions ++ [srcFile] ++ specificOptions ++ extraModeOptions
                            ++ genericOptions
                            ++ ["-outputdir", builddir, "-cp", mkClassPath defaultClassPath]
                            ++ outputOptions

      procConfig = proc "eta" options
  (exitCode, stdout, stderr) <- readProcess procConfig
  let getOutput
        | shouldRun = do
          let getClasspath
                | isBackpack =
                  fmap (defaultClassPath ++) $ globDir1 (compile "**/*.jar") builddir
                | otherwise = return defaultClassPath
          classpath <- getClasspath
          (exitCode, stdout, stderr) <- readProcess $
              proc "java" ["-ea", "-classpath", mkClassPath (outJar : classpath),
                           "eta.main"]
          let output
                | not (expectedExitCode exitCode) = BC.pack (show exitCode) <> mainOutput
                | otherwise = mainOutput
              mainOutput = stdout <> stderr
          return output
        | otherwise =
          let mainOutput = stdout <> stderr
              output
                | not (expectedExitCode exitCode) = BC.pack (show exitCode) <> mainOutput
                | otherwise = mainOutput
          in return output
  getOutput >>= BS.writeFile outputFile

genericOptions :: [String]
genericOptions =
  ["-g0",
   "-fshow-source-paths",
   "-O",
   "-dcore-lint",
   "-fno-diagnostics-show-caret",
   "-fdiagnostics-color=never",
   "-fshow-warning-groups",
   "-dno-debug-output"]

buildRootDir :: FilePath
buildRootDir = "dist"

buildDir :: String -> String -> String -> FilePath
buildDir suiteName suiteType testName =
  buildRootDir </> suiteName </> suiteType </> testName

rootDir :: FilePath
rootDir = "tests/suite"

classPathSep :: String
#ifndef mingw32_HOST_OS
classPathSep = ":"
#else
classPathSep = ";"
#endif

mkClassPath :: [FilePath] -> String
mkClassPath = intercalate classPathSep

{-# NOINLINE defaultClassPath #-}
defaultClassPath :: [FilePath]
defaultClassPath = unsafePerformIO $ do
  res <- readProcessStdout_ $ proc "etlas" $ ["exec", "eta-pkg", "--", "list", "--simple-output"]
  let packages = map BC.unpack $ BC.split ' ' res
  forM packages $ \package -> do
    res' <- readProcessStdout_ $ proc "etlas" $
      ["exec", "eta-pkg", "--", "field", package, "library-dirs,hs-libraries", "--simple"]
    let (dir:file:_) = BC.lines res'
    return $ BC.unpack dir </> (BC.unpack file <.> "jar")
