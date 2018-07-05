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
  removeDirectoryRecursive buildRootDir
  createDirectoryIfMissing True buildRootDir
  exists <- doesFileExist emptyFile
  when (not exists) $ BS.writeFile emptyFile mempty
  suites <- createTestSuites rootDir
  defaultMain (testGroup "Eta Golden Tests" suites)

createTestSuites :: FilePath -> IO [TestTree]
createTestSuites rootDir = do
  contents <- getDirectoryContents rootDir
  suitePaths <- fmap (map (\d -> rootDir </> d)) $
                filterM (\d -> doesDirectoryExist (rootDir </> d)) contents
  forM suitePaths $ \suitePath -> do
    let suiteName = takeFileName suitePath
        pat = compile "*.hs"
        compilePath = suitePath </> "compile"
        failPath    = suitePath </> "fail"
        runPath     = suitePath </> "run"
    compileFiles <- globDir1 pat compilePath
    failFiles    <- globDir1 pat failPath
    runFiles     <- globDir1 pat runPath
    return $ testGroup suiteName
        [ testGroup "compile"
            [ goldenVsFile testName emptyFile targetFile
                           (etaAction CompileMode builddir compileFile targetFile)
            | compileFile <- compileFiles
            , let testName   = takeBaseName compileFile
                  builddir   = buildDir suiteName "compile" testName
                  targetFile = builddir </> (testName <.> "stderr") ]

        , testGroup "fail"
            [ goldenVsFile testName goldenFile targetFile
                           (etaAction FailMode builddir failFile targetFile)
            | failFile <- failFiles
            , let goldenFile = replaceExtension failFile "stderr"
                  testName   = takeBaseName failFile
                  builddir   = buildDir suiteName "fail" testName
                  targetFile = builddir </> (testName <.> "stderr") ]

        , testGroup "run"
            [ goldenVsFile testName goldenFile targetFile
                           (etaAction RunMode builddir runFile targetFile)
            | runFile <- runFiles
            , let goldenFile = replaceExtension runFile "stdout"
                  testName   = takeBaseName runFile
                  builddir   = buildDir suiteName "run" testName
                  targetFile = builddir </> (testName <.> "stdout") ]
        ]

data ActionMode = CompileMode
                | FailMode
                | RunMode

etaAction :: ActionMode -> FilePath -> FilePath -> FilePath -> IO ()
etaAction mode builddir srcFile outputFile = do
  createDirectoryIfMissing True builddir
  let (specificOptions, expectedExitCode, shouldRun) = case mode of
        CompileMode -> (["-staticlib"],
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
      outJar = builddir </> "Out.jar"
      procConfig = proc "eta" $ ["--make"] ++ specificOptions ++ genericOptions
                             ++ ["-outputdir", builddir, "-cp", mkClassPath defaultClassPath]
                             ++ ["-o", outJar]
                             ++ [srcFile]
  (exitCode, stdout, stderr) <- readProcess procConfig
  let getOutput
        | shouldRun = do
          (exitCode, stdout, stderr) <- readProcess $
              proc "java" ["-ea", "-classpath", mkClassPath (outJar : defaultClassPath),
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
  ["-v0",
   "-g0",
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

emptyFile :: FilePath
emptyFile = buildRootDir </> "empty"

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
