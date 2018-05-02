{-# OPTIONS_GHC -XNoOverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Development.Shake
import Development.Shake.FilePath

import Distribution.InstalledPackageInfo
import Distribution.ParseUtils
import Distribution.ModuleName (fromString)

import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory,
                         createDirectory)

-- * Standard file/directory paths
rtsDir, libraryDir, rtsBuildDir, rtsSrcDir, rtsjar :: FilePath
rtsDir = "rts"
libraryDir = "libraries"
rtsBuildDir = rtsDir </> "build"
rtsSrcDir = rtsDir </> "src" </> "main" </> "java"
rtsjar = libJarPath "rts"

library, genBuild, top, packageConfDir, libCustomBuildDir, libJarPath,
  libName :: String -> FilePath
library x = libraryDir </> x
genBuild x = x </> "build"
top x = "../../../../" ++ x
top2 x = "../../" ++ x
packageConfDir dir = dir </> "package.conf.d"
libCustomBuildDir lib = libraryDir </> lib </> "build"
libName lib = lib ++ ".jar"
libJarPath lib = libCustomBuildDir lib </> libName lib

getEtlasDir, getEtaRoot, getEtlasLibDir :: Action FilePath
getEtlasDir = liftIO $ getAppUserDataDirectory "etlas"
getEtaRoot  = do
  Stdout actualOutput <- cmd "eta" "--print-libdir"
  return . head $ lines actualOutput
getEtlasLibDir = do
  etlasDir <- getEtlasDir
  etaVersion <- getEtaNumericVersion
  let etlasLibDir = etlasDir </> "lib"
      etaWithVersion = "eta-" ++ etaVersion
  findFileOrDir etaWithVersion etlasLibDir

findFileOrDir :: String -> FilePath -> Action FilePath
findFileOrDir pat dir = do
  results <- getDirectoryContents dir
  case filter (pat `isInfixOf`) results of
    (found:_) -> return (dir </> found)
    _         -> fail $ "Pattern not found '" ++ pat ++ "' in " ++ dir

getEtaNumericVersion :: Action String
getEtaNumericVersion = do
  Stdout actualOutput <- cmd "eta" "--numeric-version"
  return . head $ lines actualOutput

-- * Utility functions for filepath handling in the Action monad
createDirIfMissing :: FilePath -> Action ()
createDirIfMissing = liftIO . createDirectoryIfMissing True

createDir :: FilePath -> Action ()
createDir path = liftIO $ createDirectoryIfMissing True path

-- * Dependency Handling
-- TODO: Read the .cabal files for dependencies?
getDependencies :: String -> [String]
getDependencies "ghc-prim" = ["rts"]
getDependencies "base" = ["ghc-prim", "integer"]
getDependencies "integer" = ["ghc-prim"]
getDependencies "eta-repl" = ["eta-boot", "base", "eta-meta"]
getDependencies "eta-boot" = ["eta-boot-th", "base"]
getDependencies "eta-boot-th" = ["base"]
getDependencies "eta-meta" = ["base", "eta-boot-th"]
getDependencies _ = []

ignoreList :: [String]
ignoreList = []

topologicalDepsSort :: [String] -> (String -> [String]) -> [String]
topologicalDepsSort xs deps = sort' xs []
 where sort' [] ys = reverse ys
       sort' xs' ys = sort' xs2 (xs1 ++ ys)
         where (xs1, xs2) = partition (all (`elem` ys) . deps) xs'

-- * Building boot libraries

getLibs :: Action [String]
getLibs = fmap (\\ ignoreList) $ getDirectoryDirs libraryDir

nonNullString :: String -> [String]
nonNullString str
  | null str = []
  | otherwise = [str]

buildLibrary :: Bool -> (String -> String) -> String -> [String] -> Action ()
buildLibrary debug binPathArg lib _deps = do
  let dir = library lib
      installFlags = ["--allow-boot-library-installs"]
                  ++ nonNullString (binPathArg "../../")
  when (lib == "rts") $ need [rtsjar]
  unit $ cmd (Cwd dir) "etlas install" installFlags
  return ()

-- * Testing utilities
testSpec :: FilePath -> Action ()
testSpec specPath = do
  rootDir <- getEtaRoot
  specStr <- readFile' specPath
  let (command', output') = break (== '\n') specStr
      expectedOutput     = drop 1 output'
      testHome           = takeDirectory specPath
      packageDir         = packageConfDir rootDir
      testBuildDir       = genBuild testHome
  createDir testBuildDir
  unit $ cmd [Cwd testHome, AddEnv "ETA_PACKAGE_PATH" packageDir]
             "eta -shared" ["-outputdir", "build"] ["-o", jarTestFile] command' mainTestFile

  let classPathsAll = jarTestFile : map libJar ["base", "rts", "ghc-prim", "integer"]
      libJar lib = rootDir </> lib </> (lib ++ ".jar")
      classPathFolded = intercalate ":" classPathsAll

  Stdout actualOutput <- cmd (Cwd testHome) "java" ["-classpath", classPathFolded] "eta.main"
  --removeFilesAfter testBuildDir ["//*"]
  if expectedOutput == actualOutput then
    putNormal $ "Test " ++ specPath ++ " passed."
  else do
    putNormal $ "Test " ++ specPath ++ " failed."
    putNormal $ "Actual:"
    putNormal $ actualOutput
    putNormal $ "Expected:"
    putNormal $ expectedOutput
  where mainTestFile = fileName -<.> "hs"
        jarTestFile  = "build" </> (fileName -<.> "jar")
        fileName     = takeBaseName specPath


-- * Command line flags

data BuildFlags = BuildDebugInfo
                | BuildBinaries { binFilePath :: FilePath }
                deriving Eq

flags :: [OptDescr (Either String BuildFlags)]
flags =
  [
    Option "" ["debuginfo"] (NoArg $ Right $ BuildDebugInfo) "Run with debugging information."
  , Option "" ["binaries"]  (ReqArg (Right . BuildBinaries) "DIR") "Build binaries for the base libraries"
  ]

-- * The main build script
main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles=rtsBuildDir} flags $ \flags' targets -> return $ Just $ do

    if null targets
      then want [rtsjar]
      else want targets

    let debug = BuildDebugInfo `elem` flags'
        binaryOutputPath = fmap binFilePath
                         $ find (\case
                                    BuildBinaries _ -> True
                                    _ -> False)
                         $ reverse flags'
        binPathArg s = maybe "" (\p -> "--binaries-output-dir="
                                    ++ (if isRelative p then s else "")
                                    ++ p)
                       binaryOutputPath

    phony "install" $ do
      rootDir <- getEtaRoot
      exists <- doesDirectoryExist rootDir
      if exists then
        putNormal $ "eta already installed. To perform a clean install,\n"
                 ++ "run 'eta-build uninstall' followed by 'eta-build"
                 ++ " install'."
      else do
        liftIO $ createDirectory rootDir
        Stdout result <- cmd ["git", "rev-parse", "HEAD"]
        liftIO $ writeFile (rootDir </> "commit-hash") result
        unit $ cmd ["eta-pkg","init",packageConfDir rootDir]
        unit $ cmd "etlas update"
        unit $ cmd "etlas select local"
        etlasDir <- getEtlasDir
        let etlasToolsDir = etlasDir </> "tools"
        createDirIfMissing etlasToolsDir
        copyFile' "utils/coursier/coursier" $ etlasToolsDir </> "coursier"
        let verifyExt ext = "utils/class-verifier/Verify" <.> ext
        unit $ cmd ["javac", verifyExt "java"]
        createDirIfMissing (etlasToolsDir </> "classes")
        copyFile' (verifyExt "class") $ etlasToolsDir </> "classes" </> "Verify.class"
        libs <- getLibs
        let sortedLibs = topologicalDepsSort libs getDependencies
        forM_ sortedLibs $ \lib ->
          buildLibrary debug binPathArg lib (getDependencies lib)
        -- unit $ cmd $ ["etlas", "install", "template-haskell-2.11.1.0", "--allow-boot-library-installs"] ++ nonNullString (binPathArg "")

    phony "rts-clean" $ do
      liftIO $ removeFiles (libCustomBuildDir "rts") ["//*"]
      need [rtsjar]
      buildLibrary debug binPathArg "rts" []

    phony "test" $ do
      specs <- getDirectoryFiles "" ["//*.spec"]
      mapM_ testSpec specs

    phony "testclean" $ do
      specs <- getDirectoryFiles "" ["//*.spec"]
      mapM_ (\spec -> removeFilesAfter (takeDirectory spec </> "build") ["//*"]) specs

    phony "uninstall" $ do
      rootDir <- getEtaRoot
      createDirIfMissing rootDir
      putNormal $ "Cleaning files in " ++ rootDir
      removeFilesAfter rootDir ["//*"]

    phony "reinstall" $ do
      need ["uninstall"]
      need ["install"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build, libraries/*/dist"
      removeFilesAfter rtsBuildDir ["//*"]
      libs <- getLibs
      forM_ libs $ \lib -> do
        let libDir = libraryDir </> lib
        unit $ cmd (Cwd libDir) "etlas clean"

    rtsjar %> \out -> do
      createDirIfMissing rtsBuildDir
      cs <- getDirectoryFiles rtsSrcDir ["//*.java"]
      -- TODO: Setup a debug build
      let javacFlags = (if debug then ["-g"] else []) ++ ["-Xlint:-options", "-source", "1.7", "-target", "1.7"]
      unit $ cmd (Cwd rtsSrcDir) "javac" "-XDignore.symbol.file" javacFlags "-d" (top rtsBuildDir) cs
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      unit $ cmd (Cwd rtsBuildDir) "jar cf" (top2 out) classfiles
