{-# OPTIONS_GHC -XNoOverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Development.Shake
-- import Development.Shake.Command
import Development.Shake.FilePath
-- import Development.Shake.Util

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
rtsSrcDir = rtsDir </> "src"
rtsjar = libJarPath "rts"

library, genBuild, top, packageConfDir, libCustomBuildDir, libJarPath,
  libName :: String -> FilePath
library x = libraryDir </> x
genBuild x = x </> "build"
top x = "../../" ++ x
packageConfDir dir = dir </> "package.conf.d"
libCustomBuildDir lib = libraryDir </> lib </> "build"
libName lib = "HS" ++ lib ++ ".jar"
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
getDependencies "eta-repl" = ["eta-boot", "base", "template-haskell"]
getDependencies "eta-boot" = ["eta-boot-th", "base"]
getDependencies "eta-boot-th" = ["base"]
getDependencies "template-haskell" = ["base", "eta-boot-th"]
getDependencies _ = []

ignoreList :: [String]
ignoreList = ["eta-boot", "eta-boot-th", "eta-repl"]

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
      configureFlags = if debug
                       then ["--enable-optimization=0"
                            ,"--eta-options=-ddump-to-file -ddump-stg -dumpdir=dump"]
                       else ["--enable-optimization=2"]

      -- libCmd = unit . cmd (Cwd dir)
  when (lib == "rts") $ need [rtsjar]
  unit $ cmd (Cwd dir) "etlas configure" configureFlags
  unit $ cmd (Cwd dir) "etlas install" installFlags
  when (lib == "ghc-prim") $ fixGhcPrimConf
  return ()

-- By default, GHC.Prim is NOT included, and we must manually add it in for
-- consistency.
fixGhcPrimConf :: Action ()
fixGhcPrimConf = do
  rootDir <- getEtaRoot
  let confDir = packageConfDir rootDir
  (ghcPrimConf':_) <- fmap (filter ("ghc-prim" `isPrefixOf`))
                    $ getDirectoryFiles confDir ["*.conf"]
  let ghcPrimConf = confDir </> ghcPrimConf'
  confStr <- readFile' ghcPrimConf
  case parseInstalledPackageInfo confStr of
    ParseOk warnings ipi -> do
      mapM_ (putNormal . showPWarning ghcPrimConf) warnings
      let ipi' = ipi { exposedModules = ExposedModule (fromString "GHC.Prim") Nothing
                                      : exposedModules ipi }
      writeFile' ghcPrimConf $ showInstalledPackageInfo ipi'
      unit $ cmd "eta-pkg recache"
    ParseFailed err -> case locatedErrorMsg err of
                         (Nothing, s) -> putNormal s
                         (Just l, s) -> putNormal $ show l ++ ": " ++ s

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
      libJar lib = rootDir </> lib </> ("HS" ++ lib ++ ".jar")
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
        unit $ cmd ["eta-pkg","init",packageConfDir rootDir]
        unit $ cmd "etlas update"
        etlasDir <- getEtlasDir
        copyFile' "utils/coursier/coursier" $ etlasDir </> "coursier"
        let verifyExt ext = "utils/class-verifier/Verify" <.> ext
        unit $ cmd ["javac", verifyExt "java"]
        copyFile' (verifyExt "class") $ etlasDir </> "Verify.class"
        libs <- getLibs
        let sortedLibs = topologicalDepsSort libs getDependencies
        forM_ sortedLibs $ \lib ->
          buildLibrary debug binPathArg lib (getDependencies lib)
        unit $ cmd $ ["etlas", "install", "template-haskell", "--allow-boot-library-installs"] ++ nonNullString (binPathArg "")

    phony "rts-clean" $ do
      liftIO $ removeFiles (libCustomBuildDir "rts") ["//*"]
      need [rtsjar]
      let libDir = libraryDir </> "rts"
      unit $ cmd (Cwd libDir) "etlas clean"
      unit $ cmd (Cwd libDir) "etlas build"
      etlasLibDir <- getEtlasLibDir
      sourceJar   <- findFileOrDir "HSrts-0.1.0.0" (libDir </> "dist" </> "build")
      destJar     <- findFileOrDir "rts-0.1.0.0" etlasLibDir
                 >>= findFileOrDir "HSrts-0.1.0.0"
      copyFile' sourceJar destJar

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
      let javacFlags = (if debug then ["-g"] else []) ++ ["-Xlint:unchecked"]
      unit $ cmd (Cwd rtsSrcDir) "javac" "-XDignore.symbol.file" javacFlags "-d" (top rtsBuildDir) cs
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      unit $ cmd (Cwd rtsBuildDir) "jar cf" (top out) classfiles
