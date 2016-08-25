{-# GHC_OPTIONS -XNoOverloadedStrings #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory(createDirectoryIfMissing, getAppUserDataDirectory, createDirectory, removeDirectory)
import System.Console.GetOpt
import Control.Monad(forM_, when)
import Data.List (partition,stripPrefix)
import Data.Maybe(mapMaybe)
import Distribution.InstalledPackageInfo
import Distribution.ParseUtils

rtsDir = "rts"
rtsBuildDir = rtsDir </> "build"
rtsIncludeDir = rtsDir </> "include"
rtsSrcDir = rtsDir </> "src"
sampleDir = "sample"
mapandsumDir = sampleDir </> "mapandsum"
sampleBuildDir = sampleDir </> "build"
build x = rtsBuildDir </> x
debug x = liftIO $ print x
sampleBuild x = sampleBuildDir </> x
rtsjar = libJarPath "rts"
masjar = sampleBuild "mapandsum.jar"
top x = "../../" ++ x
packageConfDir dir = dir </> "package.conf.d"

ghcvmIncludePath :: FilePath -> FilePath
ghcvmIncludePath = (</> "include")

getInstallDir :: Action FilePath
getInstallDir = fmap (</> "bin") $ liftIO $ getAppUserDataDirectory "local"

getGhcVmRoot :: Action FilePath
getGhcVmRoot = liftIO $ getAppUserDataDirectory "ghcvm"

libraryDir = "libraries"
library x = libraryDir </> x

createDirIfMissing = liftIO . createDirectoryIfMissing True

getDependencies :: String -> [String]
getDependencies "ghc-prim" = ["rts"]
getDependencies "base" = ["ghc-prim", "integer"]
getDependencies "integer" = ["ghc-prim"]
getDependencies _ = []

topologicalDepsSort :: [String] -> (String -> [String]) -> [String]
topologicalDepsSort xs deps = sort' xs []
 where sort' [] ys = reverse ys
       sort' xs ys = sort' xs2 (xs1 ++ ys)
         where (xs1, xs2) = partition (all (`elem` ys) . deps) xs

libName :: String -> String
libName lib = "HS" ++ lib ++ ".jar"

libJarPath :: String -> FilePath
libJarPath lib = libraryDir </> lib </> "build" </> libName lib

buildConf :: String -> FilePath -> FilePath -> Action ()
buildConf lib confSrc confDst = do
  rootDir <- getGhcVmRoot
  confStr <- readFile' confSrc
  case parseInstalledPackageInfo confStr of
    ParseOk warnings ipi -> do
      mapM_ (putNormal . showPWarning confSrc) warnings
      let ipi' = ipi { hsLibraries = ["HS" ++ lib]
                     , pkgRoot = Just rootDir
                     , importDirs = [rootDir </> lib]
                     , libraryDirs = [rootDir </> lib]
                     , includeDirs = if lib == "rts" then
                                       [ghcvmIncludePath rootDir]
                                     else
                                       [] }
      writeFile' confDst $ showInstalledPackageInfo ipi'
    ParseFailed err -> case locatedErrorMsg err of
                         (Nothing, s) -> putNormal s
                         (Just l, s) -> putNormal $ show l ++ ": " ++ s

buildLibrary :: Bool -> String -> [String] -> Action ()
buildLibrary debug lib deps = do
  rootDir <- getGhcVmRoot
  installDir <- getInstallDir
  let libDir = libraryDir </> lib
      rootLibDir = rootDir </> lib
      conf = lib <.> "conf"
      libConf = libDir </> conf
      libBuildDir = libDir </> "build"
      libBuildConf = libBuildDir </> conf
      packageDir = packageConfDir rootDir
      ghcvmCmd = installDir </> "ghcvm"
  createDir libBuildDir
  if lib == "rts" then
    need [rtsjar]
  else do
    sourceFiles <- getDirectoryFiles libDir ["//*.hs", "//.java"]
    let ghcvmFlags = (if debug
                     then ["-v", "-ddump-to-file", "-ddump-stg", "-ddump-tc-trace"]
                     else [])
                     ++
                     (if lib == "base"
                      then ["-Iinclude"]
                      else [])
    unit $ cmd [Cwd libDir, AddEnv "GHCVM_PACKAGE_PATH" packageDir]
               ghcvmCmd "-clear-package-db" ghcvmFlags
               ["-package " ++ dep | dep <- deps]
               "-staticlib -this-package-key"
               lib "-o" ("build" </> libName lib)  "-outputdir build" sourceFiles
  buildConf lib libConf libBuildConf
  buildFiles <- getDirectoryFiles libBuildDir ["//*"]

  forM_ buildFiles $ \buildFile -> do
    let src = libBuildDir </> buildFile
        dst = rootLibDir </> buildFile
    copyFileWithDir src dst
  unit $ cmd "stack exec -- ghc-pkg" ["--package-db", packageConfDir rootDir] "--force register" libBuildConf
  return ()

createDir :: FilePath -> Action ()
createDir path = liftIO $ createDirectoryIfMissing True path

copyFileWithDir :: FilePath -> FilePath -> Action ()
copyFileWithDir src dst = do
  createDir (takeDirectory src)
  createDir (takeDirectory dst)
  copyFile' src dst

getLibs :: Action [String]
getLibs = getDirectoryDirs libraryDir

dropDirectoryN :: Int -> FilePath -> FilePath
dropDirectoryN n = head . drop n . iterate dropDirectory1

flags = [Option "" ["debuginfo"] (NoArg $ Right True) "Run with debugging information."]

-- TODO: Make the build script cleaner
main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles=rtsBuildDir} flags $ \flags targets -> return $ Just $ do
    if null targets
      then want [rtsjar]
      else want targets
    let debug = case flags of
          (x:_) -> True
          _     -> False
    phony "install" $ do
      rootDir <- getGhcVmRoot
      exists <- doesDirectoryExist rootDir
      if exists then
        putNormal $ "ghcvm already installed. To perform a clean install,\n"
                 ++ "run 'ghcvm-build uninstall' followed by 'ghcvm-build"
                 ++ " install'."
      else do
        liftIO $ createDirectory rootDir
        let root x = rootDir </> x
        unit $ cmd "stack exec -- ghc-pkg init " $ packageConfDir rootDir
        Stdout paths <- cmd "stack path"
        let binPath = head . mapMaybe (stripPrefix "compiler-bin: ") $ lines paths
            ghcPath = takeDirectory binPath
            ghcInclude = ghcPath </> "lib" </> "ghc-7.10.3" </> "include"
            ghcvmInclude = ghcvmIncludePath rootDir
        liftIO $ createDirectory ghcvmInclude
        let root x = rootDir </> x
        forM_ ["platform", "version"] $ \s -> do
          let s' = "ghc" ++ s ++ ".h"
          copyFile' (ghcInclude </> s') (ghcvmInclude </> s')
        libs <- getLibs
        let sortedLibs = topologicalDepsSort libs getDependencies
        forM_ sortedLibs $ \lib ->
          buildLibrary debug lib (getDependencies lib)

    phony "uninstall" $ do
      rootDir <- getGhcVmRoot
      putNormal "Cleaning files in ~/.ghcvm"
      removeFilesAfter rootDir ["//*"]

    phony "reinstall" $ do
      need ["uninstall"]
      need ["install"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build, sample/build"
      removeFilesAfter rtsBuildDir ["//*"]
      removeFilesAfter sampleBuildDir ["//*"]
      libs <- getLibs
      forM_ libs $ \lib -> do
        let libBuildDir = libraryDir </> lib </> "build"
        removeFilesAfter libBuildDir ["//*"]

    masjar %> \out -> do
      createDirIfMissing sampleBuildDir
      cs <- getDirectoryFiles mapandsumDir ["java/src//*.java"]
      need [rtsjar]
      -- TODO: Setup a debug build
      unit $ cmd (Cwd mapandsumDir) "javac" "-g" "-cp" (top rtsjar) "-d" (top sampleBuildDir) cs
      classfiles <- getDirectoryFiles sampleBuildDir ["//*.class"]
      unit $ cmd (Cwd sampleBuildDir) "jar cf" (top out) classfiles
      putNormal "Generated mapandsum.jar."

    rtsjar %> \out -> do
      createDirIfMissing rtsBuildDir
      cs <- getDirectoryFiles rtsSrcDir ["//*.java"]
      -- TODO: Setup a debug build
      let javacFlags = if debug then ["-g"] else []
      unit $ cmd (Cwd rtsSrcDir) "javac" "-XDignore.symbol.file" javacFlags "-d" (top rtsBuildDir) cs
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      unit $ cmd (Cwd rtsBuildDir) "jar cf" (top out) classfiles
