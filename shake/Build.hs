{-# GHC_OPTIONS -XNoOverloadedStrings #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory(createDirectoryIfMissing, getAppUserDataDirectory, createDirectory, removeDirectory)
import Control.Monad(forM_, when)
import Data.List (partition)
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
                     , libraryDirs = [rootDir </> lib] }
      writeFile' confDst (showInstalledPackageInfo ipi')
      return ()
    ParseFailed err -> case locatedErrorMsg err of
                         (Nothing, s) -> putNormal s
                         (Just l, s) -> putNormal $ show l ++ ": " ++ s

buildLibrary :: String -> [String] -> Action ()
buildLibrary lib deps = do
  rootDir <- getGhcVmRoot
  installDir <- getInstallDir
  let libDir = libraryDir </> lib
      rootLibDir = rootDir </> lib
      conf = lib <.> "conf"
      libConf = libDir </> conf
      libBuildDir = libDir </> "build"
      libBuildConf = libBuildDir </> conf
      packageDir = packageConfDir rootDir
  createDir libBuildDir
  if lib == "rts" then
    need [rtsjar]
  else do
    hsFiles <- getDirectoryFiles libDir ["//*.hs"]
    unit $ cmd [Cwd libDir, AddEnv "GHC_PACKAGE_PATH" packageDir]
               (installDir </> "ghcvm") "-clear-package-db -v"
               ["-package " ++ dep | dep <- deps]
               "-staticlib -ddump-to-file -ddump-stg -this-package-key"
               lib "-o" ("build" </> libName lib)  "-outputdir build" hsFiles
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

-- TODO: Make the build script cleaner
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [rtsjar]

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
        unit $ cmd "stack install ghcvm"
        libs <- getLibs
        let sortedLibs = topologicalDepsSort libs getDependencies
        forM_ sortedLibs $ \lib ->
          buildLibrary lib (getDependencies lib)

    phony "uninstall" $ do
      rootDir <- getGhcVmRoot
      putNormal "Cleaning files in ~/.ghcvm"
      removeFilesAfter rootDir ["//*"]

    phony "reinstall" $ do
      need ["uninstall"]
      need ["install"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build, sample/build "
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
      unit $ cmd (Cwd rtsSrcDir) "javac -g -XDignore.symbol.file" "-d" (top rtsBuildDir) cs
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      unit $ cmd (Cwd rtsBuildDir) "jar cf" (top out) classfiles
