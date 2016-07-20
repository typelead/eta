{-# GHC_OPTIONS -XNoOverloadedStrings #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory(createDirectoryIfMissing, getAppUserDataDirectory, createDirectory, removeDirectory)
import Control.Monad(forM_)
import Data.List (partition)

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
rtsjar = build "HSrts.jar"
masjar = sampleBuild "mapandsum.jar"
top x = "../../" ++ x
packageConfDir dir = dir </> "package.conf.d"

getGhcVmRoot :: Action FilePath
getGhcVmRoot = liftIO $ getAppUserDataDirectory "ghcvm"

libraryDir = "libraries"
library x = libraryDir </> x

createDirIfMissing = liftIO . createDirectoryIfMissing True

getDependencies :: String -> [String]
getDependencies "ghc-prim" = []
getDependencies "base" = ["ghc-prim"]
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

buildLibrary :: String -> [String] -> Action ()
buildLibrary lib deps = do
  rootDir <- getGhcVmRoot
  let libDir = libraryDir </> lib
  hsFiles <- getDirectoryFiles libDir ["//*.hs"]
  () <- cmd (Cwd libDir) "stack exec -- ghcvm -clear-package-db" ["-package " ++ dep | dep <- deps] "-staticlib -this-package-key" lib "-o" ("build" </> libName lib)  "-outputdir build" hsFiles

  let rootLibDir = rootDir </> lib
      conf = lib <.> "conf"
      libDir = libraryDir </> lib
      libConf = libDir </> conf
      libBuildDir = libDir </> "build"
  buildFiles <- getDirectoryFiles libBuildDir ["//*"]

  forM_ buildFiles $ \buildFile ->
    copyFile' (libBuildDir </> buildFile) (rootLibDir </> buildFile)
  () <- cmd "stack exec -- ghc-pkg" ["--package-db", packageConfDir rootDir] "--force register" libConf
  copyFile' libConf (rootLibDir </> "package.conf.d" </> conf)
  return ()

dropDirectoryN :: Int -> FilePath -> FilePath
dropDirectoryN n = head . drop n . iterate dropDirectory1

-- TODO: Make the build script cleaner
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [rtsjar, masjar]

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
        () <- cmd "stack exec -- ghc-pkg init " $ packageConfDir rootDir
        libs <- getDirectoryDirs libraryDir
        let libPaths = map libJarPath libs
        need $ topologicalDepsSort libPaths getDependencies
        -- Copy over all the files
        -- forM_ libs $ \lib -> do
        --    let rootLibDir = rootDir </> lib
        --        conf = lib <.> "conf"
        --        libDir = libraryDir </> lib
        --        libConf = libDir </> conf
        --        libBuildDir = libDir </> "build"
        --    buildFiles <- getDirectoryFiles libBuildDir ["//*"]

        --    forM_ buildFiles $ \buildFile ->
        --      copyFile' (libBuildDir </> buildFile) (rootLibDir </> buildFile)
        --    copyFile' libConf (rootLibDir </> "package.conf.d" </> conf)

      -- Install an initialize ghc-pkg
    phony "uninstall" $ do
      rootDir <- getGhcVmRoot
      putNormal "Cleaning files in rts/build & sample/build"
      removeFilesAfter rootDir ["//*"]

    phony "reinstall" $ do
      need ["uninstall"]
      need ["install"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build & sample/build"
      removeFilesAfter rtsBuildDir ["//*"]
      removeFilesAfter sampleBuildDir ["//*"]

    libJarPath "*" %> \out -> do
      let lib = takeDirectory1 . dropDirectory1 $ out
          deps = getDependencies lib
      buildLibrary lib deps

    masjar %> \out -> do
      createDirIfMissing sampleBuildDir
      cs <- getDirectoryFiles mapandsumDir ["java/src//*.java"]
      need [rtsjar]
      -- TODO: Setup a debug build
      () <- cmd (Cwd mapandsumDir) "javac" "-g" "-cp" (top rtsjar) "-d" (top sampleBuildDir) cs
      classfiles <- getDirectoryFiles sampleBuildDir ["//*.class"]
      () <- cmd (Cwd sampleBuildDir) "jar cf" (top out) classfiles
      putNormal "Generated mapandsum.jar."

    rtsjar %> \out -> do
      createDirIfMissing rtsBuildDir
      cs <- getDirectoryFiles rtsSrcDir ["//*.java"]
      -- TODO: Setup a debug build
      () <- cmd (Cwd rtsSrcDir) "javac -g -XDignore.symbol.file" "-d" (top rtsBuildDir) cs
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      () <- cmd (Cwd rtsBuildDir) "jar cf" (top out) classfiles
      putNormal "Generated rts.jar."
