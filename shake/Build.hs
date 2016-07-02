{-# GHC_OPTIONS -XNoOverloadedStrings #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory(createDirectoryIfMissing)

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
rtsjar = build "rts.jar"
masjar = sampleBuild "mapandsum.jar"
top x = "../../" ++ x

createDirIfMissing = liftIO . createDirectoryIfMissing True

-- TODO: Make the build script cleaner

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [rtsjar, masjar]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build & sample/build"
      removeFilesAfter rtsBuildDir ["//*"]
      removeFilesAfter sampleBuildDir ["//*"]

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
