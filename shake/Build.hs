import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

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

-- TODO: Make the build script cleaner

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [rtsjar, masjar]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build"
      removeFilesAfter rtsBuildDir ["//*"]

    masjar %> \out -> do
      cs <- getDirectoryFiles "" [mapandsumDir </> "java/src//*.java"]
      need [rtsjar]
      () <- cmd "javac" "-cp" (".:" ++ rtsjar)  "-d" sampleBuildDir cs
      classfiles <- getDirectoryFiles sampleBuildDir ["//*.class"]
      () <- cmd (Cwd sampleBuildDir) "jar cf" ["../../" ++ out] classfiles
      putNormal "Generated mapandsum.jar for execution."

    rtsjar %> \out -> do
      cs <- getDirectoryFiles rtsSrcDir ["//*.java"]
      let os = [build c | c <- cs]
      headers <- getDirectoryFiles "" [rtsIncludeDir </> "*.h"]
      need $ os ++ headers
      -- The flag suppresses the warnings about Unsafe
      --() <- cmd "javac -XDignore.symbol.file -Xlint:unchecked" os
      () <- cmd "javac -XDignore.symbol.file" os
      classfiles <- getDirectoryFiles rtsBuildDir ["//*.class"]
      () <- cmd (Cwd rtsBuildDir) "jar cf" ["../../" ++ out] classfiles
      putNormal "Generated rts.jar."

    "rts/build//*.java" %> \out -> do
      -- debug $ "1: " ++ out
      let input = rtsSrcDir </> (dropDirectory1 . dropDirectory1 $ out)
      -- debug $ "2: " ++ input
      need [input]
      Stdout output <- cmd Shell "cpp -iquote" rtsIncludeDir input "| sed -e" "s/#.*//" "-e" "/^$/d"
      writeFile' out output
