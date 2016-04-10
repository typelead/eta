import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

rtsDir = "rts"

rtsBuildDir = rtsDir </> "build"

rtsIncludeDir = rtsDir </> "include"

rtsSrcDir = rtsDir </> "src"

build x = rtsBuildDir </> x

debug x = liftIO $ print x

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [build "rts.jar"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build"
      removeFilesAfter rtsBuildDir ["//*"]

    build "rts.jar" %> \out -> do
      cs <- getDirectoryFiles rtsSrcDir ["//*.java"]
      let os = [build c | c <- cs]
      headers <- getDirectoryFiles "" [rtsIncludeDir </> "*.h"]
      need $ os ++ headers
      () <- cmd "javac" os
      cmd "jar cf" [out] os

    "rts/build//*.java" %> \out -> do
      -- debug $ "1: " ++ out
      let input = rtsSrcDir </> (dropDirectory1 . dropDirectory1 $ out)
      -- debug $ "2: " ++ input
      need [input]
      Stdout output <- cmd Shell "cpp -iquote" rtsIncludeDir input "| sed -e" "s/#.*//" "-e" "/^$/d"
      writeFile' out output
