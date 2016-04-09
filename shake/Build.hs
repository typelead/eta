import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory (copyFile)

rtsDir = "rts"

rtsBuildDir = rtsDir </> "build"

build x = rtsBuildDir </> x

debug x = liftIO $ print x

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=rtsBuildDir} $ do
    want [build "rts.jar"]

    phony "clean" $ do
      putNormal "Cleaning files in rts/build"
      removeFilesAfter rtsBuildDir ["//*"]

    build "rts.jar" %> \out -> do
      cs <- getDirectoryFiles rtsDir ["*.java"]
      let os = [build (takeDirectory1 c) | c <- cs]
      need os
      () <- cmd "javac" os
      cmd "jar cf" [out] os

    build "*.java" %> \out -> do
      let input = rtsDir </> (dropDirectory1 . dropDirectory1 $ out)
      need [input]
      liftIO $ copyFile input out
      cmd "sed -i -e" "s/CLOSURE_PTR/long/g" out
