import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory (copyFile)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Macros = M.Map String String

rtsDir = "rts"

rtsBuildDir = rtsDir </> "build"

build x = rtsBuildDir </> x

debug x = liftIO $ print x

defaultMacros :: Macros
defaultMacros = M.singleton "CLOSURE_PTR" "long"

validMacroIdChars = "A-Za-z_"

allMacros :: Macros -> Macros
allMacros initMap = M.insert "REF_CLOSURE_PTR" (rEF_CLOSURE_PTR cLOSURE_PTR) initMap
  where
    cLOSURE_PTR = fromJust (M.lookup "CLOSURE_PTR" initMap)
    rEF_CLOSURE_PTR "long" = "LongPtr"
    rEF_CLOSURE_PTR "int" = "IntPtr"

subs :: [String]
subs = concatMap (\expr -> ["-e", expr]) sedExprs
  where
    sedExprs = M.foldrWithKey (\k v accum -> (genSedExpr k v) : accum) [] $ allMacros defaultMacros
    genSedExpr before after = "s/[^" ++ validMacroIdChars ++ "]" ++ before ++  " /" ++ after ++ " /g"

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
      cmd "sed -i" subs out
