import Java
import java "java.lang.Math"

main :: IO ()
main =
 java $ do
   d <- Math.sqrt 25.0
   io $ print d
