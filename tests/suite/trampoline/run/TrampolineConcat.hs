import Data.List
import Data.Function

main :: IO ()
main = print $ trampoline $ concat (replicate 1000000 []) ++ ["Hello"]
