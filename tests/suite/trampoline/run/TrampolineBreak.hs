import Data.List
import Data.Function

main :: IO ()
main = do
  let input = replicate 1000000 'c' ++ "defghi"
  print $ trampoline $ snd $ break (== 'd') input
