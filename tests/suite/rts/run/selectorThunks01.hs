import Control.Exception

main :: IO ()
main = do
  a <- getChar
  print a
  let str = replicate 10000000 a
      insertb xs = before ++ "b" ++ after
        where (before, after) = break (== 'b') xs
      force (x:xs) = x `seq` force xs
      force [] = ()

  evaluate (force (insertb str))
