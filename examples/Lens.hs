-- Name: Lens Example
-- Description: Shows an example of how to use lens.
import Control.Lens
import Data.Char

main :: IO ()
main = do
  print $ ("hello","world")^._2
  print $ set _2 42 ("hello","world")
  print $ ("hello",("world","!!!"))^._2._1
  print $ set (_2._1) 42 ("hello",("world","!!!"))
  print $ "hello"^.to length
  print $ ("hello",("world","!!!"))^._2._2.to length
  print $ _1 .~ "hello" $ ((),"world")
  print $ ((), "world") & _1 .~ "hello"
  print $ view _2 (10,20)
  print $ over mapped succ [1,2,3]
  print $ over (mapped._2) succ [(1,2),(3,4)]
  print $ _1.mapped._2.mapped %~ succ $ ([(42, "hello")],"world")
  print $ both *~ 2 $ (1,2)
  res <- mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
  print res
