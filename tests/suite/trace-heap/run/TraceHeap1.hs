import Data.Char
import Debug.Trace
import Data.Function

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  let transformedString = map (traceHeapId . toUpper) "Hello World!"
  traceHeapIdIO $ transformedString
  print $ take 3 transformedString
  traceHeapIdIO $ transformedString
  print $ transformedString
  traceHeapIdIO $ transformedString
  traceHeapIdIO fibs
  print $ (fibs !! 2)
  traceHeapIdIO fibs
  print $ (fibs !! 3)
  traceHeapIdIO fibs
  print $ (fibs !! 4)
  traceHeapIdIO fibs
  print $ (fibs !! 20)
  traceHeapIdIO fibs
  let recList = fix $ \x -> [ (x !! 1) + 1, 1, (x !! 0) + 2]
  traceHeapIdIO recList
  print recList
  traceHeapIdIO recList
  return ()
