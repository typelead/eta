import Data.Char
import Debug.Trace

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
  return ()
