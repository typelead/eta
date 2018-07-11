{-# LANGUAGE TypeOperators, MagicHash, FlexibleContexts #-}
import Java

main :: IO ()
main = do
  example <- newE
  print $ fib 10
  print $ fibI 10
  print =<< fib2 11
  print =<< fibI2 11
  print =<< java (fib3 12)
  print =<< java (fibI3 12)
  print =<< java (example <.> fib4 13)
  print =<< java (example <.> fibI4 13)

data {-# CLASS "eta.example.Example" #-} Example = Example (Object# Example)
  deriving Class

intList :: Int -> Java Example (List JInteger)
intList = undefined

intList2 :: (b <: Object, a <: List b) => a -> List JInteger -> Java Example (List b)
intList2 = undefined

intList3 :: Int -> Java Example Bool
intList3 = undefined

foreign export java intList :: Int -> Java Example (List JInteger)

foreign export java intList2 :: (b <: Object, a <: List b) => a -> List JInteger -> Java Example (List b)

foreign export java intList3 :: Int -> Java Example Bool

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2 :: Int -> IO Int
fib2 = return . fib

fib3 :: Int -> Java a Int
fib3 = return . fib

fib4 :: Int -> Java Example Int
fib4 = return . fib

foreign export java "@static eta.example.Ex.fib" fib :: Int -> Int
foreign export java "@static eta.example.Ex.fib2" fib2 :: Int -> IO Int
foreign export java "@static eta.example.Ex.fib3" fib3 :: Int -> Java a Int
foreign export java "fib4" fib4 :: Int -> Java Example Int

foreign import java "@static eta.example.Ex.fib" fibI :: Int -> Int
foreign import java "@static eta.example.Ex.fib2" fibI2 :: Int -> IO Int
foreign import java "@static eta.example.Ex.fib3" fibI3 :: Int -> Java a Int
foreign import java unsafe "@new" newE :: IO Example
foreign import java "fib4" fibI4 :: Int -> Java Example Int
