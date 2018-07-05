module Main where

main :: IO ()
main = do
        putStrLn $ show $ fibonacci <$> arr
        where
            arr = [0..10]

fibrecursive 0 sum presum  = sum
fibrecursive n sum presum = fibrecursive  (n-1) (sum+presum) sum

fibonacci n = fibrecursive n 1 0