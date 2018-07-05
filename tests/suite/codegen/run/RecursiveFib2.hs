module Main where

main :: IO ()
main = do
        putStrLn $ show $ fibonacci <$> arr
        where
            arr = [0..10]

fibrecursive 0 presum sum  = sum
fibrecursive n presum sum = fibrecursive  (n-1) sum (sum+presum)

fibonacci n = fibrecursive n 0 1