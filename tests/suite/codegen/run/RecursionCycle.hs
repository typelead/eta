module Main where

main :: IO ()
main = do
        putStrLn $ show $ recursive <$> arr
        where
            arr = [0..10]


recursive' 0 a b  = a
recursive' n a b = recursive'  (n-1)  b a

recursive n = recursive' n  0 1
