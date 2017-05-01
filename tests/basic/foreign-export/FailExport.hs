module FailExport where

import Java

foreign export java fib :: Int -> Java a Int

fib n = return $ fib' n

fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)
