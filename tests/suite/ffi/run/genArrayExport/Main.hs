{-# LANGUAGE ForeignFunctionInterface #-}

foreign import java unsafe "@static Utils.test" test :: IO ()

main :: IO ()
main = test
