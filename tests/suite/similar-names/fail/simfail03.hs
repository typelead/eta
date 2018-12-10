{-# LANGUAGE TypeFamilies #-}

data family A a

data instance A Int    = Baa Int
data instance A Char   = BAa Char
data instance A Double = BAA Double

data AB = BAA

main = return ()
