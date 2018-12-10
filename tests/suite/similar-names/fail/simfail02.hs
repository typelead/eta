{-# LANGUAGE TypeFamilies #-}

data family A a

data instance A Int = Ba Int

data AB = BA

main = return ()
