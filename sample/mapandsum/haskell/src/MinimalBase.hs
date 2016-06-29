{-# LANGUAGE NoImplicitPrelude, ForeignFunctionInterface, MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes #-}
module MinimalBase
  (printInt,
   Int,
   (+),
   enumFromTo,
   one,
   zero,
   ten,
   thousand)
where

import Prelude  (($), otherwise, undefined)
import GHC.IO   (IO(..))
import GHC.Base (Int#, State#, RealWorld, isTrue#, (>#), (==#), (+#))

foreign import prim "print_printIntzh" printInt# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

data Int = I# Int#

(+) :: Int -> Int -> Int
(+) (I# i1) (I# i2) = I# (i1 +# i2)

enumFromTo :: Int -> Int -> [Int]
enumFromTo (I# x) (I# y) = eftInt x y

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

printInt :: Int -> IO ()
printInt (I# b) = IO $ \s -> case printInt# b s of (# s1, _ #) -> (# s1, () #)

one, zero, ten, thousand :: Int
zero = I# 0#
one = I# 1#
ten = I# 10#
thousand = I# 1000#
