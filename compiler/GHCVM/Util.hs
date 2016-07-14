module GHCVM.Util where

indexList :: (Integral b) => [a] -> [(b, a)]
indexList = zip [1..]
