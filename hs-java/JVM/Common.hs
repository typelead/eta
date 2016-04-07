{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- | This module declares some commonly used functions and instances.
module JVM.Common
  (toCharList,
  poolSize,
  (!),
  showListIx,
  mapFindIndex,
  byteString
  ) where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Default
import Data.List

import JVM.ClassFile

instance Default B.ByteString where
  def = B.empty

toCharList :: B.ByteString -> [Int]
toCharList bstr = map fromIntegral $ B.unpack bstr

poolSize :: Pool stage -> Int
poolSize = M.size

(!) :: (Ord k) => M.Map k a -> k -> a
(!) = (M.!)

showListIx :: (Show i, Show a) => [(i,a)] -> String
showListIx list = unlines $ map s list
  where s (i, x) = show i ++ ":\t" ++ show x

byteString ::  (Binary t) => t -> B.ByteString
byteString x = runPut (put x)

mapFindIndex :: (Num k) => (v -> Bool) -> M.Map k v -> Maybe k
mapFindIndex check m =
  case find (check . snd) (M.assocs m) of
    Nothing -> Nothing
    Just (k,_) -> Just k

