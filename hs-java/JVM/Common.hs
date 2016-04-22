{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- | This module declares some commonly used functions and instances.
module JVM.Common
  (toCharList,
  (!),
  showListIx,
  getChar8,
  getInt,
  toString,
  putString,
  getToSemicolon,
  whileJust,
  mapFindIndex,
  byteString
  ) where

import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Codec.Binary.UTF8.String hiding (encode, decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map as M
import Data.Default
import Data.List
import Data.Char

instance Default B.ByteString where
  def = B.empty

toCharList :: B.ByteString -> [Int]
toCharList bstr = map fromIntegral $ B.unpack bstr

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

-- | Read one-byte Char
getChar8 :: Get Char
getChar8 = do
  x <- getWord8
  return $ chr (fromIntegral x)

toString :: B.ByteString -> String
toString bstr = decodeString $ BC.unpack bstr

-- | Try to read integer value from decimal representation
getInt :: Get (Maybe Int)
getInt = do
    s <- getDigits
    if null s
      then return Nothing
      else return $ Just (read s)
  where
    getDigits :: Get String
    getDigits = do
      c <- lookAhead getChar8
      if isDigit c
        then do
             skip 1
             next <- getDigits
             return (c: next)
        else return []

putString :: String -> Put
putString str = forM_ str put

-- | Read string up to `;'
getToSemicolon :: Get String
getToSemicolon = do
  x <- get
  if x == ';'
    then return []
    else do
         next <- getToSemicolon
         return (x: next)

whileJust :: (Monad m) => m (Maybe a) -> m [a]
whileJust m = do
  r <- m
  case r of
    Just x -> do
              next <- whileJust m
              return (x: next)
    Nothing -> return []
