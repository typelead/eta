module GHCVM.Util
  (indexList,
   upperFirst,
   scanM,
   expectJust,
   safeHead,
   safeLast)
where

import qualified Data.Char as C
import Data.Text (Text, empty, uncons, cons)
import GHCVM.Utils.Maybes(expectJust)

indexList :: (Integral b) => [a] -> [(b, a)]
indexList = zip [1..]

upperFirst :: Text -> Text
upperFirst str = case uncons str of
  Nothing -> empty
  Just (c, str') -> cons (C.toUpper c) str'

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeLast :: [a] -> Maybe a
safeLast xs = if null xs then Nothing else Just $ last xs
