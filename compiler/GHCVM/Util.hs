module GHCVM.Util
  (indexList,
   upperFirst,
   scanM,
   concatMapM,
   expectJust,
   safeHead)
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

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where f x xs = do
            x' <- op x
            if null x' then xs
            else do xs' <- xs
                    return $ x' ++ xs'

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing
