module GHCVM.Util
  (indexList,
   upperFirst)
where

import qualified Data.Char as C
import Data.Text (Text, empty, uncons, cons)

indexList :: (Integral b) => [a] -> [(b, a)]
indexList = zip [1..]

upperFirst :: Text -> Text
upperFirst str = case uncons str of
  Nothing -> empty
  Just (c, str') -> cons (C.toUpper c) str'

