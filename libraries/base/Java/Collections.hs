{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             FlexibleContexts, AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Collections
-- Copyright   :  (c) Rahul Muttineni 2016-2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Interfacing with the Java Collections API easily.
--
-----------------------------------------------------------------------------

module Java.Collections
  ( Iterator
  , consume
  , iterator
  , hasNext
  , next )
where

import GHC.Base
import Java.Core

data {-# CLASS "java.util.Iterator" #-} Iterator a =
  Iterator (Object# (Iterator a))
  deriving Class

foreign import java unsafe "@interface hasNext"
  hasNext :: (Extends a Object) => Java (Iterator a) Bool

foreign import java unsafe "@interface next"
  next :: (Extends a Object) => Java (Iterator a) a

consume :: (Extends a Object) => Iterator a -> [a]
consume it = pureJavaWith it (go id)
  where go acc = do
          continue <- hasNext
          if continue
          then do
            e <- next
            go (acc . (e:))
          else return (acc [])

data {-# CLASS "java.lang.Iterable" #-} Iterable a =
  Iterable (Object# (Iterable a))
  deriving Class

foreign import java unsafe "@interface iterator"
  iterator :: (Extends a Object, Extends b (Iterable a)) => Java b (Iterator a)
