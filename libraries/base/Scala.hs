{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Scala
-- Copyright   :  (c) Rahul Muttineni 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  rahulmutt@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Helpers for dealing with Scala APIs.
--
-- TODO: Move to a separate scala package later
--
-----------------------------------------------------------------------------

module Scala
  (Tuple2, Tuple3, Tuple4, Tuple5)
where

import GHC.Base
import GHC.Show
import Java

data {-# CLASS "scala.Tuple2" #-} Tuple2 a b =
  Tuple2 (Object# (Tuple2 a b))
  deriving (Class, Show, Eq)

foreign import java unsafe "@new" toTuple2
  :: (a <: Object, b <: Object) => a -> b -> Tuple2 a b

foreign import java unsafe "_1" tup2_1 :: (a <: Object) => Tuple2 a b -> a

foreign import java unsafe "_2" tup2_2 :: (b <: Object) => Tuple2 a b -> b

instance (Extends a Object, Extends b Object)
  => JavaConverter (a, b) (Tuple2 a b) where
  toJava (a, b) = toTuple2 a b
  fromJava t = (tup2_1 t, tup2_2 t)

data {-# CLASS "scala.Tuple3" #-} Tuple3 a b c =
  Tuple3 (Object# (Tuple3 a b c))
  deriving (Class, Show, Eq)

foreign import java unsafe "@new" toTuple3
  :: (a <: Object, b <: Object, c <: Object) => a -> b -> c -> Tuple3 a b c

foreign import java unsafe "_1" tup3_1 :: (a <: Object) => Tuple3 a b c -> a

foreign import java unsafe "_2" tup3_2 :: (b <: Object) => Tuple3 a b c -> b

foreign import java unsafe "_3" tup3_3 :: (c <: Object) => Tuple3 a b c -> c

instance (a <: Object, b <: Object, c <: Object)
  => JavaConverter (a, b, c) (Tuple3 a b c) where
  toJava (a, b, c) = toTuple3 a b c
  fromJava t = (tup3_1 t, tup3_2 t, tup3_3 t)

data {-# CLASS "scala.Tuple4" #-} Tuple4 a b c d =
  Tuple4 (Object# (Tuple4 a b c d))
  deriving (Class, Show, Eq)

foreign import java unsafe "@new" toTuple4
  :: (a <: Object, b <: Object, c <: Object, d <: Object)
  => a -> b -> c -> d -> Tuple4 a b c d

foreign import java unsafe "_1" tup4_1 :: (a <: Object) => Tuple4 a b c d -> a

foreign import java unsafe "_2" tup4_2 :: (b <: Object) => Tuple4 a b c d -> b

foreign import java unsafe "_3" tup4_3 :: (c <: Object) => Tuple4 a b c d -> c

foreign import java unsafe "_4" tup4_4 :: (d <: Object) => Tuple4 a b c d -> d

instance (a <: Object, b <: Object, c <: Object, d <: Object)
  => JavaConverter (a, b, c, d) (Tuple4 a b c d) where
  toJava (a, b, c, d) = toTuple4 a b c d
  fromJava t = (tup4_1 t, tup4_2 t, tup4_3 t, tup4_4 t)

data {-# CLASS "scala.Tuple5" #-} Tuple5 a b c d e =
  Tuple5 (Object# (Tuple5 a b c d e))
  deriving (Class, Show, Eq)

foreign import java unsafe "@new" toTuple5
  :: (a <: Object, b <: Object, c <: Object, d <: Object, e <: Object)
  => a -> b -> c -> d -> e -> Tuple5 a b c d e

foreign import java unsafe "_1" tup5_1 :: (a <: Object) => Tuple5 a b c d e -> a

foreign import java unsafe "_2" tup5_2 :: (b <: Object) => Tuple5 a b c d e -> b

foreign import java unsafe "_3" tup5_3 :: (c <: Object) => Tuple5 a b c d e -> c

foreign import java unsafe "_4" tup5_4 :: (d <: Object) => Tuple5 a b c d e -> d

foreign import java unsafe "_5" tup5_5 :: (e <: Object) => Tuple5 a b c d e -> e

instance (a <: Object, b <: Object, c <: Object, d <: Object, e <: Object)
  => JavaConverter (a, b, c, d, e) (Tuple5 a b c d e) where
  toJava (a, b, c, d, e) = toTuple5 a b c d e
  fromJava t = (tup5_1 t, tup5_2 t, tup5_3 t, tup5_4 t, tup5_5 t)
