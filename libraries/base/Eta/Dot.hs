{-# LANGUAGE MultiParamTypeClasses, DataKinds, MagicHash, KindSignatures, FlexibleInstances,
             FlexibleContexts #-}

module Eta.Dot where

import GHC.Base
import GHC.Exts
import GHC.TypeLits

class Dot t (s :: Symbol) r where
  dot :: t -> Proxy# s -> r

class DotSmash a b c where
  dotSmash :: a -> b -> c

instance DotSmash a (a -> b) b where
  dotSmash x f = f x

instance (Functor f) => DotSmash (f a) (a -> b) (f b) where
  dotSmash fa ab = fmap ab fa

instance (Applicative f) => DotSmash (f a) (f (a -> b)) (f b) where
  dotSmash = (<**>)

instance (Monad m) => DotSmash (m a) (a -> m b) (m b) where
  dotSmash = (>>=)
