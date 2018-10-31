{-# LANGUAGE MultiParamTypeClasses, DataKinds, MagicHash, KindSignatures, FlexibleInstances,
             FlexibleContexts #-}

module Eta.Dot where

import GHC.Exts
import GHC.TypeLits

class Dot t (s :: Symbol) r where
  dot :: Proxy# s -> t -> r

class DotSmash a b c where
  dotSmash :: a -> b -> c

instance {-# OVERLAPS #-}
  DotSmash a (a -> b) b where
  dotSmash x f = f x

instance DotSmash (b -> c) (a -> b) (a -> c)  where
  dotSmash = (.)

instance {-# OVERLAPPING #-}
  (Functor f) => DotSmash (f a) (a -> b) (f b) where
  dotSmash fa ab = fmap ab fa

instance (Applicative f) => DotSmash (f (a -> b)) (f a) (f b) where
  dotSmash = (<*>)

instance {-# OVERLAPPING #-} (Monad m) => DotSmash (m a) (a -> m b) (m b) where
  dotSmash = (>>=)
