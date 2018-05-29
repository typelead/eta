{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module Eta.Utils.Maybes (
        module Data.Maybe,

        MaybeErr(..), -- Instance of Monad
        failME, isSuccess,

        orElse,
        firstJust, firstJusts,
        whenIsJust,
        expectJust,

        MaybeT(..), liftMaybeT, tryMaybeT
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Exception (catch, SomeException(..))

infixr 4 `orElse`

{-
************************************************************************
*                                                                      *
\subsection[Maybe type]{The @Maybe@ type}
*                                                                      *
************************************************************************
-}

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = firstJusts [a, b]

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = msum

expectJust :: String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)

whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) f = f x
whenIsJust Nothing  _ = return ()

-- | Flipped version of @fromMaybe@, useful for chaining.
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

{-
************************************************************************
*                                                                      *
\subsection[MaybeT type]{The @MaybeT@ monad transformer}
*                                                                      *
************************************************************************
-}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f x = MaybeT $ fmap (fmap f) $ runMaybeT x

instance (Monad m, Functor m) => Applicative (MaybeT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing

-- TODO:#if __GLASGOW_HASKELL__ < 710
-- -- Pre-AMP change
-- instance (Monad m, Functor m) => Alternative (MaybeT m) where
-- #else
instance (Monad m) => Alternative (MaybeT m) where
-- #endif
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (MaybeT m) where
  mzero       = MaybeT $ return Nothing
  p `mplus` q = MaybeT $ do ma <- runMaybeT p
                            case ma of
                              Just a  -> return (Just a)
                              Nothing -> runMaybeT q

liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT act = MaybeT $ Just `liftM` act

-- | Try performing an 'IO' action, failing on error.
tryMaybeT :: IO a -> MaybeT IO a
tryMaybeT action = MaybeT $ catch (Just `fmap` action) handler
  where
    handler (SomeException _) = return Nothing

{-
************************************************************************
*                                                                      *
\subsection[MaybeErr type]{The @MaybeErr@ type}
*                                                                      *
************************************************************************
-}

data MaybeErr err val = Succeeded val | Failed err

instance Functor (MaybeErr err) where
  fmap = liftM

instance Applicative (MaybeErr err) where
  pure  = return
  (<*>) = ap

instance Monad (MaybeErr err) where
  return v = Succeeded v
  Succeeded v >>= k = k v
  Failed e    >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME e = Failed e
