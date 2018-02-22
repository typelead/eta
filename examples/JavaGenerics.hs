-- Name: Java Generics
-- Description: Shows how to interface with Java APIs that use Generics.
-- These language extensions are currently required to support
-- Java Generics.
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

-- This imports all the standard library functionality that helps
-- you deal with importing Java methods into Eta. We are hiding certain classes
-- because they are already defined in the standard library
import Java hiding (JInteger, Collection, List, add)
import Control.Monad

main :: IO ()
main = java $ do
  list <- newArrayList
  list <.> populateArray 10

populateArray :: Int -> Java (ArrayList JInteger) ()
populateArray n = do
  forM_ range $ \i ->
    add (newInteger i)
  forM_ range $ \i -> do
    jint <- get i
    io $ print $ intValue jint * 5
  where range = [0..n]

data Collection a = Collection (@java.util.Collection a)
  deriving Class

data List a = List (@java.util.List a)
  deriving Class

-- The `Inherits` type family specifies parent classes and interfaces
-- so that the Eta typechecker can statically check inheritance relationships.
type instance Inherits (List a) = '[Collection a]

data ArrayList a = ArrayList (@java.util.ArrayList a)
  deriving Class

type instance Inherits (ArrayList a) = '[List a]

data JInteger = JInteger @java.lang.Integer
  deriving Class

foreign import java unsafe "@new" newInteger :: Int -> JInteger
foreign import java unsafe "intValue" intValue :: JInteger -> Int
foreign import java unsafe "@new" newArrayList :: Java c (ArrayList a)

-- The `Extends` multi-parameter typeclass checks whether the first type
-- is a descendant of the second. This static check is facilitated by
-- the `Inherits` type family above.
foreign import java unsafe "add" add ::
  (a <: Object, b <: (Collection a)) => a -> Java b Bool
foreign import java unsafe "get" get ::
  (a <: Object, b <: (List a)) => Int -> Java b a
