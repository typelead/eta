-- Name: Java Generics
-- Description: Shows how to interface with Java APIs that use Generics.
-- These language extensions are currently required to support
-- Java Generics.
{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds #-}

-- This imports all the standard library functionality that helps
-- you deal with importing Java methods into Eta.
import Java
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

-- The following a declarations of Java wrapper types. These types let you
-- interact directly with the corresponding Java objects.
-- This will not be the final syntax for Java wrapper types, see:
-- https://github.com/typelead/eta/issues/140
data {-# CLASS "java.util.Collection" #-} Collection a =
  Collection (Object# (Collection a))
  deriving Class

data {-# CLASS "java.util.List" #-} List a =
  List (Object# (List a))
  deriving Class

-- The `Inherits` type family specifies parent classes and interfaces
-- so that the Eta typechecker can statically check inheritance relationships.
type instance Inherits (List a) = '[Collection a]

data {-# CLASS "java.util.ArrayList" #-} ArrayList a =
  ArrayList (Object# (ArrayList a))
  deriving Class

type instance Inherits (ArrayList a) = '[List a]

data {-# CLASS "java.lang.Integer" #-} JInteger = JInteger (Object# JInteger)
  deriving Class

foreign import java unsafe "@new" newInteger :: Int -> JInteger
foreign import java unsafe "intValue" intValue :: JInteger -> Int
foreign import java unsafe "@new" newArrayList :: Java c (ArrayList a)

-- The `Extends` multi-parameter typeclass checks whether the first type
-- is a descendant of the second. This static check is facilitated by
-- the `Inherits` type family above.
foreign import java unsafe "add" add ::
  (Extends a Object, Extends b (Collection a)) => a -> Java b Bool
foreign import java unsafe "get" get ::
  (Extends a Object, Extends b (List a)) => Int -> Java b a
