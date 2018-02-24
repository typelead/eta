{-# LANGUAGE NoImplicitPrelude, MagicHash, MultiParamTypeClasses,
             FlexibleContexts, DataKinds, AllowAmbiguousTypes,
             TypeFamilies, ScopedTypeVariables, FlexibleInstances,
             Rank2Types, TypeOperators #-}
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
  ( Collection
  , add
  , List
  , Iterator
  , hasNext
  , next
  , Iterable
  , iterator
  , Enumeration
  , hasMoreElements
  , nextElement
  , Dictionary
  , Map
  , Set(..)
  , Properties )
where

import GHC.Base
import GHC.List
import GHC.Read
import Text.Read (read)
import GHC.Show
import Java.Core
import Java.StringBase
import Java.Utils

-- Collection
data {-# CLASS "java.util.Collection" #-} Collection a =
  Collection (Object# (Collection a))
  deriving Class

type instance Inherits (Collection a) = '[Iterable a]

foreign import java unsafe "@interface add" add ::
  (Extends a Object, Extends b (Collection a)) => a -> Java b Bool

toCollection :: (Extends a Object, Extends b (Collection a))
             => (forall c. Int -> Java c b) -> [a] -> b
toCollection f xs = unsafePerformJava $ do
  coll <- f (length xs)
  _ <- withObject coll $ mapM add xs
  return coll

instance Extends a Object => JavaConverter [a] (Collection a) where
  toJava   = superCast . toArrayList
  fromJava = consumeGen . superCast

-- List
data {-# CLASS "java.util.List" #-} List a =
  List (Object# (List a))
  deriving Class

type instance Inherits (List a) = '[Collection a]

instance Extends a Object => JavaConverter [a] (List a) where
  toJava   = superCast . toArrayList
  fromJava = consumeGen . superCast

-- ArrayList
data {-# CLASS "java.util.ArrayList" #-} ArrayList a =
  ArrayList (Object# (ArrayList a))
  deriving Class

type instance Inherits (ArrayList a) = '[Object, List a]

foreign import java unsafe "@new" newArrayList
  :: Extends a Object => Int -> Java b (ArrayList a)

toArrayList :: (Extends a Object) => [a] -> ArrayList a
toArrayList = toCollection newArrayList

-- Iterator
data {-# CLASS "java.util.Iterator" #-} Iterator a =
  Iterator (Object# (Iterator a))
  deriving Class

foreign import java unsafe "@interface hasNext"
  hasNext :: (Extends a Object) => Java (Iterator a) Bool

foreign import java unsafe "@interface next"
  next :: (Extends a Object) => Java (Iterator a) a

consumeItr :: (Extends a Object) => Iterator a -> [a]
consumeItr it = unsafePerformJavaWith it (go id)
  where go acc = do
          continue <- hasNext
          if continue
          then do
            e <- next
            go (acc . (e:))
          else return (acc [])

produceItr :: forall a. (Extends a Object) => [a] -> Iterator a
produceItr xs = unsafePerformJavaWith (superCast (toArrayList xs) :: Iterable a) iterator

instance Extends a Object => JavaConverter [a] (Iterator a) where
  toJava   = produceItr
  fromJava = consumeItr

-- Iterable
data {-# CLASS "java.lang.Iterable" #-} Iterable a =
  Iterable (Object# (Iterable a))
  deriving Class

foreign import java unsafe "@interface iterator"
  iterator :: (Extends a Object, Extends b (Iterable a)) => Java b (Iterator a)

consumeGen :: (Extends a Object) => Iterable a -> [a]
consumeGen it = unsafePerformJava $ do
  itr <- it <.> iterator
  return $ consumeItr itr

instance Extends a Object => JavaConverter [a] (Iterable a) where
  toJava   = superCast . toArrayList
  fromJava = consumeGen . superCast

-- Enumeration
data {-# CLASS "java.util.Enumeration" #-} Enumeration a =
  Enumeration (Object# (Enumeration a))
  deriving Class

foreign import java unsafe "@interface hasMoreElements"
  hasMoreElements :: (Extends a Object) => Java (Enumeration a) Bool

foreign import java unsafe "@interface nextElement"
  nextElement :: (Extends a Object) => Java (Enumeration a) a

consumeEnum :: (Extends a Object) => Enumeration a -> [a]
consumeEnum it = unsafePerformJavaWith it (go id)
  where go acc = do
          continue <- hasMoreElements
          if continue
          then do
            e <- nextElement
            go (acc . (e:))
          else return (acc [])

instance Extends a Object => JavaConverter [a] (Enumeration a) where
  toJava xs = unsafePerformJavaWith (toVector xs) elements
  fromJava  = consumeEnum

-- Vector
data {-# CLASS "java.util.Vector" #-} Vector a = Vector (Object# (Vector a))
  deriving Class

type instance Inherits (Vector a) = '[Object, List a]

foreign import java unsafe "@new" newVector
  :: Extends a Object => Int -> Java b (Vector a)

foreign import java unsafe elements
  :: Extends a Object => Java (Vector a) (Enumeration a)

toVector :: Extends a Object => [a] -> Vector a
toVector = toCollection newVector

-- Dictionary
data {-# CLASS "java.util.Dictionary" #-} Dictionary k v =
  Dictionary (Object# (Dictionary k v))
  deriving Class

foreign import java unsafe keys
  :: (Extends k Object, Extends v Object)
  => Java (Dictionary k v) (Enumeration k)

foreign import java unsafe get
  :: (Extends k Object, Extends v Object)
  => Object -> Java (Dictionary k v) v

fromDictionary :: (Extends k Object, Extends v Object)
               => Dictionary k v -> [(k, v)]
fromDictionary dict = unsafePerformJavaWith dict $ do
  ks <- keys
  flip mapM (fromJava ks) $ \k -> do
    v <- get (superCast k)
    return (k, v)

instance (Extends k Object, Extends v Object)
  => JavaConverter [(k, v)] (Dictionary k v) where
  toJava   = superCast . toHashtable
  fromJava = fromDictionary

-- Hashtable
data {-# CLASS "java.util.Hashtable" #-} Hashtable k v =
  Hashtable (Object# (Hashtable k v))
  deriving Class

type instance Inherits (Hashtable k v) = '[Dictionary k v, Map k v]

foreign import java unsafe "@new" newHashtable
  :: (Extends k Object, Extends v Object) => Int -> Java a (Hashtable k v)

toHashtable :: (Extends k Object, Extends v Object) => [(k, v)] -> Hashtable k v
toHashtable elems = unsafePerformJava $ do
  ht <- newHashtable (length elems)
  _ <- withObject ht $
    flip mapM elems $ \(k, v) ->
      put k v
  return ht

-- Map.Entry
data {-# CLASS "java.util.Map$Entry" #-} MapEntry k v =
  MapEntry (Object# (MapEntry k v))
  deriving Class

foreign import java unsafe "@interface getKey" getKey
  :: (Extends k Object, Extends v Object)
  => Java (MapEntry k v) k

foreign import java unsafe "@interface getValue" getValue
  :: (Extends k Object, Extends v Object)
  => Java (MapEntry k v) v

fromMap :: forall k v. (Extends k Object, Extends v Object) => Map k v -> [(k, v)]
fromMap m = unsafePerformJavaWith m $ do
  (set :: Set (MapEntry k v)) <- entrySet
  flip mapM (fromJava set) $ \me -> do
    withObject me $ do
      key <- getKey
      val <- getValue
      return (key, val)

instance (Extends k Object, Extends v Object)
  => JavaConverter [(k, v)] (Map k v) where
  toJava   = superCast . toHashMap
  fromJava = fromMap

data {-# CLASS "java.util.HashMap" #-} HashMap k v =
  HashMap (Object# (HashMap k v))
  deriving Class

type instance Inherits (HashMap k v) = '[Object, Map k v]

foreign import java unsafe "@new" newHashMap
  :: (Extends k Object, Extends v Object) => Int -> Java a (HashMap k v)

toHashMap :: (Extends k Object, Extends v Object) => [(k, v)] -> HashMap k v
toHashMap elems = unsafePerformJava $ do
  ht <- newHashMap (length elems)
  _ <- withObject ht $
    flip mapM elems $ \(k, v) ->
      put k v
  return ht

-- Set
data {-# CLASS "java.util.Set" #-} Set a = Set (Object# (Set a))
  deriving Class

type instance Inherits (Set a) = '[Collection a, Iterable a]

data {-# CLASS "java.util.HashSet" #-} HashSet a = HashSet (Object# (HashSet a))
  deriving Class

foreign import java unsafe "@new" newHashSet
  :: Extends a Object => Int -> Java b (HashSet a)

toHashSet :: Extends a Object => [a] -> HashSet a
toHashSet = toCollection newHashSet

type instance Inherits (HashSet a) = '[Object, Set a]

instance Extends a Object => JavaConverter [a] (Set a) where
  toJava   = superCast . toHashSet
  fromJava = consumeGen . superCast

-- Properties
data {-# CLASS "java.util.Properties" #-} Properties =
  Properties (Object# Properties)
  deriving Class

type instance Inherits Properties = '[Object, Map Object Object]

foreign import java unsafe "@new" newProperties :: Java a Properties

foreign import java unsafe stringPropertyNames
  :: Java Properties (Set JString)

foreign import java unsafe getProperty
  :: JString -> Java Properties String

foreign import java unsafe setProperty
  :: String -> String -> Java Properties Object

toProperties :: [(String, String)] -> Properties
toProperties props = unsafePerformJava $ do
  props' <- newProperties
  _ <- withObject props' $ do
    flip mapM props $ \(key, val) ->
      setProperty key val
  return props'

fromProperties :: Properties -> [(String, String)]
fromProperties props = unsafePerformJavaWith props $ do
  properties <- stringPropertyNames
  flip mapM (fromJava properties) $ \key -> do
    val <- getProperty key
    return (fromJava key, val)

instance JavaConverter [(String, String)] Properties where
  toJava   = toProperties
  fromJava = fromProperties

-- Start java.util.Map

data {-# CLASS "java.util.Map" #-} Map k v =
  Map (Object# (Map k v))
  deriving Class

foreign import java unsafe "@interface" clear :: (b <: Map k v, k <: Object, v <: Object) => Java b ()

foreign import java unsafe "@interface" containsKey :: (b <: Map k v,k <: Object, v <: Object)
                                       => Object -> Java b Bool

foreign import java unsafe "@interface" containsValue :: (b <: Map k v, k <: Object, v <: Object)
                                         => Object -> Java b Bool

foreign import java unsafe "@interface get"
  getMap :: (b <: Map k v, k <: Object, v <: Object) => Java b v

foreign import java unsafe "@interface"
  isEmpty :: (b <: Map k v, k <: Object, v <: Object) => Java b Bool

foreign import java unsafe "@interface"
  keySet :: (b <: Map k v, k <: Object, v <: Object) => Java b (Set k)

foreign import java unsafe "@interface"
  putAll :: (b <: Map k v, k <: Object, v <: Object, a <: k, c <: v) => Map a c -> Java b ()

foreign import java unsafe "@interface"
  remove :: (b <: Map k v, k <: Object, v <: Object) => Object -> Java b v

foreign import java unsafe "@interface"
  size :: (b <: Map k v, k <: Object, v <: Object) => Java b Int

foreign import java unsafe "@interface"
  values :: (b <: Map k v, k <: Object, v <: Object) => Java b (Collection v)

foreign import java unsafe "@interface put" put
  :: (Extends k Object, Extends v Object, Extends b (Map k v))
  => k -> v -> Java b v

foreign import java unsafe "@interface entrySet" entrySet
  :: (Extends k Object, Extends v Object, Extends b (Map k v))
  => Java b (Set (MapEntry k v))

-- End java.util.Map

-- Start java.util.SortedMap

data {-# CLASS "java.util.SortedMap" #-} SortedMap k v =
  SortedMap (Object# (SortedMap k v))
  deriving Class

foreign import java unsafe "@interface" comparator :: (k <: Object, v <: Object, b <: (SortedMap k v), k <: a)
  => Java b (Comparator a)

foreign import java unsafe "@interface entrySet" entrySetSM :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => Java b (Set (MapEntry k v))

foreign import java unsafe "@interface" firstKey :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => Java b k

foreign import java unsafe "@interface" headMap :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => k -> Java b (SortedMap k v)

foreign import java unsafe "@interface keySet" keySetSM :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => Java b (Set k)

foreign import java unsafe "@interface" lastKey :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => Java b k

foreign import java unsafe "@interface" subMap :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => k -> k -> Java b (SortedMap k v)

foreign import java unsafe "@interface" tailMap :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => k -> Java b (SortedMap k v)

foreign import java unsafe "@interface values" valuesSM :: (k <: Object, v <: Object, b <: (SortedMap k v))
  => k -> Java b (Collection v)

-- End java.util.SortedMap
