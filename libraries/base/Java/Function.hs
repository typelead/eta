{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Java.Function
-- Copyright   :  (c) Jyothsna Srinivas 2017
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  jyothsnasrinivas17@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Bindings for Java Date Time utilities
--
-----------------------------------------------------------------------------

module Java.Function where

import GHC.Base
import Data.Int

-- Start java.util.function.BiConsumer

data {-# CLASS "java.util.function.BiConsumer" #-} BiConsumer t u = BiConsumer (Object# (BiConsumer t u))
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkBiConsumer :: (t <: Object, u <: Object) => (t -> u -> Java (BiConsumer t u) ()) -> BiConsumer t u

-- End java.util.function.BiConsumer

-- Start java.util.function.BiFunction

data {-# CLASS "java.util.function.BiFunction" #-} BiFunction t u r = BiFunction (Object# (BiFunction t u r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkBiFunction :: (t <: Object, u <: Object, r <: Object)
  => (t -> u -> Java (BiFunction t u r) r) -> BiFunction t u r

-- End java.util.function.BiFunction

-- Start java.util.function.BinaryOperator

data {-# CLASS "java.util.function.BinaryOperator" #-} BinaryOperator t = BinaryOperator (Object# (BinaryOperator t))
  deriving Class

type instance Inherits (BinaryOperator t) = '[BiFunction t t t]

-- End java.util.function.BinaryOperator

-- Start java.util.function.BiPredicate

data {-# CLASS "java.util.function.BiPredicate" #-} BiPredicate t u = BiPredicate (Object# (BiPredicate t u))
  deriving Class

foreign import java unsafe "@wrapper test"
  mkBiPredicate :: (t <: Object, u <: Object) => (t -> u -> Java (BiPredicate t u) Bool) -> BiPredicate t u

-- End java.util.function.BiPredicate

-- Start java.util.function.BooleanSupplier

data {-# CLASS "java.util.function.BooleanSupplier" #-} BooleanSupplier = BooleanSupplier (Object# BooleanSupplier)
  deriving Class

foreign import java unsafe "@wrapper getAsBoolean"
  mkBooleanSupplier :: (Java BooleanSupplier Bool) -> BooleanSupplier

-- End java.util.function.BooleanSupplier

-- Start java.util.function.Consumer

data {-# CLASS "java.util.function.Consumer" #-} Consumer t = Consumer (Object# (Consumer t))
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkConsumer :: (t <: Object) => (t -> Java (Consumer t) ()) -> Consumer t

-- End java.util.function.Consumer

-- Start java.util.function.DoubleBinaryOperator

data {-# CLASS "java.util.function.DoubleBinaryOperator" #-} DoubleBinaryOperator = DoubleBinaryOperator (Object# DoubleBinaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsDouble"
  mkDoubleBinaryOperator :: (Double -> Double -> Java DoubleBinaryOperator Double) -> DoubleBinaryOperator

-- End java.util.function.DoubleBinaryOperator

-- Start java.util.function.DoubleConsumer

data {-# CLASS "java.util.function.DoubleConsumer" #-} DoubleConsumer = DoubleConsumer (Object# DoubleConsumer)
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkDoubleConsumer :: (Double -> Java DoubleConsumer ()) -> DoubleConsumer

-- End java.util.function.DoubleConsumer

-- Start java.util.function.DoubleFunction

data {-# CLASS "java.util.function.DoubleFunction" #-} DoubleFunction r = DoubleFunction (Object# (DoubleFunction r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkDoubleFunction :: (r <: Object) => (Double -> Java (DoubleFunction r) r) -> DoubleFunction r

-- End java.util.function.DoubleFunction

-- Start java.util.function.DoublePredicate

data {-# CLASS "java.util.function.DoublePredicate" #-} DoublePredicate = DoublePredicate (Object# DoublePredicate)
  deriving Class

foreign import java unsafe "@wrapper test"
  mkDoublePredicate :: (Double -> Java DoubleConsumer Bool) -> DoublePredicate

-- End java.util.function.DoublePredicate

-- Start java.util.function.DoubleSupplier

data {-# CLASS "java.util.function.DoubleSupplier" #-} DoubleSupplier = DoubleSupplier (Object# DoubleSupplier)
  deriving Class

foreign import java unsafe "@wrapper getAsDouble"
  mkDoubleSupplier :: (Double -> Java DoubleSupplier Double) -> DoubleSupplier

-- End java.util.function.DoubleSupplier

-- Start java.util.function.DoubleToIntFunction

data {-# CLASS "java.util.function.DoubleToIntFunction" #-} DoubleToIntFunction = DoubleToIntFunction (Object# DoubleToIntFunction)
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkDoubleToIntFunction :: (Double -> Java DoubleToIntFunction Int) -> DoubleToIntFunction

-- End java.util.function.DoubleToIntFunction

-- Start java.util.function.DoubleToLongFunction

data {-# CLASS "java.util.function.DoubleToLongFunction" #-} DoubleToLongFunction = DoubleToLongFunction (Object# DoubleToLongFunction)
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkDoubleToLongFunction :: (Double -> Java DoubleToLongFunction Int64) -> DoubleToLongFunction

-- End java.util.function.DoubleToLongFunction

-- Start java.util.function.DoubleUnaryOperator

data {-# CLASS "java.util.function.DoubleUnaryOperator" #-} DoubleUnaryOperator = DoubleUnaryOperator (Object# DoubleUnaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsDouble"
  mkDoubleUnaryOperator :: (Double -> Java DoubleUnaryOperator Double) -> DoubleUnaryOperator

-- End java.util.function.DoubleUnaryOperator

-- Start java.util.function.Function

data {-# CLASS "java.util.function.Function" #-} Function t r = Function (Object# (Function t r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkFunction :: (t <: Object, r <: Object) => (t -> Java (Function t r) r) -> Function t r

-- End java.util.function.Function

-- Start java.util.function.IntBinaryOperator

data {-# CLASS "java.util.function.IntBinaryOperator" #-} IntBinaryOperator = IntBinaryOperator (Object# IntBinaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkIntBinaryOperator :: (Int -> Int -> Java IntBinaryOperator Int) -> IntBinaryOperator

-- End java.util.function.IntBinaryOperator

-- Start java.util.function.IntConsumer

data {-# CLASS "java.util.function.IntConsumer" #-} IntConsumer = IntConsumer (Object# IntConsumer)
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkIntConsumer :: (Int -> Java IntConsumer ()) -> IntConsumer

-- End java.util.function.IntConsumer

-- Start java.util.function.IntFunction

data {-# CLASS "java.util.function.IntFunction" #-} IntFunction r = IntFunction (Object# (IntFunction r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkIntFunction :: (r <: Object) => (Int -> Java (IntFunction r) r) -> IntFunction r

-- End java.util.function.IntFunction

-- Start java.util.function.IntPredicate

data {-# CLASS "java.util.function.IntPredicate" #-} IntPredicate = IntPredicate (Object# IntPredicate)
  deriving Class

foreign import java unsafe "@wrapper test"
  mkIntPredicate :: (Int -> Java IntPredicate Bool) -> IntPredicate

-- End java.util.function.IntPredicate

-- Start java.util.function.IntSupplier

data {-# CLASS "java.util.function.IntSupplier" #-} IntSupplier = IntSupplier (Object# IntSupplier)
  deriving Class

foreign import java unsafe "@wrapper getAsInt"
  mkIntSupplier :: (Java IntSupplier Int) -> IntSupplier

-- End java.util.function.IntSupplier

-- Start java.util.function.IntToDoubleFunction

data {-# CLASS "java.util.function.IntToDoubleFunction" #-} IntToDoubleFunction = IntToDoubleFunction (Object# IntToDoubleFunction)
  deriving Class

foreign import java unsafe "@wrapper applyAsDouble"
  mkIntToDoubleFunction :: (Int -> Java IntToDoubleFunction Double) -> IntToDoubleFunction

-- End java.util.function.IntToDoubleFunction

-- Start java.util.function.IntToLongFunction

data {-# CLASS "java.util.function.IntToLongFunction" #-} IntToLongFunction = IntToLongFunction (Object# IntToLongFunction)
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkIntToLongFunction :: (Int -> Java IntToLongFunction Int64) -> IntToLongFunction

-- End java.util.function.IntToLongFunction

-- Start java.util.function.IntUnaryOperator

data {-# CLASS "java.util.function.IntUnaryOperator" #-} IntUnaryOperator = IntUnaryOperator (Object# IntUnaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkIntUnaryOperator :: (Int -> Java IntUnaryOperator Int) -> IntUnaryOperator

-- End java.util.function.IntUnaryOperator

-- Start java.util.function.LongBinaryOperator

data {-# CLASS "java.util.function.LongBinaryOperator" #-} LongBinaryOperator = LongBinaryOperator (Object# LongBinaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkLongBinaryOperator :: (Int64 -> Int64 -> Java LongBinaryOperator Int64) -> LongBinaryOperator

-- End java.util.function.LongBinaryOperator

-- Start java.util.function.LongConsumer

data {-# CLASS "java.util.function.LongConsumer" #-} LongConsumer = LongConsumer (Object# LongConsumer)
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkLongConsumer :: (Int64 -> Java LongConsumer ()) -> LongConsumer

-- End java.util.function.LongConsumer

-- Start java.util.function.LongFunction

data {-# CLASS "java.util.function.LongFunction" #-} LongFunction r = LongFunction (Object# (LongFunction r))
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkLongFunction :: (r <: Object) => (Int64 -> Java (LongFunction r) r) -> LongFunction r

-- End java.util.function.LongFunction

-- Start java.util.function.LongPredicate

data {-# CLASS "java.util.function.LongPredicate" #-} LongPredicate = LongPredicate (Object# LongPredicate)
  deriving Class

foreign import java unsafe "@wrapper test"
  mkLongPredicate :: (Int64 -> Java LongPredicate Bool) -> LongPredicate

-- End java.util.function.LongPredicate

-- Start java.util.function.LongSupplier

data {-# CLASS "java.util.function.LongSupplier" #-} LongSupplier = LongSupplier (Object# LongSupplier)
  deriving Class

foreign import java unsafe "@wrapper getAsLong"
  mkLongSupplier :: (Java LongSupplier Int64) -> LongSupplier

-- End java.util.function.LongSupplier

-- Start java.util.function.LongToDoubleFunction

data {-# CLASS "java.util.function.LongToDoubleFunction" #-} LongToDoubleFunction = LongToDoubleFunction (Object# LongToDoubleFunction)
  deriving Class

foreign import java unsafe "@wrapper getAsDouble"
  mkLongToDoubleFunction :: (Int64 -> Java LongToDoubleFunction Double) -> LongToDoubleFunction

-- End java.util.function.LongToDoubleFunction

-- Start java.util.function.LongToIntFunction

data {-# CLASS "java.util.function.LongToIntFunction" #-} LongToIntFunction = LongToIntFunction (Object# LongToIntFunction)
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkLongToIntFunction :: (Int64 -> Java LongToIntFunction Int) -> LongToIntFunction

-- End java.util.function.LongToIntFunction

-- Start java.util.function.LongUnaryOperator

data {-# CLASS "java.util.function.LongUnaryOperator" #-} LongUnaryOperator = LongUnaryOperator (Object# LongUnaryOperator)
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkLongUnaryOperator :: (Int64 -> Java LongUnaryOperator Int64) -> LongUnaryOperator

-- End java.util.function.LongUnaryOperator

-- Start java.util.function.ObjDoubleConsumer

data {-# CLASS "java.util.function.ObjDoubleConsumer" #-} ObjDoubleConsumer t = ObjDoubleConsumer (Object# (ObjDoubleConsumer t))
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkObjDoubleConsumer :: (t <: Object)
  => (t -> Double -> Java (ObjDoubleConsumer t) ()) -> ObjDoubleConsumer t

-- End java.util.function.ObjDoubleConsumer

-- Start java.util.function.ObjIntConsumer

data {-# CLASS "java.util.function.ObjIntConsumer" #-} ObjIntConsumer t = ObjIntConsumer (Object# (ObjIntConsumer t))
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkObjIntConsumer :: (t <: Object)
  => (t -> Int -> Java (ObjIntConsumer t) ()) -> ObjIntConsumer t

-- End java.util.function.ObjIntConsumer

-- Start java.util.function.ObjLongConsumer

data {-# CLASS "java.util.function.ObjLongConsumer" #-} ObjLongConsumer t = ObjLongConsumer (Object# (ObjLongConsumer t))
  deriving Class

foreign import java unsafe "@wrapper accept"
  mkObjLongConsumer :: (t <: Object)
  => (t -> Int64 -> Java (ObjLongConsumer t) ()) -> ObjLongConsumer t

-- End java.util.function.ObjLongConsumer

-- Start java.util.function.Predicate

data {-# CLASS "java.util.function.Predicate" #-} Predicate t = Predicate (Object# (Predicate t))
  deriving Class

foreign import java unsafe "@wrapper test"
  mkPredicate :: (t <: Object)
  => (t -> Java (Predicate t) Bool) -> Predicate t

-- End java.util.function.Predicate

-- Start java.util.function.Supplier

data {-# CLASS "java.util.function.Supplier" #-} Supplier t = Supplier (Object# (Supplier t))
  deriving Class

foreign import java unsafe "@wrapper get"
  mkSupplier :: (t <: Object)
  => (Java (Supplier t) t) -> Supplier t

-- End java.util.function.Supplier

-- Start java.util.function.ToDoubleBiFunction

data {-# CLASS "java.util.function.ToDoubleBiFunction" #-} ToDoubleBiFunction t u = ToDoubleBiFunction (Object# (ToDoubleBiFunction t u))
  deriving Class

foreign import java unsafe "@wrapper applyAsDouble"
  mkToDoubleBiFunction :: (t <: Object, u <: Object)
  => (t -> u -> Java (ToDoubleBiFunction t u) Double) -> ToDoubleBiFunction t u

-- End java.util.function.ToDoubleBiFunction

-- Start java.util.function.ToDoubleFunction

data {-# CLASS "java.util.function.ToDoubleFunction" #-} ToDoubleFunction t = ToDoubleFunction (Object# (ToDoubleFunction t))
  deriving Class

foreign import java unsafe "@wrapper applyAsDouble"
  mkToDoubleFunction :: (t <: Object)
  => (t -> Java (ToDoubleFunction t) Double) -> ToDoubleFunction t

-- End java.util.function.ToDoubleFunction

-- Start java.util.function.ToIntBiFunction

data {-# CLASS "java.util.function.ToIntBiFunction" #-} ToIntBiFunction t u = ToIntBiFunction (Object# (ToIntBiFunction t u))
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkToIntBiFunction :: (t <: Object, u <: Object)
  => (t -> u -> Java (ToIntBiFunction t u) Int) -> ToIntBiFunction t u

-- End java.util.function.ToIntBiFunction

-- Start java.util.function.ToIntFunction

data {-# CLASS "java.util.function.ToIntFunction" #-} ToIntFunction t = ToIntFunction (Object# (ToIntFunction t))
  deriving Class

foreign import java unsafe "@wrapper applyAsInt"
  mkToIntFunction :: (t <: Object)
  => (t -> Java (ToIntFunction t) Int) -> ToIntFunction t

-- End java.util.function.ToIntFunction

-- Start java.util.function.ToLongBiFunction

data {-# CLASS "java.util.function.ToLongBiFunction" #-} ToLongBiFunction t u = ToLongBiFunction (Object# (ToLongBiFunction t u))
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkToLongBiFunction :: (t <: Object, u <: Object)
  => (t -> u -> Java (ToIntBiFunction t u) Int64) -> ToLongBiFunction t u

-- End java.util.function.ToLongBiFunction

-- Start java.util.function.ToLongFunction

data {-# CLASS "java.util.function.ToLongFunction" #-} ToLongFunction t = ToLongFunction (Object# (ToLongFunction t))
  deriving Class

foreign import java unsafe "@wrapper applyAsLong"
  mkToLongFunction :: (t <: Object)
  => (t -> Java (ToLongFunction t) Int64) -> ToLongFunction t

-- End java.util.function.ToLongFunction

-- Start java.util.function.UnaryOperator

data {-# CLASS "java.util.function.UnaryOperator" #-} UnaryOperator t = UnaryOperator (Object# (UnaryOperator t))
  deriving Class

type instance Inherits (UnaryOperator t) = '[Function t t]

-- End java.util.function.ToLongFunction
