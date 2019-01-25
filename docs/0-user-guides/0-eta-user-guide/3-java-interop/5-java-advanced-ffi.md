# Java Advanced FFI

## Working With Java Generics

Now that we have access to Java inheritance relationships inside of Eta, we can now work conveniently with Java Generics.



### Importing Generic Classes

Importing generic classes is not much different than importing concrete classes - just add some type parameter.



```eta
data List a = List (@java.util.List a)
  deriving Class
```

### Importing Generic Methods

Importing generic methods is also not much different than importing concrete methods - just add `Extends` constraints that specify the type bounds for each of the generic parameters. If the parameter does not have a type bound, you should specify `Object`.



```eta
foreign import java unsafe "@interface add" add
  :: (a <: Object, b <: List a) => a -> Java b Bool
```

See the [java.util.List.add](https://docs.oracle.com/javase/7/docs/api/java/util/List.html#add(E)) documentation.



### Example

```eta
-- These language extensions are currently required to support
-- Java Generics.
{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

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

-- The following a declarations of Java wrapper types. These types let you
-- interact directly with the corresponding Java objects.
-- This will not be the final syntax for Java wrapper types, see:
-- https://github.com/typelead/eta/issues/140
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

data JInteger a = JInteger @java.lang.Integer
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

```

## Working With Java Enums

Many Java packages often contain Enums. Let’s see how to handle them. We’ll be importing [ClientInfoStatus](https://docs.oracle.com/javase/7/docs/api/java/sql/ClientInfoStatus.html) as an example.



```eta
data ClientInfoStatus = ClientInfoStatus @java.sql.ClientInfoStatus
  deriving Class

type instance Inherits ClientInfoStatus = '[Enum ClientInfoStatus]

foreign import java unsafe
  "@static @field java.sql.ClientInfoStatus.REASON_UNKNOWN"
  reasonUnknown :: ClientInfoStatus

foreign import java unsafe
  "@static @field java.sql.ClientInfoStatus.REASON_UNKNOWN_PROPERTY"
  reasonUnknownProperty :: ClientInfoStatus

foreign import java unsafe
  "@static @field java.sql.ClientInfoStatus.REASON_VALUE_INVALID"
  reasonValueInvalid :: ClientInfoStatus

foreign import java unsafe
  "@static @field java.sql.ClientInfoStatus.REASON_VALUE_TRUNCATED"
  reasonValueTruncated :: ClientInfoStatus
```

## Working with Variable Arguments

Some methods in Java, like the method [java.util.Formatter.format](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html#format(java.lang.String,%20java.lang.Object...)) with signature `Formatter (String format, Object.. args)`, take variable arguments. Variable arguments are simply arrays, hence can be imported accordingly:



```eta
data Formatter = Formatter @java.util.Formatter
  deriving Class

-- Note that we didn't have to import `Object[]` because JObjectArray already exists
-- in the standard library.
foreign import java unsafe format :: String
                                  -> JObjectArray -> Java Formatter Formatter
```

## Working With Java Interfaces

Many Java interfaces often contain just a single method and such interfaces are commonly used to pass functions and callbacks in Java. Many frameworks and libraries use this type of interface frequently, so it useful to be able convert Eta functions and implement these interfaces.



Suppose we try to make an implementation of [Runnable](https://docs.oracle.com/javase/7/docs/api/java/lang/Runnable.html) in Eta:



```eta
data Runnable = Runnable @java.lang.Runnable
  deriving Class

foreign import java unsafe "@wrapper run"
  runnable :: Java Runnable () -> Runnable

data Thread = Thread @java.lang.Thread
  deriving Class

foreign import java unsafe "@new" newThread :: Runnable -> Java a Thread
foreign import java unsafe start :: Java Thread ()

main :: IO ()
main = java $ newThread (runnable (io $ putStrLn "Run in another thread"))
           >- start
```

Note that this can be applied for abstract classes as well - just use a `@wrapper @abstract` annotation instead.



### Example

Let’s try to wrap the [java.util.function.Function](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html) interface in Java 8. The method we want to implement on the Eta side has signature `R apply(T t)`.

The import would be like this:

```eta
data Function t r = Function (@java.util.function.Function t r)
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkFunction :: (t <: Object, r <: Object)
             => (t -> Java (Function t r) r) -> Function t r
```

### Example

This example demonstrates how to wrap an interface with several methods.

When the interface or abstract class has various abstract methods we must implement all of them with the wrapper.

To wrap this java interface:

```java
public interface SomeInterface {
       String method1(int i);
       double method2(int i, int j);
}
```

We should implement this eta wrapper:

```eta
foreign import java unsafe "@wrapper method1,method2"
  :: (Int -> Java SomeInterface JString)        -- Abstract/Interface Method 1
  -> (Int -> Int -> Java SomeInterface JDouble) -- Abstract/Interface Method 2
  -> SomeInterface
```

## Next Section

In the next section, we will look at how to add Java dependencies to our Eta code.
