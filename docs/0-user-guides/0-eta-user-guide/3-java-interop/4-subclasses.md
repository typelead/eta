# Java Subclasses

## The Problem

Eta does not understand subclasses by default, so if you try to use a method defined in a superclass on a subclass, it won’t typecheck.



Using the imports from the Java Interop Examples,



```eta
foreign import java unsafe toString :: Object -> String

data File = File @java.io.File
  deriving Class

main :: IO ()
main = do
  file <- java $ newFile "test.txt"
  -- This line will not typecheck since
  -- Object cannot match with File!
  putStrLn (toString file)
```

So how do we teach Eta about Java inheritance relationships to make Java code handling smoother? By using the `Extends` typeclass from the standard library - it is accessible after importing the Java module.

## The Extends typeclass

```eta
class (Class a, Class b) => Extends a b where
  superCast :: a -> b
  unsafeCast :: b -> a
```

The `Extends` typeclass is a multi-parameter typeclass defined for JWTs where `Extends a b` means that JWT `a` is a subclass of JWT `b`. The FFI has built-in support for the `Extends` typeclass so you can freely add those constraints into your imports.

For this typeclass, you don’t define instances directly. Instead, you can declaratively specify parent classes and interfaces using the `Inherits` type family.

**NOTE:**
The standard library defines an alias for the `Extends` typeclass referred to as `<:`. For example, `Extends a Object` can written as `a <: Object`. We will be using this throughout the tutorial because it is more natural. You must enable the `TypeOperators` extension to use `<:` by placing `{-# LANGUAGE TypeOperators #-}` at the top of your file.

**NOTE:**
While you won’t find many situations to use them, you can use the `superCast` and `unsafeCast` functions when you need to cast between object types.

`superCast` just does a conversion at the Eta-level and no explicit conversion is done at the Java-level, so this function is safe to use in all cases.

`unsafeCast` does an explicit cast at the Java-level and can throw a `ClassCastException` if you make an invalid conversion, hence the name.

## The Inherits type family

```eta
type family Inherits (a :: *) :: [*]
```

The `Inherits` type family takes a JWT and returns a type-level list of JWTs.

### Example

```eta
{-# LANGUAGE TypeFamilies, DataKinds #-}

data Serializable
  = Serializable @java.io.Serializable
  deriving Class

data File = File @java.io.File
  deriving Class

type instance Inherits File = '[Object, Serializable]
```

Note that the `TypeFamilies` and the `DataKinds` extensions are required to define the Java inheritance relationships and that the first element of the type-level list **must be the parent class** and the remaining elements can be the implemented interfaces in any order. Note that it is not necessary to inform Eta about all the relationships, only those that you need for your particular application.

## Problem Resolution

The problematic code above can now be fixed:



```eta
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, FlexibleContexts #-}

import Java hiding (toString)

foreign import java unsafe toString :: (a <: Object) => a -> String
foreign import java unsafe "@new" newFile  :: String -> Java a File

data File = File @java.io.File
  deriving Class

type instance Inherits File = '[Object]

main :: IO ()
main = do
  file <- java $ newFile "test.txt"
  -- This line will now typecheck!
  putStrLn (toString file)
```

We can even change the code above to use the Java monad:



```eta
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

foreign import java unsafe toString :: (a <: Object) => Java a String
foreign import java unsafe "@new" newFile  :: String -> Java a File

data File = File @java.io.File
  deriving Class

type instance Inherits File = '[Object]

main :: IO ()
main = do
  string <- java $ newFile "test.txt" >- toString
  putStrLn string
```

### Note :
You can specify an arbitrary number of `Extends` constraints based on your use-case.

## Working With Covariance and Contravariance

In Java, covariance is expressed with `? extends X` and contravariance is expressed with `? super Y`. The [andThen](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html) method has signature `<V> Function<T,V> andThen(Function<? super R,? extends V> after)`. It exhibits both covariance and contravariance so we will import it as an example.



```eta
foreign import java unsafe "@interface andThen" andThen ::
  (t <: Object, r <: Object, v <: Object, r <: a, b <: v)
  => Function a b -> Java (Function t r) (Function t v)
```

For each `?` we should generate a fresh variable. In the case above we use `a` and `b`.

## Next Section

In the next section, we will look at more advanced Java imports.
