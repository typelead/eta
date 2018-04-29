# Java Arrays and Subclasses

## Working With Arrays

The utilities for working with arrays are defined in the `Java.Array` module which is re-exported by the `Java` module. The API is shown below:

```eta
-- The `c` type variable represents the type of the array.
-- The `e` type variable represents the type of the element of the array.
class (Class c) => JArray e c | c -> e where

  -- Create a new Java array.
  anew :: Int -> Java a c

  -- Get an element from a Java array.
  aget :: Int -> Java c e

  -- Set an element in a Java array.
  aset :: Int -> e -> Java c ()

-- Get the length of an array
alength :: JArray e c => Java c Int

-- Convert a Java array to an Eta list
arrayToList :: JArray e c => Java c [e]

-- Convert a lazy Eta list to a Java array
arrayFromList :: JArray e c => [e] -> Java a c
```

Note that the `e` type variable is determined from the `c` type variable and vice-versa, so each array type is expected to have a unique element type and each element type is expected to have a unique array type when using the API.

## Primitive Arrays

Primitive arrays have pre-defined instances in `Java.Array`. 

The following table lists the exported types and their element types.

| Java Type     |     Array Type     | Element type |
| ------------- |:------------------:| ------------:|
| `boolean[]`   |   `JBooleanArray`  |       `Bool` |
| `byte[]`      |   `JByteArray`     |       `Byte` |
| `short[]`     |   `JShortArray`    |      `Short` |
| `char[]`      |   `JCharArray`     |       `Char` |
| `int[]`       |   `JIntArray`      |        `Int` |
| `long[]`      |   `JLongArray`     |      `Int64` |
| `float[]`     |   `JFloatArray`    |      `Float` |
| `double[]`    |   `JDoubleArray`   |     `Double` |

## Object Arrays

Object arrays must be explicitly declared as JWTs and must have an instance of the JArray typeclass defined for them.

The `Java.Array` has one pre-defined object array: `JStringArray` which corresponds to `String[]` in Java and has `JString` as the element type.



### Example

```eta
{-# LANGUAGE MultiParamTypeClasses #-}

import Java

data JInteger = JInteger @java.lang.Integer
  deriving (Class, Show)

data JIntegerArray = JIntegerArray @java.lang.Integer[]
  deriving Class

foreign import java unsafe "@new" toJInteger :: Int -> JInteger
foreign import java unsafe intValue :: JInteger -> Int

-- There's a default instance for object arrays, so no need to define your own.
instance JArray JInteger JIntegerArray

main :: IO ()
main = java $ do
  arr <- arrayFromList integers
  elems <- withObject arr $ mapM aget [0..9]
  io $ print elems
  withObject arr $ mapM_ (\i -> aset i (toJInteger (i * 2))) [0..9]
  arrList <- arr <.> arrayToList
  io $ print arrList
  where integers = map toJInteger [1..10]
```

## Working with Java Subclasses

### Motivation

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

We can even change the code above to use the Java sequenceable:



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

## Next Section

We will now proceed with handling Java Generics, Enums, Converters, Interfaces, Covariances and Contravariance.
