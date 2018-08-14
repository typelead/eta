# Java Arrays

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

## Next Section

We will now proceed with handling Java Subclasses.
