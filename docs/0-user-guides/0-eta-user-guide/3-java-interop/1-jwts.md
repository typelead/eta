# Java Wrapper Types

## General Syntax

Java Wrapper Types general syntax.



```eta
data X = X @[class-name]
  deriving Class
```

- `[class-name]` should be the fully qualified Java class name and `X` should be the Eta name you would use to refer to the corresponding Java class in foreign imports. Note that `[class-name]` can also be converted to an array type by appending `[]`.
- The Class typeclass is a built-in typeclass that is a marker for a JWT. **Make sure all your JWTs derive a Class instance**.

## Examples of JWT

```eta
data JInteger = JInteger @java.lang.Integer
  deriving Class

data JIntegerArray = JIntegerArray @java.lang.Integer[]
  deriving Class
```

In this example, we’re declaring JWTs for the `java.lang.Integer` class and the `java.lang.Integer[]` array (which is technically a class on its own).

## Deriving Standard Typeclass Instances

### General Syntax

```eta
data X = X @[class-name]
  deriving (Class, Eq, Show)
```

Currently, deriving the `Class`, `Eq`, and `Show` instances for JWTs is supported. You should derive these instances based on the need of the application. The `Eq` instance will use the underlying `Object.equals()` method and the `Show` instance will use `Object.toString()`.

## Marshalling Between Java and Eta Types

When writing FFI declarations, you are essentially specifying the type of a function whose arguments will be translated from Eta types to Java types and whose result will be translated from Java types to Eta types. This translation process is called marshalling.

### Java Primitives

The following table lists the mapping from primitive Java types to Eta types.

| Java Type  |  Eta Type  |
| ---------- |------------|
| `boolean`  |     `Bool` |
| `byte`     |     `Byte` |
| `short`    |    `Short` |
| `char`     |     `Char` |
| `int`      |      `Int` |
| `long`     |    `Int64` |
| `float`    |    `Float` |
| `double`   |   `Double` |

## Java Classes & Arrays

A Java Wrapper Type will marshal to an object of the class given in the ``CLASS`` annotation. Note that
a Java array is just a special class with a ``length`` member, hence you should
declare JWT to specify that you want to marshal to a Java array.

The following table shows a couple of Eta types which aren't JWTs, but still
marshal to a Java class or return type:

| Java Type               |  Eta Type  |
| ----------------------- |------------|
| `java.lang.String`      |   `String` |
| Any nullable object `X` |  `Maybe X` |
| `void`                  |       `()` |

## Next Section

We will now proceed with Java Foreign Import Declarations.
