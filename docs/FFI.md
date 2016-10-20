# Interacting with Java in GHCVM

## Table of Contents
- # Overview
- # Background
  - # Primitive Types
  - # Declaring Tag Types
  - # Java Monad
- #Syntax
  - #Foreign Imports
- 

## Overview

The layer that interacts with Java in GHCVM is called the Foreign Function Interface (FFI). This layer will allow you to import a Java method as a Haskell function and export a Haskell function as a Java method. It automatically handles the intermediate conversions between Java types and Haskell types, so all you have to worry about is the right type signature.

## Background

The FFI revolves around some built-in types that GHCVM specially recognizes, so we'll start by introducing them.

### Unboxed & Primitive Types

Unboxed types are those types which must have a well-defined value and hence cannot have values that are suspended expressions (thunks). These are typically raw integers, floats, and objects at the JVM level. Moreover, these types can be thought of as primitive and cannot be defined using the Haskell language and hence need to be given special support by the compiler. 

They tend to be suffixed with a `#` and require the use of `MagicHash` language extension in order to be recognized in source code. Primitive types have a special representation at the JVM level, shown below.

| Primitive Type | Java Type | Notes |
| :---: | :---: | --- |
| `Char#` | int | |
| `Int#` | int | |
| `Int32#` | int | |
| `Int64#` | long | |
| `Word#` | int | |
| `Word32#` | int | |
| `Word64#` | long | |
| `Float#` | float | |
| `Double#` | double | |
| `JChar#` | char | |
| `JBool#` | boolean | |
| `JByte#` | byte | |
| `JShort#` | short | |
| `Object# c` | Depends on the tag of c |  |
| `JArray# c` | Depends on the tag of c | You *cannot* use `JArray# c` directly! It must be used in conjunction with `Object#`, like `Object# (JArray# Int#)` which corresponds to `int[]` or `Object# (JArray# (Object# String))` which corresponds to `String[]`. |
| `Addr#` | java.nio.ByteBuffer | |
| ``State# a`` | none | |
| `Void#` | none | |
| `Proxy#` | none | |
| ``a ~# b`` | none | |
| ``a ~R# b`` | none | |
| `RealWorld` | ghcvm.runtime.stg.StgClosure | |
| `Array#` | ghcvm.runtime.stg.StgClosure | |
| `ByteArray#` | ghcvm.runtime.stg.StgClosure | |
| `ArrayArray#` | ghcvm.runtime.stg.StgClosure | |
| `SmallArray#` | ghcvm.runtime.stg.StgClosure | |
| `MutableArray#` | ghcvm.runtime.stg.StgClosure | |
| `MutableByteArray#` | ghcvm.runtime.stg.StgClosure | |
| `MutableArrayArray#` | ghcvm.runtime.stg.StgClosure | |
| `SmallMutableArray#` | ghcvm.runtime.stg.StgClosure | |
| `MutVar#` | ghcvm.runtime.stg.StgClosure | |
| `MVar#` | ghcvm.runtime.stg.StgClosure | |
| `TVar#` | ghcvm.runtime.stg.StgClosure | |
| `StablePtr#` | int | |
| `StableName#` | int | |
| `BCO#` | ghcvm.runtime.stg.StgClosure | |
| `Weak#` | ghcvm.runtime.stg.StgClosure | |
| `ThreadId#` | ghcvm.runtime.stg.StgClosure | |

### Boxed Types

Boxed types, on the other hand, can have values which can be thunks and may also be undefined if the evaluation of the thunk leads to an error state. Boxed types can store both boxed and unboxed values internally.

Example:

```haskell
data Int = I# Int#
```

### Declaring Tag Types

In GHCVM, you regularly have to declare tag types. Tag types represent Java objects of a given class in Haskell and are typically wrappers for raw Java objects. 

```haskell
data {-# CLASS "[class-name-here]"} P = P (Object# P)
```
This is the generic format for declaring a tag type where:
- `[class-name-here]` is the name of a the class the tag type represents. For example, it can be `java.lang.String`.
- `P` is the Haskell name you would use to refer to it. Typically, `P` is the unqualified class name.

Example:
```haskell
data {-# CLASS "java.io.PrintStream" #-} PrintStream = PrintStream (Object# PrintStream)
```

In order to tell Haskell about it's parent/child relationships for use in the strongly typed usages of the FFI, a `Class` typeclass instance and a `Super` type family declaration must be defined. The `Class` typeclass contains methods that the FFI internally uses to get the underlying raw Java object from the tag type in the cases where one does polymorphic FFI imports. The `Super` type family defines the direct parent relationship of the class and that will be extended into an entire class hierarchy within Haskell using the laws defined for the `Extends` typeclass. The `Extends a b` typeclass is a multi-parameter typeclass that stores a relationship that `a` is descendent of `b`.

```haskell
{-# LANGUAGE TypeFamilies #-}
class Class c where
    obj :: Object# c -> c
    unobj :: c -> Object# c

type family Super (a :: *) :: *
```

Example:
```
{-# LANGUAGE TypeFamilies #-}
class Class PrintStream where
    obj = PrintStream
    unobj (PrintStream o) =  o

type instance Super PrintStream = FilterOutputStream
```

### Java Monad
TODO


## Syntax

The following will show the general syntax and what will occur in each of the cases, following by some examples.

## Examples
TODO
