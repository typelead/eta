# What is Eta?

## Overview

Eta is pure, lazy, statically-typed functional programming language on the Java Virtual Machine.

These three features put together provide a pleasing development experience:

- Libraries with APIs that are intuitive and are fun to work with.
- Codebases that developers can fearlessly modify and extend.
- Best software engineering practices that are enforced at compile-time.

## Motivation

We are all used to large codebases that everyone is afraid to touch. We feel that is unacceptable and that we need a language that encourages best practices by construction and notifies you as soon as you are headed down a painful road.

We found that Haskell solves the problem, but we realized upon research that if it is to reach the industry at large, we must have an implementation on the JVM. This opens us up to the extremely mature JVM tooling and allows Eta to be used in enterprise settings.

## Goals

Eta is a fork of GHC, the Glasgow Haskell Compiler, which focuses on:

- User Experience
    - Error messages should be easy to read
    - The developer should be able to interact with the compiler and get feedback
- Performance
    - Compilation should be fast and incremental
    - Runtime should be fast and tunable on demand.
    - Tooling should help developers quickly track down correctness and performance bugs.
- Safety
    - Developers should be notified when they're writing bad code.
    - Developers should be able to refactor quickly without breaking existing code.

As Eta shares the 25-year-old compiler infrastructure of GHC, the developer has access to very powerful optimizations that allows her to write high-level functional code and get good performance at the same time.

With Eta, you get the best of Haskell (language) and the best of Java (platform) which allows you to re-use a large repository of existing libraries.

## A Taste of Hello World

We will now take a look at the famous "Hello World" program to get a feel for Eta syntax.

```eta
main :: IO ()
main = putStrLn "Hello World!"
```

### Notes:

- `main` is called a *binder*, or constant variable.
- `()` is called *unit* and is a *type*.
- `IO ()` is called a *type*.
- `::` operator can be read as "has type"
- `=` operator is used for *binding* a *binder* to a *value*.
- `putStrLn` is a *binder* whose value is a *function*.
- `"Hello World!"` is a *string*, a sequence of characters.

You can read this in your head as:

- Line 1: The `main` binder has type `IO ()`.
- Line 2: The `main` binder is bound to an expression `putStrLn "Hello World!"`.

`main` is a special binder that corresponds to the start of the program. The entrypoint of the program *must* have type `IO ()` which can be thought of as an operation that
can potentially interact with file system, network, the OS, or perform any *side-effect*
whatsoever.

## Next Module

In the next module, we will demonstrate how to setup a working environment for Eta.
