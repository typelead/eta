# Basic Syntax

## Defining Modules

As a first step, let's understand the Eta modules and packages at a higher-level.

<img alt="Eta Packages" src="/images/eta-packages.svg" style="height:400px; padding: 20px 0px;">

An Eta package consists of modules and each module is defined in a single file with the `.eta` extension. An Eta module or program is free to import other Eta modules from other packages.

Module specification should be at the top of the source file. Every Eta program is a collection of ***modules***. Each module contains functions and types.

```eta
module Main where

main :: IO ()
main = putStrLn "You declared a main module!"
```

**NOTE**: Module names always start with a capital letter.

## Bindings

An Eta program consists of bindings. A binding is an association between a name or identifier and a value. **Other languages call these variables** because you can set new values to the variable over time.

In Eta, bindings are immutable and cannot be set to a new value once they've been defined. Bindings in Eta can be thought of as constant variables from other languages.

```eta
fullName :: String
fullName = "Harry Potter"
```

- `fullName` is an identifier of type `String`
- `fullName` has the value `"Harry Potter"`

## Functions

Defining a function in Eta is describing what it does using other functions as building blocks.

```eta
double :: Int -> Int -- function declaration
double n = 2*n       -- function definition
```

- Function declaration specifies the function name and the type of the parameters and the return type.
- Function definition is where you define the function.

Defining functions can be done in multiple lines.

```eta
xor :: Bool -> Bool -> Bool
xor True  True  = False
xor False False = False
xor x     y     = True
```

**NOTE**: Function declaration is optional in Eta since it has a **global type inference** which we are going to cover in a later chapter.

## Comments

It is good practice to describe what is going on in your code by using comments. When you declare a part of your program as a comment, the compiler will ignore it.

```eta
-- This is end-of-line comment

{- This is a block comment
   of multiple lines -}
```

## Next Section

We will now proceed with the basic coding conventions.
