# Coding Conventions

## Directory structure

For a given app the directory structure should look like this:



```console
app
- src
  - Main.hs
- java
  - Utils.java
- eta-first.cabal
- LICENSE
```

## Naming Rules

#### Source file names



In Eta the source file names should be the same as the module name with a `.eta` extension appended. And the module name always starts with a capital letter.



It is good practice to name the file after the description of what the file does.



#### Type names



In Eta types start with a capital letter.



```eta
add :: Int -> Int -> Int
add x y = x + y
```
In the above example, `Int` is a type defined in Eta's standard library and starts with a capital letter.



```eta
data Pair = Pair Int Int
```
In the above example, `Pair` is a user-defined type and starts with a capital letter.



#### Function Names



In Eta, functions start with a lowercase letter.



Function names with multiple words are written using [camelCase](https://en.wikipedia.org/wiki/Camel_case) convention.



```eta
add :: Int -> Int -> Int
add x y = x +y
```
In the above example, `add` is a function.



```eta
data Pair = Pair Int Int

myPair :: Pair
myPair = Pair 1 2
```
In the above example, `myPair` is a function written using the camelCase convention.



#### Choosing Good names



It good practice to use descriptive names. The names should make the purpose of the entity clear. One should avoid using meaningless words like `Wrapper` etc. in names.



When using an acronym as part of a declaration name, capitalize it when it consists of two letters (`IOStream`); capitalize only the first letter if it is longer (`HttpInputStream`).

## Formatting

In most cases, Eta follows Haskell and Java conventions.



Use 4 or 8 spaces for indentation. Do not use tabs.



It's good practice to format your code to fit into 80 columns.



#### Horizontal Whitespace



**Operators**



Put spaces around operator (` x + y + z `). Don't put space around **range** operator (`1..n`).



**Functions**



```eta
reverse :: [Int] -> [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```

- ` :: `  - should have space around double colon.
- ` -> `  - space around it
- `reverse []` - single space between function and its argument.

Never put a space after `(`, `[`, or before `]`, `)`.

## Good Coding Conventions

**Imports**



Standard library modules like `Java.Array`, `Java.IO`, and `Java.Utils` should be imported in alphabetical order.



```eta
import Java.Array
import Java.IO
import Java.Utils
```

## Next Section

We will now proceed with the section on Java Interop.
