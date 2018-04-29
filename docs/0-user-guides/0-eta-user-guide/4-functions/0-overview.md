# Overview

## Introduction

Functions are essence of functional programming. They are the fundamental unit of code re-use.

A function takes a value as input and produces a value as output, such that the output value is completely dependent on the input value and nothing else. This is called a *pure function*.

## Pure Function

Here's an example of a pure function:



```eta
f :: Int -> Int
f = \x -> x + 1
```

The function above adds one to its input to produce the output. Note that the type of a function contains an arrow `->` also called the function type constructor, a special type constructor that's built into the compiler. Note that the same operator `->` is also used when constructing the lambda expression `\x -> x + 1`.



As functions are extremely common, we use the syntax below:



```eta
f :: Int -> Int
f x = x + 1
```

## Function Application

You can use a function by *applying* it to an argument. The space operator is used to mean function application.



```eta
f 1 :: Int
```

## Multi-Input Function

We've looked at single-input functions. What about multi-input functions? We know the `(->)` type constructor only takes two inputs, so do we need a new type constructor for every number of inputs? Not necessarily. What if we created nested lambda expressions?



```eta
g :: (Int -> (Int -> Int))
g = \x -> \y -> x + y

g1:: Int -> (Int -> Int)
g1 x = \y -> x + y

g2 :: Int -> Int -> Int
g2 x y = x + y
```

**Note** that `g`, `g1`, `g2` are all equivalent.



As you can see, for multi-input functions, we supply an input one at a time in the expected order until all the lambdas have been eliminated, at which point the function is evaluated completely into the resultant value.

## Applying Multi-Input Functions

You can apply a multi-input functions by supplying two inputs, separated by spaces.



```eta
g2 1 2 :: Int
```

## Next Section

We will now discuss currying.
