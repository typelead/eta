# Composition

## Introduction

Now that we have functions, we need a way to combine small functions together to make larger functions that can handle complex tasks, without having to rewrite the same functions over and over again.

## Example

```eta
add :: Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

f :: Int -> Int
f x = 2 * x + 1
```

Note that `f` is a function that multiplies its argument by 2 followed by incrementing it by 1. Note that instead of reusing the existing `add` and `multiply` functions, we had declared an entirely new function.

## Composition

Suppose we had two functions `f :: Int -> Char` and `g :: Char -> Double`. Can we generate a function `h :: Int -> Double` that applies `f` and `g` one after another? Yes, we can, using the composition operator defined below.



```eta
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)
```

## Rewriting the Example

We can now rewrite the example using the existing functions, using the currying concept from the last section.



```eta
f = add 1 . multiply 2
```

Note that you should read composition pipelines from left-to-right.

## Next Section

We will then proceed to re-using function application.
