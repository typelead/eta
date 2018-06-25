# Higher-Order Functions

## Introduction

We often observe patterns that repeat in multiple situations and we would like to abstract out the common parts for code reuse and for concise code.

## Example

```eta
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs
```

Note the similarities in the two examples. Both functions combine the elements of the list with a binary operation. We can abstract out the similarities into a higher-order function.

## Abstraction

We abstract out the binary operation as well as the default value into a function called `aggregate`, shown below:



```eta
aggregate :: (Int -> Int -> Int) -> Int ->  [Int] -> Int
aggregate combine default [] = default
aggregate combine default (x:xs) =
  combine x (aggregate combine default xs)
```

Now we can redefine `sum` and `product` as:



```eta
sum = aggregate (+) 0
product = aggregate (*) 1
```

Higher order functions allow us to abstract out common patterns and allow us to define powerful, re-usable functions.

## Examples

The `aggregate` function is still highly specialized. The Eta standard library provides many general, useful, higher-order functions:



### Map

```
map :: (a -> b) -> [a] -> [b]

map (* 2) [1..5] == [2,4,6,8,10]
```

`map` transforms a list by taking a function that converts each element. Note that map preserves the original structure (doesn't delete or add elements), and each element is operated on independently.



### Filter

```
filter :: (a -> Bool) -> [a] -> [a]

filter (> 3) [1..5] == [4,5]
```

`filter` takes a list and generates a new list keeping the elements that satisfy the predicate that is supplied.

## Next Section

We will explore learn about Eta metaprogramming.
