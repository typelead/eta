# Application

## Introduction

Function application was introduced earlier and can be done with the space operator. The space operator cannot be re-used in setting where you want to avoid defining new functions.

## Example

```eta
applyOne :: (Int -> Int) -> Int
applyOne f = f 1 + 1
```

This function will apply the value `1` to whatever function is passed in as an input and then add `1` to the result.



Note that we couldn't express this function as the composition of other functions. We had to explicitly define how to use its input. What we need is a re-usable operator for function application.

## Application Operator

```eta
($) :: (a -> b) -> a -> b
($) f x = f x
```

The difference between `($)` and the normal space operator is that it is right-associative. To see the difference:



```eta
f x y z = ((f x) y) z
f $ x $ y $ z = f (x (y z))
```

## Rewriting the Example

Now `applyOne` can be rewritten as:



```eta
applyOne :: (Int -> Char) -> Char
applyOne = (+ 1) . ($ 1)
```

Note that we were able to define `applyOne` solely by composing existing functions.

## Next Section

We will explore even futher methods of re-use by exploring higher-order functions.
