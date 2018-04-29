# Currying

## Introduction

In the last section, we saw how we can obtain multi-input functions by nesting lambda expressions. This also implies that we can partially apply a function.



```eta
f = \x -> (\y -> x + y)
f 1 = (\x -> (\y -> x + y)) 1
    = (\y -> 1 + y)
```

The expression `f 1` is itself a function that takes a single input and fills in the value supplied for the first input in the original function `f`. The expression `f 1` is referred to as a *curried function*.

## Next Section

Next we will discuss how we can combine functions together.
