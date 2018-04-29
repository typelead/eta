# Recursion

## Problem Solving

One of the most powerful tactics in problem solving is taking a big problem that you don't know how to solve and breaking it down into pieces that you do know how to solve. Then, once the small problems have been solved, you can use those solutions to solve the larger problem. And that's the essence of `recursion`.

## Breaking Down Sum

The problem of summing a list can be broken down into a smaller problem: addition.

What is the sum of an empty list? 0.

What is the sum of a list with a single number? That number.

How can we break up a non-trivial list, such that we can use the information above?

We can break it up into two parts: a list with one element, and a list with the remaining elements. In fact, because the standard list type in Eta is structured that way, it's the natural way to break it up.



```
Original List: [1,2,3,4,5]
Split 1:       [1] | [2,3,4,5]
```

So one way we can compute the sum is to take the sum of the one-element list and add it
to the sum of the rest of the list. But how do we get the sum of the rest of the list?
Break up the rest of the list until it breaks down to a problem we can solve.



```
Split 2:     [1] | [2] | [3,4,5]
...
Split 5:     [1] | [2] | [3] | [4] | [5] | []
Computation:  1  +  (2  +  (3  +  (4  +  (5  + 0))))
```

We know when to stop because we'll eventually hit a case where the list containing the remaining elements will be empty, in which case the sum will be 0.

## Code

The description in the last sub-lesson can be put into code form below:



```eta
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
```

Notice that it's a *lot* less code than the Java version and we were thinking about the problem itself and not how to make the computer solve the problem! This is the transition from imperative programming to declarative programming, in which you describe the structure of your problem and it will be converted to a form the computer can understand.

## Next Section

In the next section, we'll talk about building web servers in Eta using the Servant web framework.
