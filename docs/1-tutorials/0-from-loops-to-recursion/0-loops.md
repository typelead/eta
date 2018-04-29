# Loops

## Overview

Loops are fundamental to all the mainstream programming languages and it can feel very
strange when you no longer have them in Eta.

Loops are a control-flow mechanism that let you perform a repetitive action multiple times. When you think in terms of loops, you're thinking about how to change memory locations in a coordinated way to get the desired result.

## Looping Example

Let's take a simple example for computing a sum of a list of integers. We will use Java as the host imperative language for all of our examples in this module.




```java
public class SumExample {
  public static int sumIntegers(List<Integer> ints) {
    Iterator<Integer> it = ints.iterator();
    int sum = 0;
    while (it.hasNext()) {
      sum += it.next();
    }
    return sum;
}
```

## Reflecting

What we wanted to say is that we want to combine all the numbers in the list using the addition operator, and if you step back, that Java code is quite a complex way to address that idea. In fact, the core logic of adding elements of the list is obscured in the
code that iterates over the list.

Is there a way we can write code that makes it obvious what the function is doing just
by looking at it without having to worry about *how* the result is computed?

## Next Section

We'll take a look at recursion, a powerful method used to solve problems and how we can implement `sum` much more concisely in Eta.
