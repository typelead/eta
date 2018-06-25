# Quick Start

## Overview

In this section, we'll cover all the commonly used features of the REPL.

## Prerequisites

[Eta v0.8 or above](/docs/user-guides/eta-user-guide/installation/methods)

## Start Up

You can fire up a REPL from anywhere with the following command:

```sh
$ etlas repl

...

Prelude>
```

You'll be greeted with a nice welcome message and then a prompt.

The text before the `>` will list all the modules in scope. This means that you can use any exported function or type from the listed modules. By default, `Prelude` is in scope, just as it would be in Eta modules written in text files.

## Expression

Let's try an expression!

```sh
Prelude> "Hello" ++ " " ++ "World!"
"Hello World!"
it :: (Num a) => a
```

The result of evaluating the expression is printed using its `Show` instance. Then, the type of the expression is also printed. Note that the expression is bound to an implicit binder called `it` which you can use later.

```sh
Prelude> it
"Hello World!"
it :: [Char]

Prelude> replicate 10 'c'
"cccccccccc"
it :: [Char]

Prelude> it
"cccccccccc"
it :: [Char]
```

If an expression does not have a `Show` instance, the REPL throws an error.

```sh
Prelude> \x -> x + 1

<interactive>:2:1: error:
    No instance for (Show (a0 -> a0))
      (maybe you haven't applied enough arguments to a function?)
      arising from a use of 'print'
    In the first argument of 'print', namely 'it'
    In a stmt of an interactive GHCi command: print it
```

## IO Actions

If the expression is an IO action (of type `IO a`), then the REPL will evaluate the IO action *and* execute it!

```sh
Prelude> print "hello"
"hello"
it :: ()
```

Note that the type of `it` is `()` and not `IO ()` which is the type of the expression `print "hello"`. This is because the IO action has been run and `it` is bound to the *result* of the action instead of the action itself, which is what we want most of the time.

## Modules

Suppose we wanted to use the `sort` function defined in `Data.List`.

```sh
Prelude> sort [10,9..1]

<interactive>:2:1: error:
    Not in scope: 'sort'
    Perhaps you meant 'sqrt' (imported from Prelude)

Prelude> import Data.List

Prelude Data.List> sort [10,9..1]
[1,2,3,4,5,6,7,8,9,10]
it :: (Enum a, Num a, Ord a) => [a]
```

You can use the `import` statement to import modules into scope, just like in Eta code. Observe that `Data.List` was added before the `>`.

In the REPL, it's nice to *unimport* modules as well, so there's a more powerful syntax using the `:module` command. You can use `:m` for shorthand.

### Adding Modules

You can add modules using `:m +[module1] [module2] ...` syntax.

```sh
Prelude Data.List> :m +Data.Monoid

Prelude Data.List Data.Monoid> "Hello" <> " " <> "World!"
"Hello World!"
it :: [Char]
```

### Removing Modules

You can remove modules with `:m -[module1] [module2] ...`.

```sh
Prelude Data.List Data.Monoid> :m -Data.List

Prelude Data.Monoid> sort [1..10]

<interactive>:19:1: error:
    Not in scope: 'sort'
    Perhaps you meant 'sqrt' (imported from Prelude)

Prelude Data.Monoid> [1..10] <> [11..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
it :: (Enum t, Num t) => [t]
```

### Restricting Modules

You can also specify exact what modules you want in scope with `:m [module1] [module2] ...`.

```sh
Prelude Data.Monoid> :m Data.List Data.Char

Prelude Data.List Data.Char>
```

Note that `Prelude` is added to the list even though you didn't mention it. Because `Prelude` has all the commonly used methods, it is automatically added regardless of which `:module` command you use.

## Next Section

In the next section, we will cover the different contexts in which you can use the REPL.
