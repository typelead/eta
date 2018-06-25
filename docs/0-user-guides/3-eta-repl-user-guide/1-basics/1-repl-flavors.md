# REPL Flavors

## Overview

You can use the REPL in two contexts: within an Etlas project or outside an Etlas project. In this section, we will discuss when it is appropriate to use either REPL.

## Global REPL
 
When you're evaluating libraries to use for one of your projects, you may want to try them out first before adding them to your project. In other cases, you may not have a project to work with and you just want to try something out. In either of these cases, you would want to use the **Global REPL**.

The Global REPL is global in the sense that you can use it from any directory that doesn't have any Etlas project files (`*.cabal` or `cabal.project`).

### Syntax

```
etlas repl [package1] [package2] ...
```

By default, the `base` and `eta-meta` packages will be available in the REPL. If you want additional packages to be available, you can pass them as arguments as shown above. Etlas will download and install them into the Etlas Store if they haven't been installed already.

### Example

```
$ etlas repl shakespeare blaze-html

...

Prelude> :m +Text.Hamlet Text.Blaze.Html.Renderer.String

Prelude Text.Hamlet Text.Blaze.Html.Renderer.String> putStrLn (renderHtml [shamlet|<div>Hello, world!|])
<div>Hello, world!</div>

it :: ()

```

## Project REPL

When you are working with a project, you often want to test out some of the functions you've written to see if they're working as you expected. You'd need the **Project REPL** in this case.

The Project REPL is automatically activated if you're in a project (a folder that contains a `*.cabal` or `cabal.project`).

### Syntax

```
etlas repl [TARGET]
```

The `[TARGET]` is optional, and the `library` target will be loaded by default. All the modules listed in the `exposed-modules` field will automatically be loaded into the REPL.

### Example

Create a new directory called `repl-test` and add the following files:

**Lib.hs**
```eta
module Lib where

someString :: String
someString = "someString"
```

**repl-test.cabal**
```
name: repl-test
version: 0.0.0
build-type: Simple
cabal-version: >= 1.10

library
  default-language: Haskell2010
  build-depends: base
  exposed-modules: Lib
```

You can start the Project REPL as shown below.

```sh
$ etlas repl

...

[1 of 1] Compiling Lib

Successful! One module loaded.

*Lib> someString
"someString"
it :: String
```

You can also start it by explicitly specifying the library target.

```sh
$ etlas repl lib:repl-test

...

[1 of 1] Compiling Lib

Successful! One module loaded.

*Lib> someString
"someString"
it :: String
```

This form is useful when you have executable and test components as well in your project.

## Next Section

In the next section, we will proceed to see the full reference of the Eta REPL commands.
