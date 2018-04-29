# Workflow Tutorial

## Overview

We will start with a simple project and write everything by hand to understand the different aspects of Etlas project configuration. We will gradually update it get a feel for how updating your `.cabal` file will look like.

## Library Component

1. Create a folder for `simple-package`.

   ```sh
   $ mkdir simple-package
   ```

2. Create the following directory structure.

   ```
   simple-package
   - src
     - Simple
       - Internal.hs
       - Lib.hs
   - simple-package.cabal
   ```

3. Populate the files with the contents below.

   **simple-package.cabal**
   ```sh
   name:                simple-package
   version:             0.1.0.0
   build-type:          Simple
 
   library
     hs-source-dirs:    src
     exposed-modules:   Simple.Lib
     other-modules:     Simple.Internal
     build-depends:     base >=4.8 && <4.9
     default-language:  Haskell2010
   ```

   - `hs-source-dirs` is a space-separated list of directories in which to search for the specified modules.
   
     **Example**
     ```
       hs-source-dirs: src release developer
     ```
   
   - `exposed-modules` is a space-separated list of modules. These modules will be available for import by packages that depend on this library.

     **Example**
     ```
       exposed-modules: Simple.Lib
                        Simple.Lib2
                        Simple.Lib3
     ```
     
     Each module will be searched relative to the directories.
     
     If `hs-source-dirs: src src2` is present, then Etlas will attempt to find one of the following

     - `src/Simple/Lib.hs`
     - `src2/Simple/Lib.hs` 
     
     when attempting to search for the `Simple.Lib` module.

   - `other-modules` is identical to `exposed-modules`, except the modules listed are **not** available for import by packages that depend on this library.
   - `build-depends` is a comma-separated list of dependency constraints.
   
     **Example**
     ```
     build-depends: vector == 1.2.3
                  , containers == 1.2.*
                  , base >= 4.8 && < 4.11.0
                  , binary > 4.3
                  , bytestring < 1.2
                  , mtl
     ```
     
     As you can see you can use the standard comparison operators `>`, `<`, `<=`, `>=`, `==`, and the `&&` operator to specify constraints.

   **src/Simple/Lib.hs**
   ```eta
   module Simple.Lib (simpleString) where
 
   import Simple.Internal

   simpleString :: String
   simpleString = simpleInternalString
   ```

   **src/Simple/Internal.hs**
   ```eta
   module Simple.Internal (simpleInternalString) where

   simpleInternalString :: String
   simpleInternalString = "simpleString"
   ```

4. Build your project.

   ```sh
   $ etlas build
   ```

## Add a Flag

Suppose you wanted to make the build faster for development by turning off optimizations and turn on optimizations for a release version of your library. 

You can introduce a flag like so:

**simple-package.cabal**
```
name:                simple-package
version:             0.1.0.0
build-type:          Simple

flag developer
  description: Build without optimizations
  default: False
  manual: True

library
  hs-source-dirs:    src
  exposed-modules:   Simple.Lib
  other-modules:     Simple.Internal
  build-depends:     base >=4.8 && <4.9
  default-language:  Haskell2010
  if flag(developer)
    eta-options: -O0
  else 
    eta-options: -O2
```

And then build your project with the flag:

```
etlas build -fdeveloper
```

For more information on all the fields you can specify, like `eta-options`, you can go [here]().

For more information about how to conditionally configure your package, go [here]().

## Executable Component

We will now proceed to add an executable component that uses the library component we defined above.

1. Update your directory structure.

   ```
   simple-package
   - app
     - App.hs
     - App
       - Config.hs
   - src
     - Simple
       - Internal.hs
       - Lib.hs
   - simple-package.cabal
   ```

2. Populate the files with the contents below.

   **simple-package.cabal**
   ```
   name:                simple-package
   version:             0.1.0.0
   build-type:          Simple

   flag developer
     description: Build without optimizations
     default: False
     manual: True

   library
     hs-source-dirs:    src
     exposed-modules:   Simple.Lib
     other-modules:     Simple.Internal
     build-depends:     base >=4.8 && <4.9
     default-language:  Haskell2010
     if flag(developer)
       eta-options: -O0
     else 
       eta-options: -O2
   
   executable my-app
     hs-source-dirs: app
     main-is: App.hs
     other-modules: App.Config
     build-depends: base >=4.8 && <4.9 
                  , simple-package
     hs-source-dirs: src
   ```
   
   - You must specify a name for the `executable` component. In this case, it's `my-app`.
   - The fields of an executable component are nearly the same as a library component. An executable component will not have an `exposed-modules` field and will have a `main-is` field.
   - `main-is` should be a path to a source file that contains a `main` function, the entry point of your application. The file must either have no module declaration, in which case the file name can be anything. Otherwise, the module declaration must be `module Main` and the filename must be `Main.hs`. Note that the path you provide will be taken as relative to the paths provided in the `hs-source-dirs` field.
   
   **app/App.hs**
   ```eta
   import Simple.Lib
   import App.Config

   main :: IO ()
   main = do 
     putStrLn $ "Simple String: " ++ simpleString
     putStrLn $ "App Config: "    ++ configString
   ```

   **app/App/Config.hs**
   ```eta
   module App.Config (configString) where
   
   configString :: String
   configString = "fast: true"
   ```
   
3. Run the executable.

   ```
   etlas run
   ```
   
   Normally, the `run` command requires an executable target, but since there is only one executable, that executable will be selected.
   
## Executable With Arguments

We often want to send arguments to our apps. We demonstrate below 

1. Update the file contents shown below.

   **app/App.hs**
   ```eta
   import Simple.Lib
   import App.Config
   
   import System.Environment

   main :: IO ()
   main = do 
     args <- getArgs
     putStrLn $ "Arguments: "     ++ show args
     putStrLn $ "Simple String: " ++ simpleString
     putStrLn $ "App Config: "    ++ configString
   ```

2. Run the program with arguments.

   ```
   etlas run my-app -- 1 2 3 someOtherArg
   ```
   
   Note that in this case, we specified the target name `my-app` because we are sending in arguments as well. Moreover, the `--` is used a separator between Etlas arguments and your app's arguments.
   
## Test Component

We often want to test our code so Etlas provides a test component for that purpose.

1. Update your directory structure.

   ```
   simple-package
   - app
     - App.hs
     - App
       - Config.hs
   - src
     - Simple
       - Internal.hs
       - Lib.hs
   - test
     - Test.hs
   - simple-package.cabal
   ```

2. Update the file contents shown below.

   **simple-package.cabal**
   ```sh
   name:                simple-package
   version:             0.1.0.0
   build-type:          Simple

   flag developer
     description: Build without optimizations
     default: False
     manual: True

   library
     hs-source-dirs:    src
     exposed-modules:   Simple.Lib
     other-modules:     Simple.Internal
     build-depends:     base >=4.8 && <4.9
     default-language:  Haskell2010
     if flag(developer)
       eta-options: -O0
     else 
       eta-options: -O2
   
   executable my-app
     hs-source-dirs: app
     main-is: App.hs
     other-modules: App.Config
     build-depends: base >=4.8 && <4.9 
                  , simple-package
     hs-source-dirs: src
     
   test-suite simple-unit-tests
     type: exitcode-stdio-1.0
     hs-source-dirs: test
     main-is: Test.hs
     build-depends: base >=4.8 && <4.9 
                  , HUnit
                  , simple-package
   ```
   
   - `test-suite` stanzas are just like `executable` stanzas but they have an extra field, `type`.
   - For now, `type` should always have value `exitcode-stdio-1.0` which means that the test-suite application should return a non-zero exit code if any of the tests have failed. The example below uses `exitFailure` if the any of the tests have failed.
   
   **test/Test.hs**
   ```eta
   import Simple.Lib
   import Control.Monad
   import Test.HUnit
   import System.Exit

   main :: IO ()
   main = do
     results <- runTestTT $ TestCase $ 
       assertEqual "simpleString is valid" name ""
     when (failures results > 0) $ exitFailure
     print results
   ```

3. Run the tests.

   ```
   etlas new-test
   ```
   
   Note that `new-test` functions similar to `run` except that it doesn't take arguments. If you have multiple test suites, you must distinguish the test suite you want to run by passing it as a target argument.
   
   ```
   etlas new-test simple-unit-tests
   ```
   
   **NOTE:** The `new-test` command is experimental and will become the `test` command soon.

## Next Section

In the next section, we will cover dependency management.
