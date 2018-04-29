# Initializing

## Overview

You can initialize an Etlas project using `etlas init`. The `init` command is interactive and you can find an example below.

## Interactive Example

1.  Create a new directory called `eta-first` and enter it.

    ```sh
    $ mkdir eta-first
    $ cd eta-first
    ```

2.  Initialize the project with Etlas.

    *Note:* `<[text-here]>` means to type `[text-here]` and `<Enter>` means to press the Enter key.

    ```sh
    $ etlas init
    Package name? [default: eta-first] <Enter>
    Package version? [default: 0.1.0.0] <Enter>
    Please choose a license:
      1) GPL-2
      2) GPL-3
      3) LGPL-2.1
      4) LGPL-3
      5) AGPL-3
      6) BSD2
    * 7) BSD3
      8) MIT
      9) ISC
      10) MPL-2.0
      11) Apache-2.0
      12) PublicDomain
      13) AllRightsReserved
      14) Other (specify)
    Your choice? [default: BSD3] <Enter>
    Author name? [default: ...] <Enter>
    Maintainer email? [default: ...] <Enter>
    Project homepage URL? <Enter>
    Project synopsis? <Enter>
    Project category:
    * 1) (none)
      2) Codec
      3) Concurrency
      4) Control
      5) Data
      6) Database
      7) Development
      8) Distribution
      9) Game
      10) Graphics
      11) Language
      12) Math
      13) Network
      14) Sound
      15) System
      16) Testing
      17) Text
      18) Web
      19) Other (specify)
    Your choice? [default: (none)] <Enter>
    What does the package build:
      1) Library
      2) Executable
    Your choice? <2>
    What is the main module of the executable:
    * 1) Main.hs (does not yet exist, but will be created)
      2) Main.lhs (does not yet exist, but will be created)
      3) Other (specify)
    Your choice? [default: Main.hs (does not yet exist, but will be created)] <Enter>
    Source directory:
    * 1) (none)
      2) src
      3) Other (specify)
    Your choice? [default: (none)] <2>
    What base language is the package written in:
    * 1) Haskell2010
      2) Haskell98
      3) Other (specify)
    Your choice? [default: Haskell2010] <Enter>
    Add informative comments to each field in the cabal file (y/n)? [default: n] <Enter>

    Guessing dependencies...

    Generating LICENSE...
    Generating Setup.eta...
    Generating ChangeLog.md...
    Generating src/Main.hs...
    Generating eta-first.cabal...
    ```

    The project structure should look like this:

    ```sh
    eta-first
    - src
      - Main.hs
    - ChangeLog.md
    - LICENSE
    - Setup.hs
    - eta-first.cabal
    ```

3.  Add the files `Main.hs` and `Primes.hs` in `src/` as shown below.

    ### Main.hs

    ```eta
    module Main where

    import Primes

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)
    ```

    ### Primes.hs

    ```eta
    module Primes where

    primes = filterPrime [2..]
      where filterPrime (p:xs) =
              p : filterPrime [x | x <- xs, x `mod` p /= 0]
    ```

4.  Update `eta-first.cabal`, adding an `other-modules: Primes` field.

    ### eta-first.cabal
    ```sh
    name:                eta-first
    version:             0.1.0.0
    license:             BSD3
    license-file:        LICENSE
    author:              [your name]
    maintainer:          [your email]
    build-type:          Simple
    extra-source-files:  ChangeLog.md
    cabal-version:       >=1.10

    executable eta-first
      main-is:             Main.hs
      other-modules:       Primes
      build-depends:       base >=4.8 && <4.9
      hs-source-dirs:      src
      default-language:    Haskell2010
    ```

    Any additional modules you add to the project should be added at the same indentation level as the `Primes` entry, but below it.

5.  Build and run your program.

    ```sh
    $ etlas run
    ```

## Next Section

Now that we've initialized and built our first project, the next module will cover the fine details of building a project.
