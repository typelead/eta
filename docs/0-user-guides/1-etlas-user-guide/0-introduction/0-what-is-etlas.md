# What is Etlas?

## Overview

Etlas is the build tool for the Eta programming language that is based on Haskell's [Cabal](https://www.haskell.org/cabal/) build tool. It allows you to build, run, and test your Eta projects and fetches dependencies automatically. It understands how to build Haskell packages from [Hackage](https://hackage.haskell.org/) and applies patches from the [Eta Hackage](https://github.com/typelead/eta-hackage) patch repository to make them compatible with Eta. Packages that are written for Eta are uploaded to [Etlas Index](https://github.com/typelead/etlas-index), which Etlas picks up by default.

Etlas also provides basic support for pulling in Maven dependencies allowing you to take advantage of existing JVM libraries. However, if you are working on a project with a sizable codebase that spans across multiple JVM languages, it is recommend to use the [Gradle plugin](/docs/user-guides/gradle-user-guide). It uses Etlas underneath, but does not require you to know anything about it.

## Next Section

In the next section, we will discuss the philosophy of Etlas.
