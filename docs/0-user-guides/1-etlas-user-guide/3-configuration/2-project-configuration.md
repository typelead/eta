# Project Configuration

## Overview

In the [build process](/docs/user-guides/etlas-user-guide/basics/the-build-process) section, we mentioned the **elaborated install plan** which is a detailed plan on how exactly to build all the packages in your build. There are cases in which we want to modify the configuration for certain packages in our build.

## cabal.project Overview

While the `.cabal` specifies how to build a single package, a `cabal.project` specifies how to build all the packages in your project. In most simple use cases, you never have to worry about this file since it has smart defaults. The default is to automatically pick up packages - subfolders in your project with `.cabal` files.

## Optimized Packages

When you're building a release version of your app, you may want to build with optimizations turned on for all your dependencies.

**cabal.project**
```
packages: .

all-packages
  optimization: 2
```

The `all-packages` section apply configuration to **all** packages in your build, both local and external.

## Customizing a Single Package

Say you want to enable or disable a flag for a particular package in your build. You can use the `package` section.

```
packages: .

package aeson
  flag: +old-locale
```

## Freezing Your Dependencies

To ensure full reproducibility of the build for your app, it is a good idea to freeze the dependencies so that your project build occurs in exactly the same way every time, independent of Hackage/Etlas Index changes.

```
$ etlas freeze
```

This will create a file called `cabal.project.freeze` in the current folder that contains a list of hard constraints for all the packages in your build. A freeze file ensures that the outcome dependency resolution is stable.

In practice, you should **not** freeze dependencies for libraries.

## Next Module

In the next module, we will present a full reference of `.cabal` files and `cabal.project` files.
