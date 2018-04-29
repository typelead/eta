# The Build Process

## Overview

In the last section, we ran the `etlas run` command which runs the `etlas build` command for you and executes your program's launcher script as a final step. In this section, we will cover in detail what happens when you build a project.

## Teminology

A **project** is a group of packages that may have inter-dependencies.

A **package** is a group of source files coupled with a set of flags that used to compile them into a single unit in a particular way.

A **local package** is a package that has its source files present in a folder on your local filesystem. The source files will be monitored for changes and the corresponding package will be rebuilt as needed.

A **external package** is a package that has its source files downloaded from a remote host, extracted to a temporary folder, and built once and for all.

## Build Process

The diagram below gives you a bird's eye view of the build process.

<img alt="Eta Packages" src="/images/etlas-overview.svg" style="height:800px; padding-bottom: 40px;">

At a high-level, Etlas will take stock of all the explicit and implicit build inputs and generate a plan for how to execute the build. This plan describes which build artifacts can be loaded from the store and which artifacts need to be built or rebuilt. This process is extremely efficient and will only rebuild the changed targets and their dependents.

## Inputs

<img alt="Eta Packages" src="/images/inputs-overview.svg" style="height:500px; padding-bottom: 40px;">

### Local Packages Configuration

The main inputs to a build consist of a group of local packages. These are packages that have sources extracted to a local folder and are monitored for any changes by the user. You typically deal with single package projects.

The configuration for local packages, as well as external packages, are specified with a `cabal.project` file. You are not required to specify a `cabal.project` file and by default, it will have the following contents:

```
packages: ./*.cabal
optional-packages: ./*/*.cabal
```

This means that it will pick up any `.cabal` files in the current folder as well as `.cabal` files in any subfolders.

You can find a full specification of the `cabal.project` file [here]().

### Individual Package Configuration

Individual packages are configured with a `[package].cabal`, an example which we saw in the last section. Cabal files specify the Eta modules which should be compiled, the flags that should be sent to the compiler, and many other details.

You can find a full specification of the `.cabal` file [here]().

## Eta Hackage

Etlas is the build tool for the Eta programming language, but because of similarities with Haskell, a lot of existing Haskell packages from Hackage can be compiled with minimal changes. These changes are stored in the form of a `.patch` file created by the `git format-patch` command.

Etlas will automatically apply the patches from [eta-hackage](https://github.com/typelead/eta-hackage) when necessary.

## Coursier

As Eta is a JVM language, we would like to take advantage of the JVM ecosystem. For that purpose, Etlas uses [Coursier](https://github.com/coursier/coursier) to pull dependencies from Maven repositories and provides fields in the `.cabal` file to configure coursier. See the [.cabal file reference]() for more details.

## Etlas Store

<img alt="Eta Packages" src="/images/etlas-store.svg" style="height:600px; padding-bottom: 40px;">

For external packages that you build with Etlas, Etlas will **generate a hash** of the inputs used to build the package and will stash the build artifacts into the **Etlas Store** which identifies build artifacts by their package name, package version, and hash.

This allows you to use **multiple versions** of a package and also **multiple variants** of the same package compiled using different flags to be used across different projects on the same system without facing dependency hell.

The **Etlas Store** behaves very similarly to the [Nix](https://nixos.org/nix/) store.

## Dependency Resolution

Etlas has a concept of **upper** and **lower** bounds for a given dependency to pin down which versions support the API that you need for your code. This information is used during **dependency resolution** which assigns a version to use for every transitive dependency of your project. The outcome of the process is an **install plan** which specifies the exact packages and configurations that are required for build.

## Elaborated Install Plan

After coming up with a basic install plan, Etlas will check the state of all the files and directories it is monitoring to do intelligent up-to-date checking. Given this information, Etlas will formulate a plan of action - which packages should be built (or rebuilt), and how they should be built. The resulting plan is called the **elaborated install plan**.

This phase is responsible for computing the hashes for all the packages (both local and external) involved in the build and checking whether a package with that hash is available in the Etlas Store.

## Build

Now that the elaborated install plan has been established, Etlas will proceed to run all the steps it needs to in order to complete the build. This may involve preprocessing source files using tools like [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/) before invoking the Eta compiler to complete the build.

## Eta Compiler

Etlas will invoke the Eta compiler with the flags derived from the package configuration files (both the `.cabal` file and the `cabal.project`) and proceed to build the output which is a JAR file containing the compiled JVM bytecode.

## Outputs

A JAR file is the overall output of the build. Other outputs include launcher scripts for executable projects and interface files that are used for storing type information of the exported functions of the modules of a package.

## Next Section

In the next section, we will explore different ways to build Etlas projects.
