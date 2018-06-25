# Build Components

## Overview

We gave a simplified definition of a package in the last section. In this section, we will expand on the definition and provide different examples of a package.

## Terminology

A **package** is a group of **components**, each of which is individually configured with their own sets of source files, compiler flags, and so on.

There are three types of components:

- **library**
- **executable**
- **test**

A single package can contain at most one library and any number of executables and tests.

A **stanza** is a block in the `.cabal` file that delimits a component. All the configuration for a component must be indented underneath the stanza declaration.

## Library

A library component is configured by a library stanza. A library component must have an **exposed-modules** field which denotes the public API of the library. The **other-modules** field corresponds to modules which are internal and cannot be imported by the dependent packages.

## Executable

An executable component is configured by an executable stanza. It must have a **main-is** field which specifies the name of the file relative to the specified **hs-source-dirs** field that contains the `main` function, the entry point to an Eta program. The modules that the component depends on must be specified in the **other-modules** field.

### Example

The `eta-first` project in [Initializing](./initializing) already shows an example.

## Test

An test component is configured by an test stanza. It must have a **main-is** field which specifies the name of the file relative to the specified **hs-source-dirs** field that contains the `main` function, the entry point to an Eta program. The modules that the component depends on must be specified in the **other-modules** field.

A test component must also have a **type** field which currently only has one valid value: `exitcode-stdio-1.0`.

By default, the test components of a package are **not** built by default. You can use the `--enable-tests` flag when building to enable them, or put `tests: True` in your `cabal.project` file.

## Next Section

In the next 
We will now move on to the section on build configuration.
