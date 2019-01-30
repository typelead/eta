# Cabal Project File

## Overview

This entire section is devoted to taking you through all the available configuration options for the packages in your build with the `cabal.project` file.

A `cabal.project` file allows you to select sets of packages in your build (both local and external packages) and apply configuration to them. Note that most flags of `etlas build` will have a corresponding entry in the `cabal.project`, but a `cabal.project` gives you much more flexibility.

### Alternatives

Note that `cabal.project.local` and `cabal.project.freeze` follow the same specification, but they are meant for different purposes. 

- `cabal.project.local` is not meant to be checked into source control and is meant for local, ad-hoc changes to the build. 
- `cabal.project.freeze` is meant to be checked in if the project is an end-user executable and you want to keep the dependency resolution stable across platforms.

## Field Types

The field types defined in the [Cabal File](./cabal-file#field-types) section apply in this section as well. Additional field types are defined below.

### component-name

A component name represents the name of a component like the name of an executable component for example.

### component-type

A component type can take one of the following values:

- `lib`
- `exe`
- `test`
- `bench`

### eta-version

An Eta version is of the form `[version]b[nonnegative-integer]`.

**Examples**

- `0.8.6b4`

### filepath-glob

A filepath glob is just like a `filepath` but it accepts `*` in the path to allow you to match multiple filepaths at once.

**Examples**

- `./*.cabal`
- `./hello.cabal`
- `hello.cabal`

### flag-specifier

A flag specifier is either of the form `+[flagname]` or `-[flagname]` where `+` means to enable the flag and `-` means to disable the flag.

**Examples**

- `+developer`
- `-slow`

### index-timestamp

An index timestamp can either have the value `HEAD` or an [ISO 8601 Date/Time Format](https://en.wikipedia.org/wiki/ISO_8601).

**Examples**

- `HEAD`
- `2018-04-28T11:00:19Z`

### integer

An integer is can be any whole number that is either positive or negative.

**Examples**

- `-1`
- `0`
- `1`

### nonnegative-integer

An nonnegative integer is any whole number, including 0.

**Examples**

- `0`
- `123`

### num-cpus

A num cpus can be a `positive-integer` or have the value `$ncpus`. The meaning of `$ncpus` is the number of processor cores on the system.

### optimization

An optimization can take one of the following values:

- `0`
- `1`
- `2`

### package-db

A package database can be one of the following:

- `global` - Use the global package database (located in system directory)
- `user` - Use the user package database (located in `HOME` folder)
- `clear` - Reset the package database so far
- `[directory]` - Path to the package database

### path-template

A path template that can additionally use the specialized keywords defined in the table below in the path.

| Template Variable | Description                                             |
| ----------------- | -----------                                             |
| `$prefix`         | The root of all the Etlas-related data                  |
| `$libname`        | The name of the library                                 |
| `$bindir`         | Where binaries are stored                               |
| `$libdir`         | Where libraries are installed                           |
| `$datadir`        | Where data files are installed                          |
| `$docdir`         | Where documentation is installed                        |
| `$htmldir`        | Where the html documentation is installed               |
| `$pkg`            | Name of package                                         |
| `$version`        | Version of package                                      |
| `$pkgid`          | Name and version of package, e.g. `binary-1.0`          |
| `$compiler`       | Name of compiler                                        |
| `$os`             | Current operating system                                |
| `$arch`           | Current architecture                                    |
| `$abitag`         | Tag that represents Application Binary Interface (ABI)  |
| `$abi`            | Hash that represents Application Binary Interface (ABI) |

### positive-integer

An positive integer is any whole number, not including 0.

**Examples**

- `1`
- `123`

### scoped-package

A scoped package can be of the form `[package-name]` or `[package-name]:[package-name]`. It can also take the values `all` or `none`.

**Examples**

- `bytestring`
- `all`
- `binary:bytestring`
- `none`

### scoped-packages

Scoped packages can be `none`, `all`, or `strict-comma-list[scoped-package]`.

**Examples**

- `all`
- `none`
- `bytestring`
- `bytestring,binary:bytestring`

### transport-type

A transport type should be one of the following:

- `curl`
- `wget`
- `powershell`
- `plain-http`

### user-constraint

A user constraint is of the form `[package-name] [version-range]` or `[package-name] list[flag-specifier]`.

**Examples**

- `bytestring == 0.1.2.3`
- `bytestring +fast -developer`

### verbosity

A verbosity is a number that can be one of the following values:

- `0`
- `1`
- `2`
- `3`

## Local Packages

The following top-level options specify what the local packages of a project are:

| Field                     | Field Type                                 | Description                                                                          |
| ------------------------- | :----------------------------------------: | ------------------------------------------------------------                         |
| `packages`                | `list[filepath-glob]`                      | List of package paths that should be included in the build. <br/> **Default:** `./*.cabal` |
| `optional-packages`       | `list[filepath-glob]`                      | Like `packages` but does not require the paths to exist. <br/> **Default:** `./*/*.cabal`  |
| `extra-packages`          | `comma-list[dependency-constraint]`        | Additional named packages to install to the Etlas Store                              |

### Example

**cabal.project**
```
packages: . ./app
optional-packages: ./app/custom-fork
extra-packages: bytestring > 1.0.0
```

## Global Configuration

The following top-level configuration options are not specific to any package:

| Field                   | Field Type                 | Description                                                                                                                          |
| ----------------------- | :------------------------: | -------------------------------------------------------------------------------                                                      |
| `verbose`               | `verbosity`                | Controls the verbosity of `etlas` commands, higher is more verbose. <br/> **Default:** `1`                                           |
| `jobs`                  | `num-cpus`                 | Run the given number of jobs simultaneously when building. <br/> **Default:** `1`                                                    |
| `keep-going`            | `boolean`                  | When true, continue to build other unaffected packages, after a build failure. <br/> **Default:** `False`                            |
| `builddir`              | `directory`                | Specifies the directory where build artifacts should be stored. <br/> **Default:** `dist`                                            |
| `project-file`          | `filepath`                 | Specifies the path to the project file which is used for the rest of the top-level configuration. <br/> **Default:** `cabal.project` |
| `select-eta`            | `eta-version`              | Use an Etlas-managed version of Eta.                                                                                                 |
| `auto-update`           | `boolean`                  | Whether to update the package indices automatically. <br/> **Default:** `True`                                                       |
| `send-metrics`          | `boolean`                  | Whether to send metrics to help aid the future development of Eta. <br/> **Default:** `False`                                        |
| `http-transport`        | `transport-type`           | Transport to be used when making http(s) requests. <br/> **Default:** `curl`                                                         |
| `ignore-expiry`         | `boolean`                  | Ignore expiry dates on metadata from Hackage. <br/> **Default:** `False`                                                             |
| `remote-repo-cache`     | `directory`                | Location where source tarballs downloaded from remote repositories are stored. <br/> **Default:** `~/.etlas/packages`                |
| `logs-dir`              | `directory`                | Location where build logs are stored. <br/> **Default:** `~/.etlas/logs`                                                             |
| `patches-dir`           | `directory`                | Directory where patches are stored. <br/> **Default:** `~/.etlas/patches`                                                            |
| `binaries-dir`          | `directory`                | Directory where downloaded binary executables and packages are stored. <br/> **Default:** `~/.etlas/binaries`                 |
| `build-summary`         | `path-template`            | Location where the build summary is stored. <br/> **Default:** `~/.etlas/logs/build.log`                                             |

## Solver Configuration

The following settings control the behavior of the dependency solver. All the fields are commas seperated

| Field                                        | Field Type                                                  | Description                                                                                                                                                                   |
| -------------------------------------------- | :---------------------------------------------------------: | ------------------------------------------------------------                                                                                                                  |
| `constraints`                                | `comma-list[user-constraint]`                               | Add version or flag constraints for dependencies.                                                                                                                             |
| `preferences`                                | `comma-list[dependency-constraint]`                         | Dependency constraints are taken into account, but loosely followed.                                                                                                          |
| `allow-newer`                                | `scoped-packages`                                           | Ignore upper bounds for the list of scoped packages. <br/> **Default:** `none`                                                                                                |
| `allow-older`                                | `scoped-packages`                                           | Ignore lower bounds for the list of scoped packages. <br/> **Default:** `none`                                                                                                |
| `index-state`                                | `index-timestamp`                                           | Set the source package index state to the given value. <br/> **Default:** `HEAD`                                                                                              |
| `max-backjumps`                              | `integer`                                                   | Maximum number of backtracking steps allowed while solving. <br/> `-1` to allow unlimited backtracking <br/> `0` to disable backtracking completely <br/> **Default:** `2000` |
| `reorder-goals`                              | `boolean`                                                   | Enable the solver to reorder goals according to certain heuristics. <br/> **Default:** `False`                                                                                |
| `count-conflicts`                            | `boolean`                                                   | Speed up solving by preferring goals that are involved in a lot of conflicts. <br/> **Default:** `True`                                                                       |
| `strong-flags`                               | `boolean`                                                   | Do not defer flag choices. <br/> **Default:** `False`                                                                                                                         |
| `allow-boot-library-installs`                | `boolean`                                                   | Allow `base`, `ghc-prim`, `integer-simple`, `integer-gmp`, and `template-haskell` to be installed or upgraded. <br/> **Default:** `False`                                     |

## Package Configuration

The following options can affect individual packages or sets of packages. 

### Top-Level

If package options are specified at the top-level, they apply to all **local** packages.

**Example**

```
optimization: 2
```

### all-packages

If they are applied in an `all-packages` stanza, it applies to both **local** and **external** packages.

**Example**

```
all-packages
  optimization: 2
```

### package

If they are applied in a `package` stanza, they will apply to the package listed after the `package` keyword. Both **local** and **external** package names may be specified.

**Example**

```
package bytestring
  optimization: 2
```

### Option Listing

| Field                | Field Type                     | Description                                                                                                    |
| -------------------- | :----------------------------: | ------------------------------------------------------------                                                   |
| `flags`              | `list[flag-specifier]`         | Configure flag assignments for the package                                                                     |
| `optimization`       | `optimization`                 | Build with optimization at the level specified. <br/> **Default:** `1`                                         |
| `tests`              | `boolean`                      | Enable test suites. <br/> **Default:** `False`                                                                 |
| `benchmarks`         | `boolean`                      | Enable benchmarks. <br/> **Default:** `False`                                                                  |
| `run-tests`          | `boolean`                      | Run the package test suite upon installation and fail the build if it doesn't pass. <br/> **Default:** `False` |
| `verify`             | `boolean`                      | Verifies the whether the output JAR files are valid                                                            |
| `debug-info`         | `boolean`                      | Generate extra information in the compiled binary that supports debugging. <br/> **Default:** `False`          |

## Documentation Configuration

| Field                    | Field Type            | Description                                                                       |
| ------------------------ | :-------------------: | ------------------------------------------------------------                      |
| `documentation`          | `boolean`             | Enables documentation build. <br/> **Default:** `False`                           |
| `doc-index-file`         | `path-template`       | Path to central index of API documentation                                        |
| `docs-html`              | `boolean`             | Build HTML documentation. <br/> **Default:** `True`.                              |
| `docs-html-location`     | `path-template`       | Specify template for the location of HTML documentation for prerequisite packages |
| `docs-executables`       | `boolean`             | Generate documentation for all executables. <br/> **Default:** `False`            |
| `docs-tests`             | `boolean`             | Generate documentation for all tests. <br/> **Default:** `False`                  |
| `docs-benchmarks`        | `boolean`             | Generate documentation for all benchmarks. <br/> **Default:** `False`             |
| `docs-all`               | `boolean`             | Generate documentation for all components. <br/> **Default:** `False`             |
| `docs-internal`          | `boolean`             | Generate documentation for hidden modules and symbols. <br/> **Default:** `False` |
| `docs-css`               | `filepath`            | Path to CSS file that styles the generated documentation                          |
| `docs-contents-location` | `url`                 | URL to be used as the location for the contents page                              |
| `docs-keep-temp-files`   | `boolean`             | Keep temporary files <br/> **Default:** `False`                                  |
