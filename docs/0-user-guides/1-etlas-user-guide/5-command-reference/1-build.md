# Build

## Overview

The `build` command is runs the entire build process and does incremental rebuilding as much as it can. Prefer to use [cabal.project](/docs/user-guides/etlas-user-guide/advanced-features/cabal-project-file) to configure any changes instead of sending flags to this command if the configuration is permanent. Prefer to use `cabal.project.local` for temporary changes.

## Usage

```sh
etlas build [TARGETS] [FLAGS]
```

- `[TARGETS]` is an optional list of targets that follow the form below. If none are specified, by default it builds the library and executable components of all the local packages.
- `[FLAGS]` is an optional list of flags listed below.

## Specifying Targets

The `build` command accepts targets of the following form:

| Target Form                                        | Description                                                                  |
| -----------                                        | -----------                                                                  |
| `[component-type]`                                 | Build all components of the given type                                       |
| `[package-name]`                                   | Build all the components of the given package                                |
| `[package-name]:[component-type]`                  | Build all components of the given type in the given package                  |
| `[package-name]:[component-type]:[component-name]` | Build the given component of the given package                               |
| `all`                                              | Build all components of all packages included in the build                   |
| `all:[component-type]`                             | Build all components of the given type in all packages included in the build |

**Examples**

```sh
etlas build mypackage
```

```sh
etlas build mypackage:exe:myexe
```

```sh
etlas build all:exe
```

## Basic Flags

| Flags                           | Flag Type               | Description                                                                                                                                  |
| ----------------------------    | :------------------:    | ----------------------------------------------------                                                                                         |
| `-v`, `--verbose`               | `verbosity`             | Control verbosity, higher level emits more messages                                                                                          |
| `--dry-run`                     | `switch-flag`           | Do not install anything, only print what would be installed                                                                                  |
| `--count-conflicts`             | `switch-flag`           | Try to speed up solving by preferring goals that are involved in a lot of conflicts (default).                                               |
| `--independent-goals`           | `switch-flag`           | Treat several goals on the command line as independent. <br/> If several goals depend on the same package, different versions can be chosen. |
| `--shadow-installed-packages`   | `switch-flag`           | If multiple package instances of the same version are installed, treat all but one as shadowed.                                              |
| `--strong-flags`                | `switch-flag`           | Do not defer flag choices.                                                                                                                   |
| `--allow-boot-library-installs` | `switch-flag`           | Allow etlas to install `base`, `ghc-prim`, `integer`, and `template-haskell`.                                                                |
| `--upgrade-dependencies`        | `switch-flag`           | Pick the latest version for all dependencies, rather than trying to pick an installed version.                                               |
| `--dependencies-only`           | `switch-flag`           | Install just the dependencies necessary to build the given packages                                                                          |
| `--one-shot`                    | `switch-flag`           | Do not record the packages in the world file.                                                                                                |
| `--run-tests`                   | `switch-flag`           | Run package test suites during installation.                                                                                                 |
| `-j`, `--jobs`                  | `num-cpus`              | Run given number of jobs simultaneously.                                                                                                     |
| `--keep-going`                  | `switch-flag`           | After a build failure, continue to build other unaffected packages                                                                           |
| `--offline`                     | `switch-flag`           | Don't download packages from the Internet                                                                                                    |
| `--cabal-file`                  | `filepath`              | use this Cabal file                                                                                                                          |
| `-f`, `--flags`                 | `flag-specifier`        | Perform flag assignment for the given flags                                                                                                  |
| `--constraint`                  | `user-constraint`       | Specify constraints on a package (version, installed/source, flags)                                                                          |
| `--preference=CONSTRAINT`       | `dependency-constraint` | Specify preferences (soft constraints) on the version of a package                                                                           |
| `--build-summary`               | `path-template`         | Save build summaries to file                                                                                                                 |
| `--build-log`                   | `path-template`         | Log all builds to file                                                                                                                       |
| `--project-file`                | `filepath`              | Set the name of the `cabal.project` file to search for in parent directories                                                                 |
| `--enable-uberjar-mode`         | `switch-flag`           | Enable building a standalone JAR (uberjar)                                                                                                   |
| `--disable-uberjar-mode`        | `switch-flag`           | Disable building a standalone JAR (uberjar)                                                                                                  |
| `-O`, `--enable-optimization`   | `optimization`          | Build with optimization at the given level                                                                                                   |
| `--disable-optimization`        | `switch-flag`           | Build without optimization                                                                                                                   |
| `--enable-debug-info`           | `optimization`          | Emit debug info according to the debugging level provided                                                                                    |
| `--disable-debug-info`          | `switch-flag`           | Don't emit debug info                                                                                                                        |
| `--enable-deterministic`        | `switch-flag`           | Enable being as deterministic as possible (used by the test suite)                                                                           |
| `--disable-deterministic`       | `switch-flag`           | Disable being as as deterministic as possible (used by the test suite)                                                                       |
| `--enable-tests`                | `switch-flag`           | Enable dependency checking and compilation for test suites listed in the package description file.                                           |
| `--disable-tests`               | `switch-flag`           | Disable dependency checking and compilation for test suites listed in the package description file.                                          |
| `--enable-benchmarks`           | `switch-flag`           | Enable dependency checking and compilation for benchmarks listed in the package description file.                                            |
| `--disable-benchmarks`          | `switch-flag`           | Disable dependency checking and compilation for benchmarks listed in the package description file.                                           |
| `--allow-newer`                 | `scoped-packages`       | Ignore upper bounds in all dependencies or those specified                                                                                   |
| `--allow-older`                 | `scoped-packages`       | Ignore lower bounds in all dependencies or those specified                                                                                   |
| `--enable-verify`               | `switch-flag`           | Enable verification the generated JAR files                                                                                                  |
| `--disable-verify`              | `switch-flag`           | Disable verification the generated JAR files                                                                                                 |

## Installation Directories

| Flags                        | Flag Type            | Description                                               |
| ---------------------------- | :------------------: | ----------------------------------------------------      |
| `--prefix`                   | `directory`          | Set the prefix in preparation of installation             |
| `--bindir`                   | `directory`          | installation directory for executables                    |
| `--libdir`                   | `directory`          | installation directory of libraries                       |
| `--libsubdir`                | `directory`          | subdirectory of libdir in which libs are installed        |
| `--dynlibdir`                | `directory`          | installation directory for dynamic libraries              |
| `--libexecdir`               | `directory`          | installation directory for dynamic executables            |
| `--datadir`                  | `directory`          | installation directory for read-only data                 |
| `--datasubdir`               | `directory`          | subdirectory of datadir in which data files are installed |
| `--docdir`                   | `directory`          | installation directory for documentation                  |
| `--htmldir`                  | `directory`          | installation directory for HTML documentation             |
| `--etadocdir`                | `directory`          | installation directory for etadoc interfaces              |
| `--sysconfdir`               | `directory`          | installation directory for configuration files            |

## Documentation

| Flags                        | Flag Type            | Description                                                 |
| ---------------------------- | :------------------: | ----------------------------------------------------        |
| `--enable-documentation`     | `switch-flag`        | Enable building of documentation                            |
| `--disable-documentation`    | `switch-flag`        | Disable building of documentation                           |
| `--doc-index-file`           | `path-template`      | Path to central index of API documentation                  |
| `--docs-html`                | `switch-flag`        | Generate HTML documentation (the default)                   |
| `--docs-html-location`       | `url`                | Location of HTML documentation for pre-requisite packages   |
| `--docs-executables`         | `switch-flag`        | Run etadoc for executables targets                          |
| `--docs-tests`               | `switch-flag`        | Run etadoc for test suite targets                           |
| `--docs-benchmarks`          | `switch-flag`        | Run etadoc for benchmark targets                            |
| `--docs-all`                 | `switch-flag`        | Run etadoc for all targets                                  |
| `--docs-internal`            | `switch-flag`        | Run etadoc for internal modules and include all the symbols |
| `--docs-css`                 | `filepath`           | Use the given path as the etadoc stylesheet                 |
| `--docs-contents-location`   | `url`                | Set the given URL as the location for the contents page     |
