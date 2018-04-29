# Cabal File

## Overview

This entire section is devoted to taking you through all the available configuration options for a single package in its `.cabal` file.

A `.cabal` file consists of metadata fields and stanzas. The metadata field describe aspects of the package that do not affect how the package itself is built.

## Field Types

The tables in the following subsections contain a **Field Type** column that describes what values are allowed. We describe the field types below.

### build-type

A build type should always have the value `Simple`.

### boolean

A boolean can either be `True` or `False`.

### dependency-constraint

A dependency constraint is of the form `[package-name] [version-range]`.

**Examples**

- `bytestring > 1.0.0`
- `text >= 1.0.0 && < 2.0.0`
- `binary == 1.0.0`

### directory

A directory is a `filepath` that must necessarily point to a folder.

**Examples**

- `/usr/include`

### extension

A valid Eta language extension. Prefix `No` to disable the extension.

**Examples**

- `DataKinds`
- `NoDataKinds`

### filepath

A filepath is a valid path to a file on the filesystem.

**Examples**

- `/usr/include`
- `hello/myfolder`

### license

A license can take one of the following values:

- `GPL-[version]`
- `LGPL-[version]`
- `AGPL-[version]`
- `BSD2`
- `BSD3`
- `BSD4`
- `ISC`
- `MIT`
- `MPL-[version]`
- `Apache-[version]`
- `AllRightsReserved`
- `OtherLicense`

It can also be any other string, but in that case, it has no special treatment from Etlas since it won't be recognized.

**Examples**

- `GPL-1.0`
- `RandomLicense`

### list[X]

A list is a whitespace-separated sequence of character sequences which are of type `X`. You can add any amount of whitespace, including newlines, before and after the commas.

**Examples**

- `/path1/path2 /path3/path4` (`list[filepath]`)
- `1.2.3.4      4.5.6  7.8.9` (`list[version]`)

### comma-list[X]

A comma list is a comma-separated sequence of character sequences which are of type `X`. You can add any amount of whitespace, including newlines, between `X`s.

**Examples**

- `/path1/path2   ,    /path3/path4` (`list[filepath]`)
- `1.2.3.4, 4.5.6, 7.8.9` (`list[version]`)

### module

A module is a `.` separated sequence of alphanumeric character sequences that begin with a capital letter.

**Examples**

- `MyModule`
- `Hello.World`
- `A.B.C`

### maven-dependency

A Maven dependency is of the form `[group-id]:[artifact-id]:[version]` where

- `[group-id]` is the group ID of the Maven artifact
- `[artifact-id]` is the artifact ID of the Maven artifact
- `[version]` is the version of the Maven artifact

**Examples**

- `org.apache.commons:commons-lang3:3.7`
- `com.google.guava:guava:25.0-jre`

### maven-repo

A Maven repository is a location which Etlas can use to resolve a Maven dependency. It can be a URL or one of the keywords listed below.

| Keyword                    | Maven Repository URL                                     |
| -------------------------- | -------------------------------------------------------- |
| `central`                  | `https://repo1.maven.org/maven2/`                        |
| `javaNet1`                 | `http://download.java.net/maven/1/`                      |
| `sonatype:[type]`          | `https://oss.sonatype.org/content/repositories/[type]`   |
| `jcenter`                  | `https://jcenter.bintray.com/`                           |
| `bintray:[owner]:[repo]`   | `https://dl.bintray.com/[owner]/[repo]/`                 |

- `[type]` should be one of `public`, `snapshots`, or `releases`.
- `[owner]` should be the owner of the repository and `[repo]` should be the name of the Bintray repository.

**NOTE:** If you need to access a Maven repository that requires credentials, you can specify the URL in the form `http://[user]:[pass]@[repo-url]`.

**Examples**

- `jcenter`
- `central`
- `https://dl.bintray.com/theanilpaudel/android/`
- `bintray:theanilpaudel:android`

### package-name

A package name consists of alpha-numeric character sequences separated by `-`. Note that each alpha-numeric sequence **must** contain **at least one** alphabetic character.

**Examples**

- `sompackage`
- `my-package`
- `hello-123a`
- `h123-package`

### source-repository-type

A source repository type can be any one of the following:

- `git`
- `mercurial`
- `darcs`
- `svn`
- `cvs`
- `bazaar`
- `arch`
- `monotone` 

### stability

A stability can be any character sequence that describes the stability of the package.

**Examples**

- `alpha`
- `experimental`
- `provisional`
- `stable`

### strict-comma-list[X]

A strict comma list is just like a `comma-list` except whitespace is not allowed before or after the comma.

**Examples**

- `/path1/path2,/path3/path4` (`list[filepath]`)
- `1.2.3.4,4.5.6,7.8.9` (`list[version]`)

### test-suite-type

A test suite type describes the way a test should be run. Etlas only supports `exitcode-stdio-1.0`.

**Examples**

- `exitcode-stdio-1.0`

### package-executable

A package executable is of the form `[package-name]-[any]` where `[any]` is the name of the executable.

**Examples**

- `alex:alex`
- `happy:happy`

### version

A version consists of numeric sequences separated by `.`. 

**Examples**

- `1`
- `1.0.0`
- `12.1234.203`

### version-range

A version range consists of a comparison operator followed by a `version` 

Comparison operators:

- `>=`
- `>`
- `<`
- `<=`
- `==`

The conjunction operator `&&` can be used to combine multiple `version-range`s.

**Examples**

- `>= 1.0`
- `< 1.0 && > 0.5`
- `== 1.0.0`

## Metadata Fields

Other than `name`, `version`, and `build-type`, every other field is optional.

| Field                | Field Type            | Description                                                     |
| -------------------- | :-------------------: | ------------------------------------------------------------    |
| `name`               | `package-name`        | Name of the package                                             |
| `version`            | `version`             | Version of the package                                          |
| `cabal-version`      | `version-range`       | Versions of the Cabal specifications supported                  |
| `build-type`         | `build-type`          | Type of build used by this package                              |
| `license`            | `license`             | License type that pertains to the package                       |
| `license-file`       | `filepath`            | Path to the license file                                        |
| `license-files`      | `list[filepath]`      | List of paths to the license files that pertain to this package |
| `copyright`          | `any`                 | Copyright notice with holder and years held                     |
| `author`             | `any`                 | Author of the package                                           |
| `maintainer`         | `list[any]`           | List contact information of maintainers of the package          |
| `stability`          | `stability`           | Stability level of the package                                  |
| `homepage`           | `url`                 | Package homepage                                                |
| `bug-reports`        | `url`                 | URL where users should direct bug reports                       |
| `package-url`        | `url`                 | Location of a source bundle for the package                     |
| `synopsis`           | `any`                 | Short description of the package                                |
| `description`        | `any`                 | Long description of the package                                 |
| `category`           | `any`                 | Category of the package                                         |
| `data-files`         | `list[filepath]`      | List of files to be installed for runtime use by the package    |
| `data-dir`           | `directory`           | Directory where Etlas looks for data files to install           |
| `extra-tmp-files`    | `list[filepath]`      | List of additional files/directories to be removed by [clean]() |

## Library Stanza

The library stanza starts with a `library` keyword and all the fields of the stanza must be indented underneath. There can only be one library in a package and the name is taken from the `name` metadata field above.

The library stanza should contain the following fields:

| Field                | Field Type            | Description                                                  |
| -------------------- | :-------------------: | ------------------------------------------------------------ |
| `exposed-modules`    | `list[module]`        | A list of modules exported by this package                   |

The library stanza supports the [common fields](#common-fields) below.

### Example

```
name: hello

library
  exposed-modules: Hello
```

## Executable

The executable stanza starts with a `executable` keyword followed by the name of the executable and all the fields of the stanza must be indented underneath. There can any number of executables in a package.

The name of the executable can be anything, but it shouldn't contain spaces.

The executable stanza must contain the following fields:

| Field                | Field Type            | Description                                                  |
| -------------------- | :-------------------: | ------------------------------------------------------------ |
| `main-is`            | `filepath`            | The path to the `.hs` file containing the `Main` module      |

The executable stanza supports the [common fields](#common-fields) below.

### Example

```
executable my-executable
  main-is: Main.hs
```

## Test Suites

The test suite stanza starts with a `test-suite` keyword followed by the name of the test suite and all the fields of the stanza must be indented underneath. There can any number of test suites in a package.

The name of the test suite can be anything, but it shouldn't contain spaces.

The test suite stanza must contain the following fields:

| Field                | Field Type            | Description                                                  |
| -------------------- | :-------------------: | ------------------------------------------------------------ |
| `type`               | `test-suite-type`     | Type of test suite to execute                                |
| `main-is`            | `filepath`            | Path to the `.hs` file containing the `Main` module          |

The test suite stanza supports the [common fields](#common-fields) below.

### Example

```
test-suite my-test-suite
  type: exitcode-stdio-1.0
  main-is: Main.hs
```

## Common Fields

Common fields shared among library, executable, and test components for building and configuring those components.

| Field                       | Field Type                                  | Description                                                                               |
| --------------------------- | :-----------------------------------------: | ------------------------------------------------------------                              |
| `build-depends`             | `comma-list[dependency-constraint]`         | List of dependencies required for this package                                            |
| `other-modules`             | `list[module]`                              | List of modules used by the component but not exposed to users                            |
| `hs-source-dirs`            | `list[directory]`                           | Root directories for modules. <br/> **Default:** `.`                                      |
| `default-extensions`        | `list[extension]`                           | List of language extensions used by every module                                          |
| `build-tool-depends`        | `list[package-executable]`                  | List of programs needed to build this component                                           |
| `buildable`                 | `boolean`                                   | Is the component buildable? <br/> **Default:** `True`                                     |
| `eta-options`               | `list[any]`                                 | Arguments for the Eta compiler                                                            |
| `includes`                  | `list[filepath]`                            | A list of header files to be sent to the Eta preprocessor                                 |
| `install-includes`          | `list[filepath]`                            | A list of header files from this package to be installed.                                 |
| `include-dirs`              | `list[directory]`                           | List of directories to search for header files for the Eta preprocessor                   |
| `java-sources`              | `list[filepath]`                            | List of Java source file dependencies (`.java`, `.class`, `.jar`)                         |
| `cpp-options`               | `list[any]`                                 | Arguments for the Eta preprocessor                                                        |
| `maven-depends`             | `list[maven-dependency]`                    | List of Maven dependencies                                                                |
| `maven-repos`               | `list[maven-repo]`                          | List of Maven repositories for resolving Maven dependencies. <br/> **Default:** `central` |

## Package Flags

You can declare conditional flags to allow users to build different flavors of your package depending on their need.

You can declare a flag with the `flag` keyword followed by the name of the flag. The fields allowed for a flag are listed in the table below.

| Field                | Field Type            | Description                                                                                       |
| -------------------- | :-------------------: | ------------------------------------------------------------                                      |
| `description`        | `any`                 | The description of this flag                                                                      |
| `default`            | `boolean`             | The default value of the flag. <br/> **Default:** `True`                                          |
| `manual`             | `boolean`             | The value of the flag when the flag is passed as an argument to Etlas. <br/> **Default:** `False` |

Check out the section on [Conditional Configuration](./conditional-configuration) for more information on how to use these flags.

### Example

```
flag developer
  description: operate in developer mode
  default: False
  manual: True
```

## Source Repositories

You can declare the location of your package's source if it's losted in a source control management (SCM) system like Git, Mercurial, and so on.

You can declare a source repository with the `source-repository` keyword followed by either `this` or `head`.

The fields allowed for a source repository are listed in the table below.


| Field                | Field Type                    | Description                                                    |
| -------------------- | :---------------------------: | ------------------------------------------------------------   |
| `type`               | `source-repository-type`      | Name of the SCM used for the repository                        |
| `location`           | `url`                         | Location of the repository                                     |
| `branch`             | `any`                         | Branch for the respective SCM                                  |
| `tag`                | `any`                         | Tag for the respective SCM                                     |
| `subdir`             | `directory`                   | Subfolder of the repository that is a standalone Etlas package |
| `module`             | `any`                         | Required for the CVS and should not be used otherwise          |

### Example

```
source-repository this
  type: git
  location: https://github.com/Jyothsnasrinivas/eta-jdbc
  tag: 0.2.0.0
```

## Next Section

In the next section, we will learn how to configure our packages conditionally.
