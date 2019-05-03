# Compiler Configuration

## Overview

The Eta compiler takes many command line options that you may want to use for your SBT build. We provide an DSL to allow you to configure compiler options easily.

The following table lists the settings that are available for the `Eta[Type]` configurations:


| Property          | Type          |
| --------          | ----          |
| `language`        | `String`      |
| `extensions`      | `Seq[String]` |
| `ghcOptions`      | `Seq[String]` |
| `cppOptions`      | `Seq[String]` | 
| `installIncludes` | `Seq[String]` | 
| `includeDirs`     | `Seq[File]`   | 

## language

This specifies the language to use for the build. A language consists of a set of predefined language extensions are are defined by the compiler. Currently, there are only two values you can put for this field:

- `"Haskell98"` 
- `"Haskell2010"`

There are corresponding constants:

```scala
val Haskell98 = "Haskell98"
val Haskell2010 = "Haskell2010"
```

**Default:** `Haskell2010` (`"Haskell2010"`)

Additionally you can obtain the list of supported languages in this build using the SBT command `eta-languages`, e.g.:

```shell
$ sbt eta-languages
```

### Example

Sets the language to `Haskell98` for all the Eta source files in the `EtaLib` configuration.

```sbtshell
language in EtaLib := Haskell98
```

Sets the language to `Haskell98` globally for all the Eta source files in the project using `Eta` configuration.

```sbtshell
language in Eta := Haskell98
```

## extensions

The set of language extensions to enable or disable for the build.

You can obtain the list of supported extensions in this build using the SBT command `eta-extensions`, e.g.:

```shell
$ sbt eta-extensions
```

### Example 1

Enables the `DataKinds` language extension by default for all the Eta source files in the `EtaLib` configuration.

```sbtshell
extensions in EtaLib += "DataKinds"
```

### Example 2

Disables the `DataKinds` language extension by default for all the Eta source files in the `EtaTest` configuration.

```sbtshell
extensions in EtaTest += "NoDataKinds"
```

## ghcOptions

The direct flags to send to the Eta compiler. You would typically use this property to set warning flags.

### Example

Enables all warnings and converts warnings to errors.

```sbtshell
ghcOptions in Eta := Seq("-Wall", "-Werror")
```

## cppOptions

The flags to send to the preprocessor used by the Eta compiler to preprocess files that enable the `CPP` extension.

### Example

Enables the `CPP` language extension for all the Eta source files in the `EtaTest` configuration and sends some flags to the preprocessor that define the `MY_VERSION` constant to be `1200`.

```sbtshell
extensions in EtaTest += "CPP"
cppOptions in EtaTest := Seq("-DMY_VERSION=1200")
```

## includeDirs

Paths to directories which contain include files that can later be referenced with `#include` directives. 

### Example

Locates header files in `/path/to/includes` so that the files in the `EtaLib` configuration can access the include files they need. 

```sbtshell
includeDirs in EtaLib += file("/path/to/includes") 
```

## installIncludes

Names of include files to install along with the package being built.

### Example

Configures the `CTypes.h` header file to be installed along with the package defined by the `EtaLib` configuration. 

```sbtshell
installIncludes in EtaLib += "CTypes.h"
```

### Configuring project globally

Sometimes we want to share configuration across all `Eta[Type]` configurations. For that you must specify settings for `Eta` configuration. Also you can to reassign this values for specific configuration.    

```sbtshell
lazy val root = (project in file(".")).settings(
  ghcOptions in Eta := Seq("-Werror", "-Wall"), // Will be applied for all configurations: `EtaLib`, `EtaExe`, `EtaTest`
  extensions in Eta := Seq("DataKinds"),        // Will be applied for all configurations: `EtaLib`, `EtaExe`, `EtaTest`
  ghcOptions in EtaTest += "-threaded",         // For configuration `EtaTest` compiler flag `-threaded` will be added, i.e. the full list of compiler flags will be: Seq("-Werror", "-Wall", "-threaded")
  extensions in EtaTest += "CPP",               // For configuration `EtaTest` extension `CPP` will be added, i.e. the full list of extensions will be: Seq("DataKinds", "CPP")
  cppOptions in EtaTest := Seq("-DMY_VERSION=1200")
)
```
