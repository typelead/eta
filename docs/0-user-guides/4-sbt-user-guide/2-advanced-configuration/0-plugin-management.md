# Plugin Management

## Overview

This section will help you to use SBT Eta plugin for your build. 

## Configurations

Plugin introduced several SBT configuration:

- `Eta`
- `EtaLib`
- `EtaExe`
- `EtaTest`

### Eta Configuration

This configuration is used for specify default settings for all other `Eta[Type]` configurations.

**NOTE:** Also you can to reassign or add settings values for each specific configuration.

### EtaLib Configuration

This configuration provides setting for `library` artifact of your Eta project. By default it is included in any project. 

### EtaExe Configuration

This configuration provides setting for `executable` artifact of your Eta project. It will be included in your build only if you specify a value for `hsMain` setting, e.g.:

```sbtshell
hsMain in EtaExe := Some("Main.hs")
```

### EtaTest Configuration

This configuration provides setting for `test-suite` artifact of your Eta project. It will be included in your build only if you specify a value for `hsMain` setting, e.g.:

```sbtshell
hsMain in EtaTest := Some("Spec.hs")
```

## Modules

Each Eta project contains a list of modules, which are included in package. You can specify them for each `Eta[Type]` configuration using `modules` setting.

**Example**

```sbtshell
modules in EtaLib ++= Seq(
  exposed("Hello.Mod"),
  module("Hello.Internal")
)
```

- `exposed()` - this module will be included in package as a part of it's public interface, i.e. will be available for other projects used your package. 
- `module()` - this module used by the package but not exposed to users, for example it can be a part of internal implementation or can be used in executable or test.

## Main Module

Executables and tests must have the Main module which is an entry point when they are started. You can specify it using `hsMain` setting for specific configuration.

**Example**

```sbtshell
hsMain in EtaExe := Some("Main.hs")
hsMain in EtaTest := Some("Spec.hs")
```

**NOTE:** If you don't assign a value to `hsMain` setting for `EtaExe` or `EtaTest` configuration then executable or tests will not be included into your project.

## Test Suite Interface

Etlas supports two test suite interfaces, called `exitcode-stdio-1.0` and `detailed-1.0`. 

Test suites using the `exitcode-stdio-1.0` interface are executables that indicate test failure with a non-zero exit code when run. They may provide human-readable log information through the standard output and error channels. This interface is provided primarily for compatibility with existing test suites, it is preferred that new test suites be written for the `detailed-1.0` interface.

Test suites using the `detailed-1.0` interface are modules exporting the symbol `tests :: [Test]`. The Test type is exported by the module `Distribution.TestSuite` provided by Etlas. The `detailed-1.0` interface allows Etlas and other test agents to inspect a test suite’s results case by case, producing detailed human- and machine-readable log files.

You can specify test suite interface using `testSuiteType` setting for `Eta` or `EtaTest` configuration.

There are corresponding constants:

- `exitcodeTestSuite` - means `exitcode-stdio-1.0`
- `detailedTestSuite` - means `detailed-1.0`

**Default:** `exitcodeTestSuite`

**Example**

```sbtshell
testSuiteType in EtaTest := detailedTestSuite
``` 

## Applying with Locally Built Plugin

If you're hacking on the plugin or want to try the unreleased version due to critical bug fixes or cool features, you have to build and install the plugin from source.

```sh
$ git clone https://github.com/typelead/sbt-eta.git
$ cd sbt-eta
$ sbt "^ publishLocal"
```

## Next Section

In the next section, we will cover all the options available when configuring globally.
