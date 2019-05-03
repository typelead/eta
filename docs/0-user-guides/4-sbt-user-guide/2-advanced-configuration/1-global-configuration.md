# Global Configuration

## Overview

Once you've applied the plugin, you'll want to specify the Eta and Etlas executables you want to apply for your build as well as other configuration options.

The following table lists all the global configuration properties available for the `ThisBuild` configuration (use `inThisBuild(...)` function).

| Property                 | Type             |
| --------                 | ----             |
| `etaVersion`             | `String`         |
| `etlasUseSystem`         | `Boolean`        |
| `etlasVersion`           | `String`         |
| `etlasPath`              | `Option[String]` |
| `etaSendMetrics`         | `Boolean`        |

## Configuring Eta

There are two ways to specify which Eta executable to use for your build:

- Version
- System

### etaVersion

This will automatically download the appropriate Eta binary for your platform with the specified version and use that binary to perform your SBT build.

This is typically used for production builds and ensures build reproducibility.

```sbtshell
inThisBuild(Seq(
  etaVersion := "0.8.6b5"
))
```

If you don't specify value for `etaVersion` setting the latest Eta binary will be used.

**WARNING:** It is recommended to specify value for `etaVersion` setting for production builds to support the reproducibility of your build.

## Configuring Etlas

There are three ways in which you can specify which Etlas executable to use for your build:

- System
- Version
- Path

### etlasUseSystem

This will search for an executable with the name `etlas` in the directories specified by the `PATH` environment variable that is available to SBT and use that executable to perform your build.

This is typically used when you want to test out a source-installed version of Etlas.

**Default:** true

```sbtshell
inThisBuild(Seq(
  etlasUseSystem := false
))
```

**WARNING:** This method is **not** recommended for production builds since it kills the reproducibility of your build.

### etlasVersion

This will automatically download the appropriate Etlas binary for your platform with the specified version and use that binary to perform your SBT build.

This is typically used for production builds and ensures build reproducibility.

```sbtshell
inThisBuild(Seq(
  etlasUseSystem := false,
  etlasVersion   := "1.5.0.0"
))
```

**NOTE:** This setting will be applied only if `etlasUseSystem` set to `false`.

### etlasPath

This will use the Etlas executable specified in the path to run the build.

This is typically used when you want to test out a source-installed version of Etlas that isn't on your `PATH`.

**Default:** None (it means to use Etlas in your `PATH`)

```sbtshell
inThisBuild(Seq(
  etlasUseSystem := false,
  etlasPath      := Some(file("/path/to/etlas"))
))
```

**NOTE:** This setting will be applied only if `etlasUseSystem` is set to `false`.

**WARNING:** This method is **not** recommended for production builds since it kills the reproducibility of your build.

### etaSendMetrics

If this is your first time using Eta on your system, you will get prompted about your preference for telemetry.

If you are fine with sending metrics, you can specify the following setting:

```sbtshell
inThisBuild(Seq(
  etaSendMetrics := true
))
```

If you are **not** fine with sending metrics, you can replace **true** with **false** in the commands listed above.

**NOTE:** This setting will be applied only the first time you express your preference.

## Examples

### Example 1

```sbtshell
inThisBuild(Seq(
  etaVersion     := "0.8.6b4",
  etlasUseSystem := false,
  etlasVersion   := "1.5.0.0"
))
```

The plugin will download and cache **Eta v0.8.6b4** and **Etlas v1.5.0.0**.

### Example 2

```sbtshell
inThisBuild(Seq(
  etaVersion := "0.8.6b5"
))
```

The plugin will download and cache **Eta v0.8.6b5** and use the Etlas it can find on the `PATH`.

### Example 3

```sbtshell
inThisBuild(Seq(
  etlasUseSystem := false,
  etlasVersion   := "1.5.0.0"
))
```

The plugin will use the Eta it can find on the `PATH` and download and cache **Etlas v1.5.0.0**.

## Next Section

In the next section, we'll look at how to manage dependencies for our builds.
