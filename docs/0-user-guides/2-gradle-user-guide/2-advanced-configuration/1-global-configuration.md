# Global Configuration

## Overview

Once you've applied the plugin, you'll want to specify the Eta and Etlas executables you want to apply for your build as well as other configuration options.

The following table lists all the global configuration properties available for the `eta` extension block.

| Property                 | Type      |
| --------                 | ----      |
| `version`                | `String`  |
| `useSystemEta`           | `Boolean` |
| `etlasVersion`           | `String`  |
| `useSystemEtlas`         | `Boolean` |
| `etlasPath`              | `String`  |
| `preInstallDependencies` | `Boolean` |


## Configuring Eta

There are two ways to specify which Eta executable to use for your build:

- Version
- System

### version

This will automatically download the appropriate Eta binary for your platform with the specified version and use that binary to perform your Gradle build.

This is typically used for production builds and ensures build reproducibility.

```groovy
eta {
  version = '0.8.6b4'
}
```

### useSystemEta

This will search for an executable with the name `eta` in the directories specified by the `PATH` environment variable that is available to Gradle and use that executable to perform your build.

This is typically used when you want to test out a source-installed version of Eta.

```groovy
eta {
  useSystemEta = true
}
```

**WARNING:** This method is **not** recommended for production builds since it kills the reproducibility of your build.

## Configuring Etlas

There are three ways in which you can specify which Etlas executable to use for your build:

- Version
- System
- Path

### etlasVersion

This will automatically download the appropriate Etlas binary for your platform with the specified version and use that binary to perform your Gradle build.

This is typically used for production builds and ensures build reproducibility.

```groovy
eta {
  etlasVersion = '1.5.0.0'
}
```

### useSystemEtlas

This will search for an executable with the name `etlas` in the directories specified by the `PATH` environment variable that is available to Gradle and use that executable to perform your build.

This is typically used when you want to test out a source-installed version of Etlas.

```groovy
eta {
  useSystemEtlas = true
}
```

**WARNING:** This method is **not** recommended for production builds since it kills the reproducibility of your build.

### etlasPath

This will use the Etlas executable specified in the path to run the build.

This is typically used when you want to test out a source-installed version of Etlas that isn't on your `PATH`.

```groovy
eta {
  etlasPath = '/path/to/etlas'
}
```

**WARNING:** This method is **not** recommended for production builds since it kills the reproducibility of your build.

## Configuring Dependency Resolution

Some Gradle plugins force dependency resolution of some configurations at task graph build time. This is a problem since the `installDependenciesEta` task dynamically injects dependencies into configurations **after** the task graph has been built. Hence, we provide an option to get around this problem.

### preInstallDependencies

When this option is set to `true`, it will install all the Eta dependencies required for the build before executing any tasks.

```groovy
eta {
  preInstallDependencies = true
}
```

## Examples

### Example 1

```groovy
eta {
  version = '0.8.6b4'
  etlasVersion = '1.5.0.0'
}
```

The plugin will download and cache **Eta v0.8.6b4** and **Etlas v1.5.0.0**.

### Example 2

```groovy
eta {
  version = '0.8.6b4'
  etlasVersion = '1.5.0.0'
  preInstallDependencies = true
}
```

The plugin will download and cache **Eta v0.8.6b4** and **Etlas v1.5.0.0**. Moreover, it will install all the dependencies of your projects before executing any tasks.

### Example 3

```groovy
eta {
  version = '0.8.6b4'
  useSystemEtlas = true
}
```

The plugin will download and cache **Eta v0.8.6b4** and use the Etlas it can find on the `PATH`.

### Example 4

```groovy
eta {
  useSystemEta = true
  etlasVersion = '1.5.0.0'
}
```

The plugin will use the Eta it can find on the `PATH` and download and cache **Etlas v1.5.0.0**.

## Next Section

In the next section, we'll look at how to manage dependencies for our builds.
