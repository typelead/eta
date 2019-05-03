# Plugin Basics

## Overview

The Eta SBT Plugin provides configuration options to customize your build and overrides some of SBT's default tasks and settings to make the integration as smooth as possible.

At a high-level, the plugin translates the configuration information you provide via the DSL into a form that [Etlas](/docs/user-guides/etlas-user-guide/introduction/what-is-etlas) can understand and calls out to Etlas to take care of the rest of the build.

This architecture allows us to add most of the core build features to Etlas and merely update the user-facing configuration DSL for each of the build tools we support. Moreover, it allows us to provide a *native* experience for each build tool without having to reimplement the same logic.

## Applying the Plugin

You can add the plugin to the build defining it in [plugins.sbt](https://www.scala-sbt.org/1.0/docs/Using-Plugins.html#Declaring+a+plugin).

```sbtshell
addSbtPlugin("com.typelead" % "sbt-eta" % "0.3.0")
```

This will load the plugin from [Maven Central](https://mvnrepository.com/repos/central). To apply the plugin to the project, you must enable it for those subprojects where you want to use Eta, e.g.:

```sbtshell
lazy val root = (project in file(".")).enablePlugins(SbtEta)
```

## Global Configuration

By default plugin will use Etlas and Eta installed into your system. But you can configure the versions of the Eta compiler and Etlas build tool that you would like to use for the build. The recommend way to do this is by specifying the versions so that your build becomes reproducible. For advanced usage, you can consult [this](/docs/user-guides/sbt-user-guide/advanced-configuration/global-configuration) section.

Note that this configuration will apply to **all** subprojects in your SBT build.

### Example

```sbtshell
lazy val root = (project in file("."))
  .settings(
    inThisBuild(Seq(
      etlasUseSystem := false,
      etaVersion     := "0.8.6b5",
      etlasVersion   := "1.5.0.0"
    ))
  )
```

This will tell SBT not to use system installed versions, install **Etlas v1.5.0.0** and **Eta v0.8.6b5** and build the project using those executables.

## Configuring Sources

By default, the Eta modules to compile for your default configuration are expected to be in `src/main/eta`. You can configure this by modifying the `sourceDirectory` and `sourceDirectories` settings.

### Example 1

```sbtshell
sourceDirectories in EtaLib += baseDirectory.value / "eta"
```

This will look for Eta sources inside of the `eta` directory as well as `src/main/eta`.

### Example 2

```sbtshell
sourceDirectory in EtaLib := baseDirectory.value / "eta"
```

This will look for Eta sources inside of the `eta` directory only.

## Configuring Dependencies

You can add dependencies to your project by using the `eta()` method with standard `libraryDependencies` setting. You can add the latest version of a package using `eta("[package-name]")` or specify the concrete version for it using `eta("[package-name]", "[version-range]")`. The `[version-range]` should be in [Ivy version range notation](/docs/user-guides/sbt-user-guide/advanced-configuration/dependency-management#eta-packages).

### Example 1

```sbtshell
libraryDependencies in EtaLib += eta("base", "4.8.2.0")
```

This will add a dependency on `base-4.8.2.0` to the `compile` configuration.

## Configuring the Compiler

You can specify additional options which will be sent to the Eta compiler.

### Example 1

```sbtshell
ghcOptions in Eta += "-Wall"
```

The `ghcOptions` setting takes a sequence of `String`'s and sends those as direct arguments to the compiler. In this case, we are turning on all the warnings.

### Example 2

```sbtshell
extensions in Eta += "DataKinds"
```

The `extensions` setting is a list of extensions to enable or disable for Eta project.

For advanced usage, consult [this](/docs/user-guides/sbt-user-guide/advanced-configuration/compiler-configuration) page.

## More Examples

You can find more examples in the [sbt-eta](https://github.com/typelead/sbt-eta/tree/master) repository.

## Next Section

In the next section, we will cover advanced usages of the plugin.
