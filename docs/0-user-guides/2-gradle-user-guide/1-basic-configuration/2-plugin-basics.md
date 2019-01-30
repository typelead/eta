# Plugin Basics

## Overview

The Eta Gradle Plugin provides configuration options to customize your build and overrides some of Gradle's default mechanisms and lifecycle to make the integration as smooth as possible.

At a high-level, the plugin translates the configuration information you provide via the DSL into a form that [Etlas](/docs/user-guides/etlas-user-guide/introduction/what-is-etlas) can understand and calls out to Etlas to take care of the rest of the build.

This architecture allows us to add most of the core build features to Etlas and merely update the user-facing configuration DSL for each of the build tools we support. Moreover, it allows us to provide a *native* experience for each build tool without having to reimplement the same logic.

## Applying the Plugin

You can apply the plugin using the [Gradle Plugins DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block).

```groovy
plugins {
    id 'com.typelead.eta' version '0.7.2'
}
```

This will load the plugin from the [Gradle Plugins Portal](https://plugins.gradle.org/) and immediately apply the plugin to the project.

## Global Configuration

The plugin requires you to configure the versions of the Eta compiler and Etlas build tool that you would like to use for the build. The recommend way to do this is by specifying the versions so that your build becomes reproducible. For advanced usage, you can consult [this](/docs/user-guides/gradle-user-guide/advanced-configuration/global-configuration) section.

Note that this configuration will apply to **all** projects in your Gradle build.

### Example

```groovy
eta {
  version = '0.8.6b4'
  etlasVersion = '1.5.0.0'
}
```

This will tell Gradle to install **Etlas v1.5.0.0** and **Eta v0.8.6b4** and build the project using those executables.

**NOTE**: You can only use the `eta` extension block in the root project. If you have a single `build.gradle` file in your project with no subfolders containing `build.gradle` files, you are building the root project.

## Configuring Sources

By default, the Eta modules to compile for your default configuration are expected to be in `src/main/eta`. You can configure this by modifying the `sourceSets` extension.

### Example 1

```groovy
sourceSets {
  main {
    eta {
      srcDir 'eta'
    }
  }
}
```

This will look for Eta sources inside of the `eta` directory as well as `src/main/eta`.

### Example 2

```groovy
sourceSets {
  main {
    eta {
      srcDirs = ['eta']
    }
  }
}
```

This will look for Eta sources inside of the `eta` directory only.

## Configuring Dependencies

You can add dependencies to your project by using the `eta()` method. The `eta()` method can take a variable number of arguments that should all be of the form `"[package-name]:[version-range]"`. The `[version-range]` should be in [Ivy version range notation](/docs/user-guides/gradle-user-guide/advanced-configuration/dependency-management#version-dependency).

### Example 1

```groovy
dependencies {
  compile eta('base:4.8.2.0')
}
```

This will add a dependency on `base-4.8.2.0` to the `compile` configuration.

### Example 2

```groovy
dependencies {
  compile eta('base:4.8.2.0')
  compile eta('array:0.5.2.0')
}
```

and

```groovy
dependencies {
  compile eta('base:4.8.2.0', 'array:0.5.2.0')
}
```

are equivalent because the `eta()` method can take a variable number of arguments.

## Task Graph

The plugin creates tasks to aid in Eta compilation and injects them into the task graph. The following diagram shows the task dependencies.

<img src="/images/gradle-overview.svg" style="padding-bottom: 50px;" />
<br/>

### setupEnvironmentEta

This task is attached to the root project. It is responsible for installing and the configuring the appropriate versions of Eta and Etlas.

### resolveDependenciesEta

This task is attached to the root project. This task collects Eta dependencies from all configurations across all projects in the build and finds a consistent set of dependencies to use for the build.

### installDependencies[SourceSet]Eta

This task builds and installs the Eta dependencies for a given sourceSet's `compileClasspath` configuration if they haven't been installed already. Moreover, it injects the resolved dependencies into the corresponding configuration in which it was declared.

### compile[SourceSet]Eta

This task compiles the source files configured for the sourceSet with the dependencies specified in the `compileClasspath` configuration. Note that the `compile[SourceSet]Java` task is run before this and the output is added as a dependency when compiling.

### repl[SourceSet]Eta

This task loads the source files configured for the sourceSet with the dependencies specified in the `compileClasspath` configuration into the Eta REPL and allows you to interact with your code.

## Configuring the Compiler

The `compile[SourceSet]Eta` task contains an `options` field which can be used to send additional options to the compiler.

### Example 1

```groovy
compileEta {
  options.args = ['Wall']
}
```

The `args` property takes a list of `String`'s and sends those as direct arguments to the compiler. In this case, we are turning on all the warnings.

### Example 2

```groovy
compileEta {
  options {
    extensions {
      DataKinds
    }
  }
}
```

The `extensions` property is a container that contains all the extensions to enable or disable for the corresponding sourceSet.

For advanced usage, consult [this](/docs/user-guides/gradle-user-guide/advanced-configuration/compiler-configuration) page.

## More Examples

You can find more examples in the [gradle-eta](https://github.com/typelead/gradle-eta/tree/master/examples) repository.

## Next Section

In the next section, we will cover advanced usages of the plugin.
