# Plugin Management

## Objectives

- Differentiate between the plugin variations
- Demonstrate how and when to use each plugin variation

## Overview

There are many ways to include the Eta plugin and variations of it to your project. This section will help you decide which plugin(s) to use for your build.

## Plugin Variants

There are three plugins which the Eta Gradle plugin provides:

- Eta Base Plugin
- Eta Plugin
- Eta Android Plugin

### Eta Base Plugin

This plugin is used for configuring your Eta and Etlas versions and other global parametrs for all the projects in your build. It will activate the `eta` extension block that will allow you to do so.

**NOTE:** Both the Eta Plugin and the Eta Android Plugin apply the Eta Base Plugin by default, so if you apply either one, it is not required to apply this one!

### Eta Plugin

This plugin is used for JVM projects and depends on the Java Plugin. It overrides the Java Plugin's lifecycle so that it can provide a smooth integration.

### Eta Android Plugin

This plugin is used for Android projects. It overrides the Android lifecycle to provide a smooth integration into the build.

## Applying with Plugins DSL

The recommended way to apply the plugins mentioned above is the [Plugins DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block). Note that using this DSL will automatically download the plugin **and** apply it to the current project, so you don't need to add an `apply plugin:` statement afterward.

### Eta Base Plugin

```groovy
plugins {
    id 'com.typelead.eta.base' version '0.7.2'
}
```

### Eta Plugin

```groovy
plugins {
    id 'com.typelead.eta' version '0.7.2'
}
```

### Eta Android Plugin

```groovy
plugins {
    id 'com.typelead.eta.android' version '0.7.2'
}
```

## Applying with Legacy Method

The old method of applying plugins can be done by specifying a `buildscript` block which points to the [Gradle Plugins Portal](https://plugins.gradle.org/).

```groovy
buildscript {
  repositories {
    maven {
      url 'https://plugins.gradle.org/m2/'
    }
  }
  dependencies {
    classpath 'gradle.plugin.com.typelead:gradle-eta:0.7.2'
  }
}
```

Now that you have told Gradle *where* to find the plugin binary, the next step is to specify *which* plugin you want to apply to your build.

### Eta Base Plugin

```groovy
apply plugin: 'eta-base'
```

### Eta Plugin

```groovy
apply plugin: 'eta'
```

### Eta Android Plugin

```groovy
apply plugin: 'eta-android'
```

## Applying with Locally Built Plugin

If you're hacking on the plugin or want to try the unreleased version due to critical bug fixes or cool features, you have to build and install the plugin from source.

```sh
$ git clone https://github.com/typelead/gradle-eta
$ cd gradle-eta
```

#### Linux/OS X

```sh
$ ./gradlew pTML
```

#### Windows

```sh
$ gradle.bat pTML
```

Then, add a `buildscript` block in the `build.gradle` for the project in which you want to use the plugin you just built.

```groovy
buildscript {
  repositories {
    mavenLocal()

    dependencies {
      classpath 'com.typelead:gradle-eta:latest.release'
    }
  }
}
```

Finally, applying the plugins follows the same process as [Applying with Legacy Method](#applying-with-legacy-method), excluding the `buildscript` block.

## Next Section

In the next section, we will cover all the options available when configuring globally.
