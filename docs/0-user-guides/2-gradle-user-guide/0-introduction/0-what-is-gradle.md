# What is Gradle?

## Overview

Gradle is an advanced build automation system that uses a [Groovy DSL](https://docs.gradle.org/current/dsl/) for build configuration. It is very popular in the JVM world for providing an incredibly concise way of specifying builds and allowing build engineers to extend the DSL for their particular needs.

## Motivation

We chose to add full support for Eta in Gradle through the [Eta Gradle Plugin](https://github.com/typelead/gradle-eta) for a few reasons we outline below:

- **Extensiblility**: Gradle is very extensible and allows you to specify concise configuration for highly complex builds.
- **Reproducibility**: Gradle allows you to generate a [wrapper script](https://docs.gradle.org/current/userguide/gradle_wrapper.html) that will let anyone build your project without having Gradle installed before hand, ensuring that your project can build on every configuration reliably.
- **JVM Support**: Gradle makes it very easy to setup polyglot JVM language projects allowing Eta to be easily plugged in to any JVM setup.
- **Caching**: Gradle offers [build caching](https://docs.gradle.org/current/userguide/build_cache.html) which will cache a build based on the inputs and outputs. This works great for organizations which can setup a remote build cache to save time in building large internal projects.

## Next Section

In the next section, we'll discuss the philosophy of the Eta Gradle plugin.
