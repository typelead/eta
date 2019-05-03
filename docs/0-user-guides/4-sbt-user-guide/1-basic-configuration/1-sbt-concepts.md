# Gradle Concepts

## Overview

Gradle consists of fundamental concepts in its build system for which you should have a basic understanding to configure and debug your builds.

## SourceSet

A **SourceSet** is a group of source files that are to be compiled together. You can find more information about sourceSets [here](https://docs.gradle.org/current/dsl/org.gradle.api.tasks.SourceSet.html).

## Configuration

A **Configuration** is collection of dependencies in the form of files (JAR files) that your project depends on. It can also consist of **artifacts** which are the outputs when you build the configuration. The dependencies can be thought of as the *inputs* and the artifacts can be thought of as the *outputs*.


Configurations can extend other configurations which mean that they inherit the collection of dependencies (and artifacts) of their parents.

The most common configuration is `compile` which you be using most of the time when specifying dependencies.

You can find more information about configurations [here](https://docs.gradle.org/current/userguide/managing_dependency_configurations.html) and [here](https://docs.gradle.org/current/dsl/org.gradle.api.artifacts.Configuration.html).

## Task

A **Task** does the actual work in your build. Tasks can depend on other tasks. For example, a task that generates source code will be depended on by a task that compiles the source code. Your build execution works by running tasks in dependency order.

You can demand that a task be executed by supplying it as an argument to the gradle command, like so

```sh
$ ./gradlew compileEta
```

and Gradle will run all the tasks that `compileEta` depends on in dependency order followed by the task itself.

You can learn more about tasks [here](https://docs.gradle.org/current/userguide/more_about_tasks.html) and [here](https://docs.gradle.org/current/dsl/org.gradle.api.Task.html).

## Evaluation

All the `build.gradle` files in your project are **evaluated** (since they are just Groovy source code) into a `Project` object which stores all the information about the build. The full lifecycle of evaluation is documented [here](https://docs.gradle.org/current/javadoc/org/gradle/api/Project.html).

## Plugins

Plugins are code that are run against a `Project` object to customize the evaluation of the `Project`. You can find more information about plugins [here](https://docs.gradle.org/current/userguide/plugins.html).

## Execution

Once all the projects in your build have been fully evaluated, Gradle will construct a **task graph** and will execute the tasks in dependency order. You can read more about the build cycle [here](https://docs.gradle.org/current/userguide/build_lifecycle.html).

## Next Section

We will proceed to cover the basics of the Eta plugin.
