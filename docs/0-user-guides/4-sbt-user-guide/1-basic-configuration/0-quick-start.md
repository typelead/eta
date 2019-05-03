# Quick Start

## Overview

You'll create a simple Eta application using [SbtEta plugin](https://github.com/typelead/sbt-eta). 

## Requirements

You must have the following installed on your system:

- [JDK 8 or above](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

## Create a project

1. Create a project folder with following contents:

```       
.
├── build.sbt
├── project
│   └── plugins.sbt
└── src
    ├── main
    │   └── eta
    │       └── Main.hs
    └── test
        └── eta
            └── Spec.hs
```

- `build.sbt`:

```sbtshell
lazy val root = (project in file("."))
  .settings(
    inThisBuild(Seq(
      etlasUseSystem := false,
      etaVersion     := "0.8.6b5",
      etlasVersion   := "1.5.0.0",
      scalaVersion   := "2.12.8",
      version        := "0.1.0-SNAPSHOT"
    )),
    name := "example",
    hsMain in EtaExe := Some("Main.hs"),
    hsMain in EtaTest := Some("Spec.hs")
  )
  .enablePlugins(SbtEta)
```

- `project/plugins.sbt`:

```sbtshell
addSbtPlugin("com.typelead" % "sbt-eta" % "0.3.0")
```

- `src/main/eta/Main.hs`

```haskell
main = putStrLn "Hello, Eta!"
```

- `src/test/eta/Spec.hs`

```haskell
main = putStrLn "Tests will be here."
```

2. Run project using command:

```sh
$ sbt test run
```

## Next Section

In the next section, we will cover the basic concepts of SBT.
