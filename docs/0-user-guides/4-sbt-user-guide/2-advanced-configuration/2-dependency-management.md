# Dependency Management

## JVM Dependencies

A JVM dependency is simply a JAR file which can be added as a dependency and imported into your Eta modules through the Foreign Function Interface.

### Maven Dependencies

To specify Maven dependencies for your Eta project you can use standard `libraryDependencies` setting for specific `Eta[Type]` configuration.

**Example**

```sbtshell
libraryDependencies in EtaLib += "com.google.guava" % "guava" % "25.0-jre"
```

Using standard `resolvers` setting for any `Eta[Type]` configuration you can specify Maven repositories from which the dependencies are downloaded from. By default, the dependencies are downloaded from [Maven Central](https://mvnrepository.com/repos/central).

**Example**

```sbtshell
resolvers in EtaLib ++= Seq(Resolver.jcenterRepo, Resolver.sonatypeRepo("public"))
```

## Eta Packages

Unlike standard SBT dependency management, Eta downloads and builds all of its dependencies from source. To integrate into SBT's infrastructure, we provide a method `eta()` that can be used in standard `libraryDependencies` setting for any `Eta[Type]` configuration.

The `eta()` method takes:

- One argument: `eta("package-name")` - use the latest version of package
- Two arguments: `eta("package-name", "version-or-version-range")` - use the specific version or version range of package

### Latest Version

**Example**

```sbtshell
libraryDependencies in EtaLib += eta("array")
```

### Exact Version

You can fix an exact version for each dependency.

**Example**

```sbtshell
libraryDependencies in EtaLib += eta("array", "1.2.3.0")
```

### Version Range

| Notation | Description                                                                     |
| -------- | --------------------------------------                                          |
| `[a,b]`  | Matches all versions greater than or equal to `a` and less than or equal to `b` |
| `[a,b[`  | Matches all versions greater than or equal to `a` and less than than `b`        |
| `]a,b]`  | Matches all versions greater than `a` and less than or equal to `b`             |
| `]a,b[`  | Matches all versions greater than `a` and less than than `b`                    |
| `[a,)`   | Matches all versions greater than or equal to `a`                               |
| `]a,)`   | Matches all versions greater than `a`                                           |
| `(,b]`   | Matches all versions less than or equal to `b`                                  |
| `(,b[`   | Matches all versions less than than `b`                                         |

**Example**

```sbtshell
libraryDependencies in EtaLib += eta("array", "[1.2.3.0,1.2.4.0[")
```

### Version Prefix

A version prefix `a.b.c.d.+` corresponds to `[a.b.c.d.0,a.b.c.(d + 1).0[`.

**Example**

```sbtshell
libraryDependencies in EtaLib += eta("array", "1.2.3.+")
```

## Git Dependency

You can specify git dependencies using `gitDependencies` setting. 

### Commit

```sbtshell
gitDependencies in EtaLib += git(
  "eta-spark-core", 
  "https://github.com/Jyothsnasrinivas/eta-spark-core", 
  commit("acbbe10b68f22f8f3f8ac21c82f12bb811a2fa7e")
)
```

### Tag

```sbtshell
gitDependencies in EtaLib += git(
  "eta-spark-core", 
  "https://github.com/Jyothsnasrinivas/eta-spark-core", 
  tag("0.1.1.0")
)
```

### Branch

```sbtshell
gitDependencies in EtaLib += git(
  "eta-spark-core", 
  "https://github.com/Jyothsnasrinivas/eta-spark-core", 
  branch("master")
)
```

### Subdirectory

```sbtshell
gitDependencies in EtaLib += git(
  "cborg", 
  "https://github.com/well-typed/cborg", 
  tag("3d274c14ca3077c3a081ba7ad57c5182da65c8c1"),
  "cborg"
)
```

## Next Section

We'll cover how to configure the compiler in the next section.
