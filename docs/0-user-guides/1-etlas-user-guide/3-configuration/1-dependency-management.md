# Dependency Management

## Overview

Etlas supports a few types of dependencies and sources to retrieve those dependencies.

- **Eta Dependency**
- **JVM Dependency**

## Eta Dependency

An Eta dependency is a dependency that packages up compiled Eta code and Eta interface files which store type information. You can import Eta modules from these dependencies.

When you add a dependency constraint in the `build-depends` field, Etlas needs to download and install that package from a particular source. We'll discuss the different sources from which Eta searches for dependencies below.

## From Hackage

When it encounters a dependency that it needs to resolve, Etlas will query [Hackage](https://hackage.haskell.org/) and see if it exists there. Hackage is a repository that stores packages for the [Haskell](https://www.haskell.org/) programming language. 

Because of the similarities between Eta and Haskell, a large number of packages can be compiled with minor changes (if any). For packages which require minor changes, Etlas queries [Eta Hackage](https://github.com/typelead/eta-hackage) and checks if a patch is available and if so, applies that patch before building the package.

Etlas maintains a cached index of Hackage to make this lookup fast. If you see a package listed on the Hackage website and Etlas fails to find it on your system, then try running `etlas update` to update your cached index.

## From Etlas Index

Etlas also queries [Etlas Index](https://github.com/typelead/etlas-index) when searching for Eta dependencies. 

Etlas Index is where all the packages that rely on JVM- or Eta-specific features for the core functionality should be placed. For example, a purely-functional API for JDBC would belong in Etlas Index because it cannot function without the JDBC API which is specific to the JVM world.

## From Local Packages

Another source for Eta dependencies is local packages - packages that are stored on your filesystem. By default, all subfolders in your project that contain `.cabal` files will automatically be treated as local packages. If you want more fine-grained control, you can create a `cabal.project` file that specifies exactly which packages should be included in your project.

### Example

**cabal.project**
```
packages:
  . 
  package1/
  package2/
  package-root/package3/
```

For each of the folders listed in the `packages` field, a `*.cabal` file will be detected and the folder will be treated as a local package.

## From Git

You can add Git dependencies to your project by specifying them in your `cabal.project` file.

### Example

**cabal.project**
```
packages: .

source-repository-package
  type: git
  location: https://github.com/Jyothsnasrinivas/eta-spark-core
  commit: acbbe10b68f22f8f3f8ac21c82f12bb811a2fa7e
```

If the package `eta-spark-core` with the exact same version as in the commit specified can be found in Hacakge or Etlas Index, the GitHub location will override the other search locations. This makes it easy to depend on forked versions of existing libraries when the released versions of those libraries don't contain the functionality required for your project.


## JVM Dependency

A JVM dependency is simply a JAR file which can be added as a dependency and imported into your Eta modules through the Foreign Function Interface. Etlas allows you to specify JVM dependencies using various fields that are available for all components.

## From Local Files

You can include local `.java` files, `.class` files, and `.jar` files into your build by specifying their paths in the `java-sources` field.

### Example

**example.cabal**
```
name: example
version: 0.1.0.0
build-type: Simple

library
  hs-source-dirs:   src
  exposed-modules:  Example
  build-depends:    base >=4.8 && <4.9
  default-language: Haskell2010
  java-sources: java/Utils.java
                classes/Utils2.class
                jars/lib.jar
```

## From Maven Repositories

You can include JVM dependencies from Maven repositories, which are a popular way to distribute JVM libraries. 

### Specifying Maven Dependencies

These dependencies are specified using the `maven-depends` field, and should be of the form shown below:

- `[group-id]:[artifact-id]:[version]`

### Specifying Maven Repositories

The Maven repositories from which the dependencies are downloaded from are specified in the `maven-repos` field. By default, the dependencies are downloaded from [Maven Central](https://mvnrepository.com/repos/central).

The `maven-repos` field can take a URL to a Maven repository OR a keyword from the table below.

| Keyword                    | Maven Repository URL                                     |
| -------------------------- | -------------------------------------------------------- |
| `central`                  | `https://repo1.maven.org/maven2/`                        |
| `javaNet1`                 | `http://download.java.net/maven/1/`                      |
| `sonatype:[type]`          | `https://oss.sonatype.org/content/repositories/[type]`   |
| `jcenter`                  | `https://jcenter.bintray.com/`                           |
| `bintray:[owner]:[repo]`   | `https://dl.bintray.com/[owner]/[repo]/`                 |

- `[type]` should be one of `public`, `snapshots`, or `releases`.
- `[owner]` should be the owner of the repository and `[repo]` should be the name of the Bintray repository.

**NOTE:** If you need to access a Maven repository that requires credentials, you can specify the URL in the form `http://[user]:[pass]@[repo-url]`.

### Example 1

```
name: example
version: 0.1.0.0
build-type: Simple

library
  hs-source-dirs:   src
  exposed-modules:  Example
  build-depends:    base >=4.8 && <4.9
  default-language: Haskell2010
  maven-depends: com.google.guava:guava:25.0-jre
                 org.apache.commons:commons-lang3:3.7
```

### Example 2

```
name: example
version: 0.1.0.0
build-type: Simple

library
  hs-source-dirs:   src
  exposed-modules:  Example
  build-depends:    base >=4.8 && <4.9
  default-language: Haskell2010
  maven-depends: com.custom:mypackage:1.0
  maven-repos: jcenter
```

## Next Section

We will proceed to learn about project configuration.
