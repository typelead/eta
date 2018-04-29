# Dependency Management

## Overview

Unlike standard Gradle dependency management, Eta downloads and builds all of its dependencies from source. To integrate into Gradle's infrastructure, we provide a method `eta()` that can be used in the `dependencies` block.

## Eta Method

The `eta()` method takes one of the following:

- A variable number of arguments of the form `[package-name]:[version-or-version-range]`
- It can take a map of keys for specifying Git dependencies

## Version Dependency

### Exact Version

You can fix an exact version for each dependency.

**Example**

```groovy
dependencies {
  compile eta('array:1.2.3.0')
}
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

```groovy
dependencies {
  compile eta('array:[1.2.3.0,1.2.4.0[')
}
```

### Version Prefix

A version prefix `a.b.c.d.+` corresponds to `[a.b.c.d.0,a.b.c.(d + 1).0[`.

**Example**

```groovy
dependencies {
  compile eta('array:1.2.3.+')
}
```

## Git Dependency

### Commit

```groovy
dependencies {
  compile eta(package:  'eta-spark-core',
              location: 'https://github.com/Jyothsnasrinivas/eta-spark-core',
              commit:   'acbbe10b68f22f8f3f8ac21c82f12bb811a2fa7e')
}
```

### Tag

```groovy
dependencies {
  compile eta(package:  'eta-spark-core',
              location: 'https://github.com/Jyothsnasrinivas/eta-spark-core',
              tag:      '0.1.1.0')
}
```

### Branch

```groovy
dependencies {
  compile eta(package:  'eta-spark-core',
              location: 'https://github.com/Jyothsnasrinivas/eta-spark-core',
              branch:   'master')
}
```

## Next Section

We'll cover how to configure the compiler in the next section.
