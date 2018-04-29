# Conditional Configuration

## Overview

As Eta evolves and your packages make use of newer features, you will the need to start worrying about backwards compatibility for your package.

Or maybe you just want to provide different flavors of your package that is built against different dependencies according to certain conditions.

To handle these and other user cases, Etlas provides **conditional configuration** which allows you to configure options when certain conditions hold.

## if/else blocks

In your `.cabal` file, you can use `if` and `else` keywords to specify a condition under which the fields in the block should be included.

### Example 1

```sh
if os(windows)
  cpp-options: -DWINDOWS
```

### Example 2

```sh
if os(windows)
  build-depends: my-windows-library
else
  build-depends: unix-library
```

## Condition Values

After simplification, a condition evaluates to `true` or `false`, where `true` means that condition holds and `false` means the condition doesn't hold.

### Example 1

```sh
if true
  build-depends: base
```

is the same as

```sh
build-depends: base
```

## Predicates

The following built-in predicates, or conditions, are available:

| Predicate | Argument Type   | Description (Returns `true` if...)                                    |
| --------- | :-------------: | ----------------------------------                                    |
| `os`      | `os`            | Current operating system matches the argument                         |
| `arch`    | `arch`            | Current architecture matches the argument                             |
| `impl`    | `prog-constraint` | Program constraint in the argument holds with the build configuration |
| `flag`    | `flag`            | Flag has been assigned via CLI or `cabal.project` configuration       |

Each of the argument types is explained in detail below.

### os

The operating system can be specified by the following values:

- `linux`
- `osx`
- `windows`
- `solaris`
- `freebsd`
- `openbsd`
- `netbsd`

**Example**

```sh
if os(freebsd)
  cpp-options: -DPLATFORM_FREEBSD
```

### arch

The architecture can be specified by the following values:

- `i386`
- `x86_64`
- `ppc`
- `ppc64`
- `sparc`
- `alpha`

**Example**

```sh
if arch(x86_64)
  cpp-options: -DENABLE_64BIT_OPTIMIZATIONS
```

### prog-constraint

A program constraint takes the form `[compiler-name] [version-range]` where `[compiler-name]` can be the name of the compiler you want to check against and `[version-range]` has been specified in the [previous section](./cabal-file#field-types). Note that `[version-range]` is optional and if not present, just checks whether `[compiler-name]` is configured for the build.

**Example 1**

```sh
if impl(eta > 0.7.2.1)
  java-sources: java/eta-0.7.3/Utils.java
else
  java-sources: java/eta-0.7.2/Utils.java
```

**Example 2**

```sh
if impl(eta)
  cpp-options: -DCOMPILING_ETA
```

### flag

This predicate is used to check whether a [package flag](./cabal-file#package-flags) has been evaluated to `true` or not.

**Example**

```sh
if flag(developer)
  cpp-options: -DDEVELOPMENT
```

## Operators

The logical operators supported (in precedence order) is as follows:

- `!`: Logical NOT
- `&&`: Logical AND
- `||`: Logical OR

### Example

```sh
if impl(eta) && !os(windows)
  eta-options: -funix-optimizations
```

## Next Section

In the next section, we will cover all ways you can configure a `cabal.project` file.
