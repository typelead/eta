# Compiler Configuration

## Overview

The Eta compiler takes many command line options that you may want to use for your Gradle build. We provide an Options DSL to allow you to configure compiler options easily.

The following table lists the properties that are available for the `options` property of the `compile[SourceSet]Eta` task:


| Property          | Type                                            |
| --------          | ----                                            |
| `language`        | `String`                                        |
| `extensions`      | `NamedDomainObjectContainer<LanguageExtension>` |
| `args`            | `List<String>`                                  |
| `cpp`             | `List<String>`                                  | 
| `installIncludes` | `List<String>`                                  | 
| `includeDirs`     | `List<String>`                                  | 

## language

This specifies the language to use for the build. A language consists of a set of predefined language extensions are are defined by the compiler. Currently, there are only two values you can put for this field:

- `'Haskell98'` 
- `'Haskell2010'`

**Default:** `'Haskell2010'`

### Example

Sets the language to `Haskell98` for all the Eta source files in the `main` sourceSet.

```groovy
compileEta {
  options.language = 'Haskell98'
}
```

## extensions

The set of language extensions to enable or disable for the build.

### Example 1

Enables the `DataKinds` language extension by default for all the Eta source files in the `main` sourceSet.

```groovy
compileEta {
  options {
    extensions {
      DataKinds
    }
  }
}
```

### Example 2

Disables the `DataKinds` language extension by default for all the Eta source files in the `test` sourceSet.

```groovy
compileTestEta {
  options {
    extensions {
      NoDataKinds
    }
  }
}
```

## args

The direct flags to send to the Eta compiler. You would typically use this property to set warning flags.

### Example

Enables all warnings and converts warnings to errors.

```groovy
compileEta {
  options.args = ['-Wall', '-Werror']
}
```

## cpp

The flags to send to the preprocessor used by the Eta compiler to preprocess files that enable the `CPP` extension.

### Example

Enables the `CPP` language extension for all the Eta source files in the `test` sourceSet and sends some flags to the preprocessor that define the `MY_VERSION` constant to be `1200`.

```groovy
compileTestEta {
  
  options {
    extensions {
      CPP
    }
    cpp = ['-DMY_VERSION=1200']
  }
}
```

## includeDirs

Paths to directories which contain include files that can later be referenced with `#include` directives. 

### Example

Locates header files in `/path/to/includes` so that the files in the `main` sourceSet can access the include files they need. 

```groovy
compileEta {
  options {
    includeDirs = ['/path/to/includes']
  }
}
```

## installIncludes

Names of include files to install along with the package being built.

### Example

Configures the `CTypes.h` header file to be installed along with the package defined by the `main` sourceSet. 

```groovy
compileEta {
  options.installIncludes = ['CTypes.h']
}
```

### Configuring Multiple Tasks

Sometimes we want to share configuration across multiple tasks. Here's a quick example of how to use Groovy's language features to configure multiple tasks at a time.

```groovy
[compileEta, compileTestEta].each {
  options {
    args = ['-Werror','-Wall']
    extensions {
      DataKinds
    }
  }
}
```
