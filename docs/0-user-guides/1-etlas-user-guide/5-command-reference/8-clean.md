# Clean

## Overview

The `clean` command cleans all the build artifacts and the incremental build cache.

## Usage

```sh
etlas clean [FLAGS]
```

## Usage

| Flags                        | Flag Type            | Description                                             |
| ---------------------------- | :------------------: | ----------------------------------------------------    |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                      |
| `-v`, `--verbose`            | `verbosity`          | Control verbosity, higher verbosity emits more messages |
| `--builddir`                 | `directory`          | Location of build artifacts. <br/> **Default:** `dist`  |
