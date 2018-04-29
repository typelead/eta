# Get

## Overview

The `get` command is used to download the source to the specified destination directory (the current directory by default).

## Usage

```sh
etlas get [FLAGS] PACKAGES`
```

- `PACKAGES` should be a list of specified as either `[package-name]` or `[package-name]-[version]`

## Flag Types

### source-repository-spec

A source repository specification can be either `this` or `head`.

## Etlas Get Flags

| Flags                        | Flag Type                      | Description                                             |
| ---------------------------- | :----------------------------: | ----------------------------------------------------    |
| `-h`, `--help`               | `switch-flag`                  | Show the help text                                      |
| `-v`, `--verbose`            | `verbosity`                    | Control verbosity, higher verbosity emits more messages |
| `-d`, `--destdir`            | `directory`                    | Location where to unpack the source code                |
| `-s`, `--source-repository`  | `source-repository-spec`       | Location where to unpack the source code                |
| `--index-state`              | `index-timestamp`              | Sets the Hackage index state                            |
