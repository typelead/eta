# List

## Overview

The `list` command is used to query both the source package database and the installed package database.

## Usage

### Generic Listing

```sh
etlas list [FLAGS]
```

- `[FLAGS]` can be any of the flags listed below.

This will list all the available packages subject to filtering by the flags.

### Specific Listing

```sh
etlas list [FLAGS] STRINGS
```

- `[FLAGS]` can be any of the flags listed below.
- `STRINGS` can be a list of search terms for the packages

This will list all the available packages that match the search terms and subject to filtering by the flags.

## Flags

| Flags                        | Flag Type            | Description                                                             |
| ---------------------------- | :------------------: | ----------------------------------------------------                    |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                                      |
| `-v`, `--verbose`            | `verbosity`          | Control verbosity, higher verbosity emits more messages                 |
| `--installed`                | `switch-flag`        | Lists just the installed packages                                       |
| `--simple-output`            | `switch-flag`        | Print in a easy-to-parse format                                         |
| `--package-db`               | `package-db`         | Append the given package database to the list of package databases used |
