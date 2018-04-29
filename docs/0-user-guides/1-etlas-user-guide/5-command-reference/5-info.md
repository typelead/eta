# Info

## Overview 

The `info` command is used to obtain information about a particular package.

## Usage

```sh
etlas info [FLAGS] PACKAGES`
```

- `[FLAGS]` can be any of the flags listed below.
- `PACKAGES` can be a list of search terms for the packages

## Etlas Info Flags

| Flags                        | Flag Type            | Description                                                             |
| ---------------------------- | :------------------: | ----------------------------------------------------                    |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                                      |
| `-v`, `--verbose`            | `verbosity`          | Control verbosity, higher verbosity emits more messages                 |
| `--package-db`               | `package-db`         | Append the given package database to the list of package databases used |
