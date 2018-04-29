# Select

## Overview

The `select` command is used to control the Eta version to be used globally by Etlas.

## Usage

### Set Eta Version

```sh
etlas select [VERSION]
```

`[VERSION]` must be in `eta-version` format OR the value `local`. If it is `local`, it will attempt to find the Eta executable on the `PATH`.

### List Versions

```sh
etlas select [FLAGS]
```

This form will query the list of available binary Eta versions.

## Flags

| Flags                        | Flag Type            | Description                                                                         |
| ---------------------------- | :------------------: | ----------------------------------------------------                                |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                                                  |
| `-v`, `--verbose`            | `verbosity`          | Control verbosity, higher verbosity emits more messages                             |
| `--list`                     | `switch-flag`        | Lists the available Eta versions.                                                   |
| `--installed`                | `switch-flag`        | To be used in conjunction with `--list`. Will only list the installed Eta versions. |
