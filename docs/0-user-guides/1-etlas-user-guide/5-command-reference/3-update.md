# Update

## Overview

The `update` command takes care of updating the following indices/caches:

- Cached [Hackage](https://hackage.haskell.org/) Index
- Patches from [Eta Hackage](https://github.com/typelead/eta-hackage)
- Binary Index

## Usage

```
etlas update [FLAGS]
```

## Supported Flags

| Flags                        | Flag Type            | Description                                             |
| ---------------------------- | :------------------: | ----------------------------------------------------    |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                      |
| `-v`, `--verbose`            | `verbosity`          | Control verbosity, higher verbosity emits more messages |
| `--index-state`              | `index-timestamp`    | Sets the Hackage index state                            |
