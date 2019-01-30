# Global Flags

## Overview

Global flags are flags that can be applied for *any* Eta invocation and must be passed **before** the command.

```
etlas [GLOBAL-FLAGS] [COMMAND] [COMMAND-FLAGS-OR-ARGS]
```

## Flag Types

For reference on the meaning of a value of the **Flag Type** column, please consult the [Field Types](/docs/user-guides/etlas-user-guide/advanced-features/cabal-file#field-types) section. 

A `switch-flag` is a flag that takes no argument and activates the flag upon use. Every other flag type represents the format of a single argument for the flag.

## Basics Flags

| Flags                        | Flag Type            | Description                                             |
| ---------------------------- | :------------------: | ----------------------------------------------------    |
| `-h`, `--help`               | `switch-flag`        | Show the help text                                      |
| `--version`                  | `switch-flag`        | Print version information                               |
| `--numeric-version`          | `switch-flag`        | Print just the version number                           |
| `--select-eta`               | `eta-version`        | Select which version of Eta to use with this invocation |

### Example 1

```sh
etlas --numeric-version
```

### Example 2

```sh
etlas --select-eta=0.8.6b4 update
```

## Periodic Behavior

| Flags                        | Flag Type            | Description                                                                |
| ---------------------------- | :------------------: | ----------------------------------------------------                       |
| `--enable-auto-update`       | `switch-flag`        | Automatically run `etlas update` on a daily basis                          |
| `--disable-auto-update`      | `switch-flag`        | Does not automatically run `etlas update` on a daily basis                 |
| `--enable-send-metrics`      | `switch-flag`        | Automatically sends metrics to facilitate better evolution of Eta          |
| `--disable-send-metrics`     | `switch-flag`        | Does not automatically sends metrics to facilitate better evolution of Eta |


## Configuration

| Flags                        | Flag Type            | Description                                                          |
| ---------------------------- | :------------------: | ----------------------------------------------------                 |
| `--config-file`              | `filepath`           | Set an alternate location for the config file                        |
| `--ignore-expiry`            | `switch-flag`        | Ignore expiry dates on signed Hackage metadata                       |
| `--http-transport`           | `transport-type`     | Specifies the transport to use for http(s) requests                  |
| `--patches-dir`              | `directory`          | Specify directory to store patches.                                  |
| `--binaries-dir`             | `directory`          | Specify location to store downloaded binary executables and packages |

## Next Section

In the next section, we will now move on to the command-specific Etlas flags. 
