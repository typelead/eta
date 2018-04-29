# Nix

## Prerequisites

Make sure you have the following tools installed on your system:

- [Nix](https://nixos.org/nix/)

## Installation

To obtain an environment with `eta` and `etlas`, run the following commands:

```sh
$ git clone --branch stable --recursive --depth 1 https://github.com/typelead/eta
$ cd eta
$ nix-shell -A eta-build-shell
```

Once in the shell, run the following commands:

```sh
$ eta-build uninstall
$ eta-build install
```

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
