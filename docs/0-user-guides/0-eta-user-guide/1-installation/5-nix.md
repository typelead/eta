# Nix

## Prerequisites

Make sure you have the following tools installed on your system:

- [Nix](https://nixos.org/nix/)

## Installation

Install the Nix overlay:

```sh
$ git clone https://github.com/eta-lang/eta-nix.git
$ mkdir -p ~/.config/nixpkgs/overlays
$ ln -s $PWD/eta-nix/overlay.nix ~/.config/nixpkgs/overlays/eta-overlay.nix
```

To obtain an environment with `eta` and the `lens` package available:

```sh
$ nix-shell -p 'etaPackages.etaWithPackages (p: [ p.lens ])'
```

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
