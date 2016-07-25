#!/usr/bin/env sh

stack build && stack exec ghcvm-build uninstall && stack exec ghcvm-build install && stack install ghcvm
