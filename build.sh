#!/usr/bin/env sh

stack build && stack install ghcvm
#stack exec ghcvm-build uninstall; stack exec ghcvm-build install

