#!/usr/bin/env sh

stack install && stack exec ghcvm-build -- clean && stack exec ghcvm-build -- uninstall && stack exec ghcvm-build -- install $@
