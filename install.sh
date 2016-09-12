#!/usr/bin/env sh

stack build && stack install && stack exec ghcvm-build -- install $@
