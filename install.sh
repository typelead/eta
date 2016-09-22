#!/usr/bin/env sh

stack install && stack exec ghcvm-build -- install $@
