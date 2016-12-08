#!/usr/bin/env sh

stack setup && stack install && stack exec eta-build -- install $@
