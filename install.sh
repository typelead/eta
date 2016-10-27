#!/usr/bin/env sh

stack install && stack exec eta-build -- install $@
