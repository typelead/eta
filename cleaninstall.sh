#!/usr/bin/env sh

stack install && stack exec eta-build -- clean && stack exec eta-build -- uninstall && stack exec eta-build -- install $@
