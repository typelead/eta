#!/usr/bin/env sh

set -e

INSTALL_DIR="$1"
if [ \( -z "$INSTALL_DIR" \) -o \( "-" = "$INSTALL_DIR" \) ]
then
    INSTALL_DIR=`stack path --local-bin > /dev/null 2>&1 || echo "" > /dev/null`
    if [ -z "$INSTALL_DIR" ]
    then
        INSTALL_DIR=`stack path --local-bin-path`
    fi
fi

if [ "$#" -gt 0 ]; then shift; fi

stack install --local-bin-path="$INSTALL_DIR"
stack exec eta-build -- clean
stack exec eta-build -- uninstall
stack exec eta-build -- install "$@"
