#!/usr/bin/env sh

set -e

INSTALL_DIR="$1"
if [ \( -z "$INSTALL_DIR" \) -o \( "-" = "$INSTALL_DIR" \) ]
then
    INSTALL_DIR=`stack path --local-bin 2> /dev/null || echo ""`
    if [ -z "$INSTALL_DIR" ]
    then
        INSTALL_DIR=`stack path --local-bin-path`
    fi
fi

if [ "$#" -gt 0 ]; then shift; fi

stack install eta:exe:eta eta-pkg etlas eta-build --local-bin-path="$INSTALL_DIR" -j1
stack exec eta-build -- clean
stack exec eta-build -- uninstall
stack exec eta-build -- install "$@"
