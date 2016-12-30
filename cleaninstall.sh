#!/usr/bin/env sh

INSTALL_DIR="$1"
if [ -z "$INSTALL_DIR" ]
then
    INSTALL_DIR=`stack path --local-bin`
fi

shift

stack install --local-bin-path="$INSTALL_DIR" && stack exec eta-build -- clean && stack exec eta-build -- uninstall && stack exec eta-build -- install "$@"
