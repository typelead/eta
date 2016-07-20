#!/usr/bin/env sh

stack build
stack exec ghcvm-build clean
stack exec ghcvm-build
(cd ./sample/mapandsum/haskell/; stack build)

