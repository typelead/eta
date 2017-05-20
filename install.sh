#!/usr/bin/env sh

set -e

stack setup
./cleaninstall.sh "$@"
