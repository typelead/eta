#!/usr/bin/env sh

git pull
git submodule update --recursive
./cleaninstall.sh
epm update
