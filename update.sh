#!/usr/bin/env sh

_pull=$(git pull)
git submodule sync
_subs=$(git submodule update --recursive)

if [ "$_pull" = "Already up-to-date." ] && [ -z "$_subs" ]; then
	echo "Already up-to-date."
	exit 0
fi

./cleaninstall.sh
etlas update
