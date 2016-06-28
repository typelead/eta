#!/usr/bin/env sh

if [ ! -f "./rts/build/rts.jar" ]; then
    echo "Error: rts.jar not found!"
    echo "Run ./build.sh first."
fi

if [ ! -f "./sample/build/mapandsum.jar" ]; then
    echo "Error: mapandsum.jar not found!"
    echo "Run ./build.sh first."
fi

jdb -classpath "rts/build/rts.jar:sample/build/mapandsum.jar" mapandsum.Main "$@"
