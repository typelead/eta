#!/usr/bin/env sh

set -e

rm -rf binaries/ release.log
mkdir -p binaries
touch ./binaries/base-index
./install.sh - --binaries=binaries 2>&1 | tee release.log
cat release.log | grep "^Installed" | cut -d" " -f2 >> binaries/base-index
cp binaries/base-index binaries/index
cd utils/scripts
./rs-upload.sh
