#!/usr/bin/env sh

set -e

rm -rf ./lib
mvn dependency:copy-dependencies "-DoutputDirectory=./lib"

javac -classpath ".:lib/*" UploadBinaries.java
java -classpath ".:lib/*" UploadBinaries


