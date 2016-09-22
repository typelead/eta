#!/usr/bin/env sh

java -javaagent:slf4j-ext-1.7.21.jar=ignore=org/slf4j/:ch/qos/logback/:org/apache/log4j/:cern/colt/:java/ -cp slf4j-api-1.7.21.jar:slf4j-simple-1.7.21.jar:javassist-3.20.0-GA.jar:"$1" -Djava.compiler=NONE ghcvm.main
