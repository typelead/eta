#!/usr/bin/env sh

set -e

stack build eta:exe:eta-test
stack exec eta-test
