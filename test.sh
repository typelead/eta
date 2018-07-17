#!/usr/bin/env sh

set -e

stack build eta:exe:eta-package-test
stack exec eta-package-test
