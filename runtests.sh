#!/usr/bin/env sh

(cd rts; ./gradlew test)
stack setup
stack build eta:exe:eta-package-test eta:exe:eta-test-suite
stack exec eta-test-suite
stack exec eta-package-test
