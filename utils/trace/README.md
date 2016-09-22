# JVM Tracing Utility

This utility uses bytecode instrumentation to trace the execution of the JVM. Useful for debuggining minimal failing examples.

## Setup

1. Build `Filter.hs`.

  ``` $ stack exec -- ghc Filter.hs ```

  The `Filter` program is used to nicely indent the call stack.

2. Dump the archive `tracejars.tar.gz`.

  ``` $ tar -xzvf tracejars.tar.gz ```

## Running

Run the trace and dump it to a file.

```
$ ./trace.sh [path-to-jar] | ./Filter > TRACE_DUMP
```

Analyze the resulting `TRACE_DUMP` to find your bug.

Courtesy of [Brian McKenna](https://github.com/puffnfresh).
