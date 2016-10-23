## Debugging Stack traces in ghcvm

This document will debug stack trace for the following error message
which has been produced on running the program. This is useful in
filing in a more helpful bug report.

``` shell
Exception in thread "main" java.lang.NoClassDefFoundError: Calendar
        at oldzmtimezm1zi1zi0zi3.system.Time$satzus10SQ.thunkEnter(Unknown Source)
        at ghcvm.runtime.thunk.StgInd.enter(StgInd.java:19)
        at ghcvm.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
        at base.text.parsercombinators.ReadP$skipSpaceszuskip.enter(Unknown Source)
        at base.ghc.Read$satzus5BWW.enter(Unknown Source)
        at ghcvm.runtime.apply.StgFun.apply(StgFun.java:116)
        at ghcvm.runtime.apply.Apply$8.enter(Apply.java:75)
        at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
        at ghcvm.runtime.apply.StgFun.apply(StgFun.java:116)
        at ghcvm.runtime.apply.Apply$8.enter(Apply.java:75)
        at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
        at ghcvm.runtime.apply.StgFun.apply(StgFun.java:116)
        at ghcvm.runtime.apply.Apply$8.enter(Apply.java:75)
        at base.text.parsercombinators.ReadP$run.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$satzus10SY.thunkEnter(Unknown Source)
        at ghcvm.runtime.thunk.StgInd.enter(StgInd.java:19)
        at ghcvm.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
        at oldzmtimezm1zi1zi0zi3.system.Time$lvl98zus13J8.thunkEnter(Unknown Source)
        at ghcvm.runtime.thunk.StgInd.enter(StgInd.java:19)
        at ghcvm.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
        at base.ghc.Base$zpzp.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$lvl112zus13P6.thunkEnter(Unknown Source)
        at ghcvm.runtime.thunk.StgInd.enter(StgInd.java:19)
        at ghcvm.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
        at base.ghc.Base$zpzp.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$zdwformatCalendarTime.enter(Unknown Source)
        at oldzmtimezm1zi1zi0zi3.system.Time$zdfShowClockTimezuzdcshow.enter(Unknown Source)
        at ghcvm.runtime.apply.StgFun.apply(StgFun.java:116)
        at ghcvm.runtime.apply.ApP.stackEnter(ApP.java:17)
        at ghcvm.runtime.stg.StackFrame.enter(StackFrame.java:43)
        at ghcvm.runtime.stg.StgContext.checkForStackFrames(StgContext.java:75)
        at base.ghc.io.handle.Text$zdwa7.enter(Unknown Source)
        at base.ghc.io.handle.Text$hPutStr2.enter(Unknown Source)
        at base.system.IO$print1.enter(Unknown Source)
        at base.system.IO$print.enter(Unknown Source)
        at ghcvm.runtime.apply.Apply$20.enter(Apply.java:210)
        at ghcvm.runtime.apply.StgPAP.apply(StgPAP.java:46)
        at ghcvm.runtime.apply.ApV.stackEnter(ApV.java:12)
        at ghcvm.runtime.stg.StackFrame.enter(StackFrame.java:43)
        at ghcvm.runtime.stg.StackFrame.enter(StackFrame.java:26)
        at ghcvm.runtime.stg.StackFrame.enter(StackFrame.java:26)
        at ghcvm.runtime.stg.Capability.schedule(Capability.java:245)
        at ghcvm.runtime.RtsScheduler.scheduleWaitThread(RtsScheduler.java:57)
        at ghcvm.runtime.Rts.evalLazyIO(Rts.java:92)
        at ghcvm.runtime.Rts.hsMain(Rts.java:37)
        at ghcvm.main.main(Unknown Source)
Caused by: java.lang.ClassNotFoundException: Calendar
        at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
        at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:331)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
        ... 48 more
```

1. Parse the top most line

``` shell
oldzmtimezm1zi1zi0zi3.system.Time$satzus10SQ.thunkEnter(Unknown Source)
```

This gives the following information:

* Package name: `old-time`
* Module name: `System.Time`
* Thunk name: `satzus10SQ`

The error is in `old-time` package in the module `System.Time` in
expression `satzus10SQ` Which happens to be an thunk as can be
ascertained by the call of thunkEnter

2. Find the JAR file

If you ran the code without using `cabalvm`, then you might have
indicated it's location using the `-o` option (Example: `ghcvm -o
Out.jar SuperAwesomeModule.hs`).

If you did use `cabalvm`, then it's likely location is
`dist/build/<executable-name>/<executable-name>.jar`.

3. Extract JAR file

Perform extraction in that directory:

```shell
jar xf [executable-name].jar
```

4. Find thunk class file

``` shell
sibi::jane { ~/scripts }-> cd oldzmtimezm1zi1zi0zi3/system/
sibi::jane { ~/scripts/oldzmtimezm1zi1zi0zi3/system }-> ls Time\$satzus10SQ.class
Time$satzus10SQ.class
```

5. Run Java Class disassembler

``` shell
javap -c -v Time\$satzus10SQ.class
```

This will print readable Java bytecode. Submitting a bug report with bytecode
is highly helpful.

6. Produce STG Dump of the offending Package

In this case, this is for the package `old-time`. In the `.cabal` file
for the project, add a new field called `ghc-options` and set
`-ddump-stg -ddump-to-file` as the value.

Clean and re-build your package again. There will be a corresponding
`System/Time.dump-stg` file that is generated.

7. Decoding z-encoding

`satzus10SQ` is encoding using
[z-encoding](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames). Decode
it using the table found there. Decoding it for `satzus10SQ`, I get `sat_s10SQ`.

(Another example: Decode it for `satzus10ZZ8`. The solution is `sat_s10Z8`)

8. Get the STG definition for `sat_s10SQ`

Search the `Time.dump-stg` file for the definition of `sat_s10SQ` and
save that to a separate file.

Filing bug report with error message, STG dump and the bytecode is
highly helpful.
