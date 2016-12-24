Debugging Stack Traces
======================

This document will debug stack trace for the following error message which has been
produced on running the program. This is useful in filing a more helpful bug report.

.. highlight:: java

.. code::

    Exception in thread "main" java.lang.NoClassDefFoundError: Calendar
            at oldzmtimezm1zi1zi0zi3.system.Time$satzus10SQ.thunkEnter(Unknown Source)
            at eta.runtime.thunk.StgInd.enter(StgInd.java:19)
            at eta.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
            at base.text.parsercombinators.ReadP$skipSpaceszuskip.enter(Unknown Source)
            at base.ghc.Read$satzus5BWW.enter(Unknown Source)
            at eta.runtime.apply.StgFun.apply(StgFun.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
            at eta.runtime.apply.StgFun.apply(StgFun.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
            at eta.runtime.apply.StgFun.apply(StgFun.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$run.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$satzus10SY.thunkEnter(Unknown Source)
            at eta.runtime.thunk.StgInd.enter(StgInd.java:19)
            at eta.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
            at oldzmtimezm1zi1zi0zi3.system.Time$lvl98zus13J8.thunkEnter(Unknown Source)
            at eta.runtime.thunk.StgInd.enter(StgInd.java:19)
            at eta.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
            at base.ghc.Base$zpzp.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$lvl112zus13P6.thunkEnter(Unknown Source)
            at eta.runtime.thunk.StgInd.enter(StgInd.java:19)
            at eta.runtime.stg.StgClosure.evaluate(StgClosure.java:20)
            at base.ghc.Base$zpzp.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$zdwformatCalendarTime.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$zdfShowClockTimezuzdcshow.enter(Unknown Source)
            at eta.runtime.apply.StgFun.apply(StgFun.java:116)
            at eta.runtime.apply.ApP.stackEnter(ApP.java:17)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:43)
            at eta.runtime.stg.StgContext.checkForStackFrames(StgContext.java:75)
            at base.ghc.io.handle.Text$zdwa7.enter(Unknown Source)
            at base.ghc.io.handle.Text$hPutStr2.enter(Unknown Source)
            at base.system.IO$print1.enter(Unknown Source)
            at base.system.IO$print.enter(Unknown Source)
            at eta.runtime.apply.Apply$20.enter(Apply.java:210)
            at eta.runtime.apply.StgPAP.apply(StgPAP.java:46)
            at eta.runtime.apply.ApV.stackEnter(ApV.java:12)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:43)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:26)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:26)
            at eta.runtime.stg.Capability.schedule(Capability.java:245)
            at eta.runtime.RtsScheduler.scheduleWaitThread(RtsScheduler.java:57)
            at eta.runtime.Rts.evalLazyIO(Rts.java:92)
            at eta.runtime.Rts.hsMain(Rts.java:37)
            at eta.main.main(Unknown Source)
    Caused by: java.lang.ClassNotFoundException: Calendar
            at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
            at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
            at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:331)
            at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
            ... 48 more

#. **Parse the top most line**

   .. code::

      oldzmtimezm1zi1zi0zi3.system.Time$satzus10SQ.thunkEnter(Unknown Source)

   This gives the following information:

   * Package name: ``old-time``
   * Module name: ``System.Time``
   * Thunk name: ``satzus10SQ``

   The error is in ``old-time`` package in the module ``System.Time`` in expression
   ``satzus10SQ`` which happens to be an thunk as can be ascertained by the call of
   ``thunkEnter``.

#. **Find the JAR file**

   If you ran the code without using ``epm``, then you might have indicated its
   location using the ``-o`` option.

   **Example:** ``eta -o Out.jar SuperAwesomeModule.hs``

   If you did use ``epm``, then its probable location is
   ``dist/build/<executable-name>/<executable-name>.jar``.

#. **Extract JAR file**

   Perform extraction in that directory::

     jar xf [executable-name].jar

#. **Find thunk class file**

   ::

      cd oldzmtimezm1zi1zi0zi3/system/
      ls Time\$satzus10SQ.class

#. **Run the Java Class disassembler**

   ::

      javap -c -v Time\$satzus10SQ.class

   This will print readable Java bytecode. Submitting a bug report with bytecode is
   very helpful.

#. **Produce STG Dump of the Package**

   In this case, the package is ``old-time``. In the ``.cabal`` file
   for the project, add a new field called ``ghc-options`` and set
   ``-ddump-stg -ddump-to-file`` as the value.

   Clean and re-build your package again. There will be a corresponding
  ``System/Time.dump-stg`` file that is generated.

#. **Decoding z-encoding**

   ``satzus10SQ`` is encoded using
    [z-encoding](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames)    . Decode it using the table found there. Decoding ``satzus10SQ`` gives
   ``sat_s10SQ``.

    **Another example:** ``satzus10ZZ8`` decodes to ``sat_s10Z8``

#. **Get the STG Definition**

   Search the ``Time.dump-stg`` file for the definition of ``sat_s10SQ`` and save that
   to a separate file.

   Filing bug report with error message, STG dump and the bytecode is highly helpful.
   You can find an example dump of these three messages
   [here](https://gist.github.com/psibi/5bb5387912dec1ca9817cba7de7a1dac).
