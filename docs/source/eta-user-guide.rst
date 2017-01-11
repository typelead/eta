Eta User Guide
==============

.. note::

   This user guide is still under active development. Please consult the `GHC 7.10.3 User Guide <https://downloads.haskell.org/~ghc/7.10.3/docs/html/users_guide/>`_
   for language-level details as it is very similar to Eta.

Interacting with Java
---------------------

Overview
^^^^^^^^

The layer that interacts with Java in Eta is called the Foreign Function Interface
(FFI). This layer will allow you to import a Java method as a Eta function and
export a Eta function as a Java method. It automatically handles the intermediate
conversions between Java types and Eta types, so all you have to worry about is the
right type signature.

Background
^^^^^^^^^^^

The FFI revolves around some built-in types that Eta specially recognizes, so we'll
start by introducing them.

Unboxed & Primitive Types
"""""""""""""""""""""""""

Unboxed types are those types which must have a well-defined value and hence cannot
have values that are suspended expressions (thunks). These are typically raw integers,
floats, and objects at the JVM level. Moreover, these types can be thought of as
primitive and cannot be defined using the Eta language and hence need to be given
special support by the compiler.

They tend to be suffixed with a ``#`` and require the use of ``MagicHash`` language
extension in order to be recognized in source code. Primitive types have a special
representation at the JVM level, shown below.

Boxed Types
""""""""""""

Boxed types, on the other hand, can have values which can be thunks and may also be
undefined if the evaluation of the thunk leads to an error state. Boxed types can store
both boxed and unboxed values internally.

Example::

  data Int = I# Int#

Primitive Types Reference
"""""""""""""""""""""""""

+-----------------+------------------------+--------------------------------+---------------------------------------------+
| Boxed Type      | Primitive Type         | Java Type                      | Notes                                       |
+=================+========================+================================+=============================================+
| ``Char``        | ``Char#``              | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Int``         | ``Int#``               | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Int``         | ``Int#``               | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Int32``       | ``Int32#``             | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Int64``       | ``Int64#``             | long                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Word``        | ``Word#``              | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Word32``      | ``Word32#``            | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Word64``      | ``Word64#``            | long                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Float``       | ``Float#``             | float                          |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Double``      | ``Double#``            | double                         |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``JChar#``             | char                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``JString``     | ``JString#``           | java.lang.String               | To cast it to Eta ``String``,               |
|                 |                        |                                | use ``fromJString`` from ``Java.String``    |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Bool``        |  ``JBool#``            | boolean                        | This is a special case, since ``Bool``      |
|                 |                        |                                | is not exactly the boxed form of ``JBool#`` |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``JByte#``             | byte                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``JShort#``            | short                          |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``Object# c``          | Reference type                 | Depends on the tag of c                     |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``Addr#``              | java.nio.ByteBuffer            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ````State# a````       | none                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``Void#``              | none                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Proxy``       | ``Proxy#``             | none                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``a ~ b``       | ``a ~# b``             | none                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``a ~R b``      | ``a ~R# b``            | none                           |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``RealWorld``          | eta.runtime.stg.StgClosure     |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Array``       | ``Array#``             | eta.runtime.io.StgArray        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``ByteArray#``         | eta.runtime.io.StgByteArray    |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``ArrayArray#``        | eta.runtime.stg.StgArray       | Typically only contains ``ByteArray#`` and  |
|                 |                        |                                | ``ArrayArray#`` types as elements.          |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``SmallArray#``        | eta.runtime.stg.StgArray       | Identical to ``Array#``.                    |
|                 |                        |                                | Kept for compatibility with GHC.            |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableArray#``      | eta.runtime.io.StgArray        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableByteArray#``  | eta.runtime.io.StgByteArray    |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableArrayArray#`` | eta.runtime.stg.StgArray       | Typically only contains ``ByteArray#``,     |
|                 |                        |                                | ``ArrayArray#``, and the mutable variants   |
|                 |                        |                                | as elements.                                |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``SmallMutableArray#`` | eta.runtime.stg.StgArray       | Identical to ``MutableArray#``.             |
|                 |                        |                                | Kept for compatibility with GHC.            |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``IORef``,      |                        |                                |                                             |
| ``STRef``       | ``MutVar#``            | eta.runtime.io.StgMutVar       |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``MVar``        | ``MVar#``              | eta.runtime.concurrent.StgMVar |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``TVar``        | ``TVar#``              | eta.runtime.stm.StgTVar        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``StablePtr a`` | ``StablePtr#``         | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``StableName``  | ``StableName#``        | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``BCO#``               | eta.runtime.interpreter.StgBCO |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Weak``        | ``Weak#``              | eta.runtime.stg.StgWeak        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``ThreadId``    | ``ThreadId#``          | eta.runtime.stg.StgTSO         |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+

Declaring Tag Types
"""""""""""""""""""

In Eta, you regularly have to declare tag types. Tag types represent Java objects of a
given class in Eta and are typically wrappers for raw Java objects.

.. code::

   data {-# CLASS "[class-name-here]" #-} P = P (Object# P)

This is the generic format for declaring a tag type where:

- ``[class-name-here]`` is the name of a the class the tag type represents. For
  example, it can be ``java.lang.String``.
- ``P`` is the Eta name you would use to refer to it. Typically, ``P`` is the
  unqualified class name.

Example::

  data {-# CLASS "java.io.PrintStream" #-} PrintStream =
    PrintStream (Object# PrintStream)

In order to tell Eta about it's parent/child relationships for use in the strongly
typed usages of the FFI, a ``Class`` typeclass instance and a ``Super`` type family
declaration must be defined. The ``Class`` typeclass contains methods that the FFI
internally uses to get the underlying raw Java object from the tag type in the cases
where one does polymorphic FFI imports. The ``Super`` type family defines the direct
parent relationship of the class and that will be extended into an entire class
hierarchy within Eta using the laws defined for the ``Extends`` typeclass. The
``Extends a b`` typeclass is a multi-parameter typeclass that stores a relationship
that ``a`` is descendent of ``b``.

.. code::

   {-# LANGUAGE TypeFamilies #-}
   class Class c where
       obj :: Object# c -> c
       unobj :: c -> Object# c

   type family Super (a :: *) :: *

Example::

  {-# LANGUAGE TypeFamilies #-}
  class Class PrintStream where
      obj = PrintStream
      unobj (PrintStream o) =  o

  type instance Super PrintStream = FilterOutputStream

Java Monad
^^^^^^^^^^^
TODO


Syntax
^^^^^^

The following will show the general syntax and what will occur in each of the cases,
following by some examples.

Foreign imports
"""""""""""""""
TODO

Foreign exports
"""""""""""""""

The general syntax for foreign exports:

.. code-block:: console

  foreign export java "javaFunctionName" functionName :: var1 -> var2 -> var3
    -> Java tagType returnType

Where:

* ``javaFunctionName`` - identifier of java method that is generated for ``tagType``
  class
* ``functionName`` - haskell function name that is exported. The name can be omitted
  and the generated Java method will have the same name as Eta function.
* ``var<N>`` - argument types that can be marshalled into Java types.
  (TODO: which types can be marshalled?)
* ``tagType`` - [tag type](#declaring-tag-types) that corresponds to Java class where
  the function will be generated. You cannot specify polymorphic type variable, only
  specialised one (see `#77 <https://github.com/typelead/eta/issues/77>`_.
* ``returnType`` - return type that can be marshalled back from Java into Eta.
  (TODO: which types can be marshalled?)

The following example::

  import GHC.Base
  import GHC.Pack

  data {-# CLASS "mypackage.Export" #-} Export = Export (Object# Export)

  foreign export java sayHello :: JString -> Java Export JString

  sayHello n = return . mkJString $ "Hello, " ++ unpackCString n ++ "!"

And Java class that is generated:

.. code-block:: java

    package hello;

    public class Export {
        public Export() {}

        public String sayHello(String var1) {}
    }

Examples
^^^^^^^^^
TODO

Debugging Stack Traces
----------------------

This document will debug stack trace for the following error message which has been
produced on running the program. This is useful in filing a more helpful bug report.

.. code-block:: console

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

   .. code-block:: console

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

   Perform extraction in that directory:

   .. code-block:: console

      jar xf [executable-name].jar

#. **Find thunk class file**

   .. code-block:: console

      cd oldzmtimezm1zi1zi0zi3/system/
      ls Time\$satzus10SQ.class

#. **Run the Java Class disassembler**

   .. code-block:: console

      javap -c -v Time\$satzus10SQ.class

   This will print readable Java bytecode. Submitting a bug report with bytecode
   is very helpful.

#. **Produce STG Dump of the Package**

   In this case, the package is ``old-time``. In the ``.cabal`` file
   for the project, add a new field called ``ghc-options`` and set
   ``-ddump-stg -ddump-to-file`` as the value.

   Clean and re-build your package again. There will be a corresponding
   ``System/Time.dump-stg`` file that is generated.

#. **Decoding z-encoding**

   ``satzus10SQ`` is encoded using
   `z-encoding <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames>`_. Decode it using the table found there. Decoding ``satzus10SQ`` gives
   ``sat_s10SQ``.

   **Another example:** ``satzus10ZZ8`` decodes to ``sat_s10Z8``

#. **Get the STG Definition**

   Search the ``Time.dump-stg`` file for the definition of ``sat_s10SQ`` and save that
   to a separate file.

   Filing bug report with error message, STG dump and the bytecode is highly
   helpful. You can find an example dump of these three messages
   `here <https://gist.github.com/psibi/5bb5387912dec1ca9817cba7de7a1dac>`_.

