Eta User Guide
==============

.. note::

   This user guide is still under active development. Please consult the `GHC 7.10.3 User Guide <https://downloads.haskell.org/~ghc/7.10.3/docs/html/users_guide/>`_
   for language-level details as it is very similar to Eta.

Using the Eta CLI
-----------------

The Eta compiler can be invoked from the command line using the ``eta`` command.

.. code-block:: console

    eta [MODE_FLAGS] [OPTIONS] [SOURCE_FILES]

See `eta --help` for more details.

If you are using Etlas, can you directly invoke it as follows:

.. code-block:: console

   etlas exec eta -- [MODE_FLAGS] [OPTIONS] [SOURCE_FILES]

See `etlas exec eta -- --help` for more details.

Java Foreign Function Interface
-------------------------------

Overview
^^^^^^^^

The layer that interacts with Java in Eta is called the Foreign Function Interface
(FFI). This layer will allow you to import a Java method as an Eta function and
export a Eta function as a Java method. It automatically handles the intermediate
conversions between Java types and Eta types, so all you have to worry about is the
right type signature.

Background
^^^^^^^^^^^

The FFI revolves around some built-in types that Eta specially recognizes, so we'll
start by introducing them.

Unboxed & Primitive Types
"""""""""""""""""""""""""

Unboxed types are primitive types which must have values that are fully evaluated and
cannot have values that are suspended expressions (thunks) like normal types do.
Since they are primitives, they cannot be defined using the Eta language and need to
be given special support by the compiler.

They suffixed with a ``#`` and require the use of ``MagicHash`` language
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
| -               | ``Addr#``              | long                           |                                             |
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
| -               | ``RealWorld``          | eta.runtime.stg.Closure     |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Array``       | ``Array#``             | eta.runtime.io.Array        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``ByteArray#``         | eta.runtime.io.ByteArray    |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``ArrayArray#``        | eta.runtime.stg.Array       | Typically only contains ``ByteArray#`` and  |
|                 |                        |                                | ``ArrayArray#`` types as elements.          |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``SmallArray#``        | eta.runtime.stg.Array       | Identical to ``Array#``.                    |
|                 |                        |                                | Kept for compatibility with GHC.            |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableArray#``      | eta.runtime.io.Array        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableByteArray#``  | eta.runtime.io.ByteArray    |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``MutableArrayArray#`` | eta.runtime.stg.Array       | Typically only contains ``ByteArray#``,     |
|                 |                        |                                | ``ArrayArray#``, and the mutable variants   |
|                 |                        |                                | as elements.                                |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``SmallMutableArray#`` | eta.runtime.stg.Array       | Identical to ``MutableArray#``.             |
|                 |                        |                                | Kept for compatibility with GHC.            |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``IORef``,      |                        |                                |                                             |
| ``STRef``       | ``MutVar#``            | eta.runtime.io.MutVar       |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``MVar``        | ``MVar#``              | eta.runtime.concurrent.MVar |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``TVar``        | ``TVar#``              | eta.runtime.stm.TVar        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``StablePtr a`` | ``StablePtr#``         | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``StableName``  | ``StableName#``        | int                            |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| -               | ``BCO#``               | eta.runtime.interpreter.BCO |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``Weak``        | ``Weak#``              | eta.runtime.stg.Weak        |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+
| ``ThreadId``    | ``ThreadId#``          | eta.runtime.stg.TSO         |                                             |
+-----------------+------------------------+--------------------------------+---------------------------------------------+

Declaring Java Wrapper Types
""""""""""""""""""""""""""""

In Eta, you declare a JWT (Java Wrapper Type) in order to create a link between an Eta
data type and a Java class or interface. JWTs can be though of as wrappers over raw
Java objects.

.. code::

   data P a b c ... = P (@[class-name-here] a b c ...)
     deriving Class

This is the generic format for declaring a tag type where:

- ``P`` is the Eta name you would use to refer to it. Typically, ``P`` is the
  unqualified class name.
- ``a b c ...``` are the type variables that correspond to the generic parameters
  of the underlying Java class. Note that type variables in Eta *must* start with
  a lowercase letter.
- ``[class-name-here]`` is the name of a the class the tag type represents. For
  example, it can be ``java.lang.String``.

Example::

  data PrintStream = PrintStream (@java.io.PrintStream)
    deriving Class

In order to tell Eta about the JWT's parent/child relationships, a ``Class`` typeclass
instance and a ``Inherits`` type family declaration must be defined. A ``Class``
instance is obtained with the `deriving` clause above.

An example of declaring an instance of the ``Inherits`` type family is shown below:

Example::

  {-# LANGUAGE TypeFamilies #-}

  type instance Inherits PrintStream = '[FilterOutputStream]

The first element in the type-level list should be the parent class and the remaining
elements can be the interfaces it implements. Note that the ``TypeFamilies`` extension
must be enabled in order to declare the instance.

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

  import Java

  data Export = Export (@mypackage.Export)

  foreign export java sayHello :: JString -> Java Export JString

  sayHello n = return . toJava $ "Hello, " ++ fromJava n ++ "!"

And Java class that is generated:

.. code-block:: java

    package mypackage;

    public class Export {
        public Export() {}

        public String sayHello(String n) {
          // Code to invoke the Eta runtime system.
        }
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
            at eta.runtime.thunk.UpdatableThunk.enter(UpdatableThunk.java:19)
            at eta.runtime.stg.Closure.evaluate(Closure.java:20)
            at base.text.parsercombinators.ReadP$skipSpaceszuskip.enter(Unknown Source)
            at base.ghc.Read$satzus5BWW.enter(Unknown Source)
            at eta.runtime.apply.Function.apply(Function.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
            at eta.runtime.apply.Function.apply(Function.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$satzus341G.enter(Unknown Source)
            at eta.runtime.apply.Function.apply(Function.java:116)
            at eta.runtime.apply.Apply$8.enter(Apply.java:75)
            at base.text.parsercombinators.ReadP$run.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$satzus10SY.thunkEnter(Unknown Source)
            at eta.runtime.thunk.UpdatableThunk.enter(UpdatableThunk.java:19)
            at eta.runtime.stg.Closure.evaluate(Closure.java:20)
            at oldzmtimezm1zi1zi0zi3.system.Time$lvl98zus13J8.thunkEnter(Unknown Source)
            at eta.runtime.thunk.UpdatableThunk.enter(UpdatableThunk.java:19)
            at eta.runtime.stg.Closure.evaluate(Closure.java:20)
            at base.ghc.Base$zpzp.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$lvl112zus13P6.thunkEnter(Unknown Source)
            at eta.runtime.thunk.UpdatableThunk.enter(UpdatableThunk.java:19)
            at eta.runtime.stg.Closure.evaluate(Closure.java:20)
            at base.ghc.Base$zpzp.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$doFmtzus13PB.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$zdwformatCalendarTime.enter(Unknown Source)
            at oldzmtimezm1zi1zi0zi3.system.Time$zdfShowClockTimezuzdcshow.enter(Unknown Source)
            at eta.runtime.apply.Function.apply(Function.java:116)
            at eta.runtime.apply.ApP.stackEnter(ApP.java:17)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:43)
            at eta.runtime.stg.StgContext.checkForStackFrames(StgContext.java:75)
            at base.ghc.io.handle.Text$zdwa7.enter(Unknown Source)
            at base.ghc.io.handle.Text$hPutStr2.enter(Unknown Source)
            at base.system.IO$print1.enter(Unknown Source)
            at base.system.IO$print.enter(Unknown Source)
            at eta.runtime.apply.Apply$20.enter(Apply.java:210)
            at eta.runtime.apply.PAP.apply(PAP.java:46)
            at eta.runtime.apply.ApV.stackEnter(ApV.java:12)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:43)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:26)
            at eta.runtime.stg.StackFrame.enter(StackFrame.java:26)
            at eta.runtime.stg.Capability.schedule(Capability.java:245)
            at eta.RuntimeScheduler.scheduleWaitThread(RtsScheduler.java:57)
            at eta.Runtime.evalLazyIO(Rts.java:92)
            at eta.Runtime.main(Rts.java:37)
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

   If you ran the code without using ``etlas``, then you might have indicated its
   location using the ``-o`` option.

   **Example:** ``eta -o Out.jar SuperAwesomeModule.hs``

   If you did use ``etlas``, then its probable location is
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
