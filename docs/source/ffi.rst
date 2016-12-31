Interacting with Java
=====================

Overview
--------

The layer that interacts with Java in Eta is called the Foreign Function Interface
(FFI). This layer will allow you to import a Java method as a Eta function and
export a Eta function as a Java method. It automatically handles the intermediate
conversions between Java types and Eta types, so all you have to worry about is the
right type signature.

Background
-----------

The FFI revolves around some built-in types that Eta specially recognizes, so we'll
start by introducing them.

Unboxed & Primitive Types
^^^^^^^^^^^^^^^^^^^^^^^^^

Unboxed types are those types which must have a well-defined value and hence cannot
have values that are suspended expressions (thunks). These are typically raw integers,
floats, and objects at the JVM level. Moreover, these types can be thought of as
primitive and cannot be defined using the Eta language and hence need to be given
special support by the compiler.

They tend to be suffixed with a ``#`` and require the use of ``MagicHash`` language
extension in order to be recognized in source code. Primitive types have a special
representation at the JVM level, shown below.

Boxed Types
^^^^^^^^^^^^

Boxed types, on the other hand, can have values which can be thunks and may also be
undefined if the evaluation of the thunk leads to an error state. Boxed types can store
both boxed and unboxed values internally.

Example::

  data Int = I# Int#

Primitive Types Reference
^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^

In Eta, you regularly have to declare tag types. Tag types represent Java objects of a
given class in Eta and are typically wrappers for raw Java objects.

::

   data {-# CLASS "[class-name-here]"} P = P (Object# P)

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

::

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
-----------
TODO


Syntax
------

The following will show the general syntax and what will occur in each of the cases,
following by some examples.

Foreign imports
^^^^^^^^^^^^^^^
TODO

Foreign exports
^^^^^^^^^^^^^^^

The general syntax for foreign exports::

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

.. highlight:: java

And Java class that is generated::

  package hello;

  /* Imports */

  public class Export {
      public Export() {
      }

      public String sayHello(String var1) {
          /* Implementation */
      }
  }

Examples
---------
TODO
