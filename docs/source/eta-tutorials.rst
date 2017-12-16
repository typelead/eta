.. _eta-tutorials:

Eta Tutorials
=============

.. _interacting-with-java:

Interacting with Java
---------------------

In this section, we will cover all the different ways you can interact with Java in
Eta so that you can reuse your favorite Java libraries. The mechanism for
interacting with Java in Eta is called the **Foreign Function Interface (FFI)**.

To use Java methods inside Eta, you must first import them with **foreign import
declarations**. To use Eta methods inside of Java, you must first export them with
**foreign export declarations**. The remaining sections detail how to do each.

Prerequisites
^^^^^^^^^^^^^

You must have a basic understanding of monads to understand the rest of the
section.

Quick Start
^^^^^^^^^^^

When interfacing with Java, you should import the ``Java`` module from the standard
library and enable the ``MagicHash`` language extension like so::

   {-# LANGUAGE MagicHash #-}

   import Java

This will import the ``Java`` monad and related helper functions for working inside
the monad.

Consider the following Java code:

.. code-block:: java

   package eta.example;

   public class Counter {

     private int counter;

     public int publicCounter;

     public static final int COUNTER_MAX = 1000;

     public static int numCounters;

     public Counter() {}

     public Counter(int initial) {
       this.counter = initial;
     }

     public void increment() {
       this.counter = Math.min(this.counter + 1, COUNTER_MAX);
       this.publicCounter = counter;
     }

     public int get() {
       return counter;
     }

     public void set(int value) {
       this.counter = Math.min(value, COUNTER_MAX);
       this.publicCounter = counter;
     }

   }

A Java method is simply a function that takes an object as an implicit argument
bound to the ``this`` variable. Implicit contexts such as these can be represented
as a monad, a state monad to be specific. A state monad threads state through each
monadic action so the state is being passed around internally even though it's not
visible in the code.

This correspondence is the basis for the built-in ``Java`` monad in Eta.

The above example can be imported as follows:

.. code::

   data Counter = Counter @eta.example.Counter
     deriving Class

   foreign import java unsafe "@new" newCounter :: Java a Counter
   foreign import java unsafe "@new" newCounterWith :: Int -> Java a Counter
   foreign import java unsafe increment :: Java Counter ()
   foreign import java unsafe get :: Java Counter Int
   foreign import java unsafe set :: Int -> Java Counter ()
   foreign import java unsafe "@static @field eta.example.Counter.COUNTER_MAX"
     cOUNTER_MAX :: Java a Int
   foreign import java unsafe "@field publicCounter" getPublicCounter
     :: Java Counter Int
   foreign import java unsafe "@field publicCounter" setPublicCounter
     :: Int -> Java Counter ()
   foreign import java unsafe "@static @field numCounters" getNumCounters
     :: Java a Int
   foreign import java unsafe "@static @field numCounters" setNumCounters
     :: Int -> Java a ()

Now let's examine each of declarations above in more detail.

.. _java-wrapper-type:

Defining a Java Wrapper Type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When working with the FFI, you need a way to refer to a given Java class inside of
Eta. This is done through **Java Wrapper Types (JWTs)**.

General Syntax
""""""""""""""

**Syntax**

.. code::

   data X = X @[class-name]
     deriving Class

- ``[class-name]`` should be the fully qualified Java class name and ``X`` should
  be the Eta name you would use to refer to the corresponding Java class in foreign
  imports. Note that ``[class-name]`` can also be converted to an array type by
  appending ``[]``.
- The ``Class`` typeclass is a built-in typeclass that is a marker for a JWT.
  **Make sure all your JWTs derive a Class instance.**

**Example**

.. code::

   data JInteger = JInteger @java.lang.Integer
     deriving Class
   data JIntegers = JIntegers @java.lang.Integer[]
     deriving Class

In this example, we're declaring JWTs for the ``java.lang.Integer`` class and the
``java.lang.Integer[]`` array (which is technically a class on its own).

.. note::

   You may find the declaration syntax a bit cumbersome or even confusing. If you have
   a preference of syntax, please let us know!

Deriving Standard Typeclass Instances
"""""""""""""""""""""""""""""""""""""

**Syntax**

.. code::

   data X = X @[class-name]
     deriving (Class, Eq, Show)

Currently, deriving the ``Class``, ``Eq``, and ``Show`` instances for JWTs is
supported. You should derive these instances based on the need of the application.
The ``Eq`` instance will use the underlying ``Object.equals()`` method and the
``Show`` instance will use ``Object.toString()``.

.. _marshalling-java-eta:

Marshalling between Java and Eta types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When writing FFI declarations, you are essentially specifying the type of a function
whose arguments will be translated from Eta types to Java types and whose result
will be translated from Java types to Eta types. This translation process is called
**marshalling**.

Java Primitives
"""""""""""""""

The following table lists the mapping from primitive Java types to Eta types.

+-----------------+------------+
|    Java Type    |  Eta Type  |
+=================+============+
|   ``boolean``   |  ``Bool``  |
+-----------------+------------+
|    ``byte``     |  ``Byte``  |
+-----------------+------------+
|    ``short``    | ``Short``  |
+-----------------+------------+
|    ``char``     | ``JChar``  |
+-----------------+------------+
|    ``int``      |  ``Int``   |
+-----------------+------------+
|    ``long``     | ``Int64``  |
+-----------------+------------+
|    ``float``    | ``Float``  |
+-----------------+------------+
|    ``double``   | ``Double`` |
+-----------------+------------+

.. note::

   All the Eta types shown above can be treated as numbers since they have
   instances for ``Num`` and related typeclasses. Thus, you can use functions
   like ``fromIntegral`` to convert between them.


Java Classes & Arrays
"""""""""""""""""""""

A Java Wrapper Type, as mentioned in the section :ref:`java-wrapper-type`, will
marshal to an object of the class given in the ``CLASS`` annotation. Note that
a Java array is just a special class with a ``length`` member, hence you should
declare JWT to specify that you want to marshal to a Java array.

The following table shows a couple of Eta types which aren't JWTs, but still
marshal to a Java class or return type:

+------------------------------+--------------+
|    Java Type                 |  Eta Type    |
+==============================+==============+
|   ``java.lang.String``       | ``String``   |
+------------------------------+--------------+
|   Any nullable object ``X``  | ``Maybe X``  |
+------------------------------+--------------+
|   ``void``                   | ``()``       |
+------------------------------+--------------+

When writing FFI declarations that return objects, you can wrap the result type
in a ``Maybe`` if the documentation of the corresponding Java method clearly
states that ``null`` is a potential return type. It is always safe to wrap the
result in a ``Maybe`` type, but the user will have to bear the burden of dealing
with an unnecessary case if the result is always a non-``null`` object.

If the ``Maybe`` type is not used for a method that actually does return ``null``, then
a ``NullPointerException`` will occur when a method is invoked on that object.

.. note::

   Since ``java.lang.String`` is special and also used quite frequently in Java,
   a JWT called ``JString`` is already provided for you. It supports the
   ``OverloadedStrings`` extension so you can have expressions like
   ``"Some string" :: JString``.

The Java Monad
^^^^^^^^^^^^^^

As mentioned before, the ``Java`` monad is used to contain the implicit ``this``
context. It can be effectively thought as a state monad with a given Java object
as the state.

.. code::

   newtype Java c a = Java {- Internal definition -}

As can be seen from the above definition, the ``Java`` monad has *two* type
parameters ``c`` and ``a``. The ``c`` parameter should be some JWT and the ``a``
parameter is the return type of the monad.

Java Foreign Import Declarations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Foreign import declarations are used to import a Java method as an Eta monadic
action, typically in the Java monad.

General Syntax
""""""""""""""

.. code-block:: console

   foreign import java [safety] "[import-string]" [eta-identifier]
     :: [arg-type-1] -> [arg-type-2] -> .. -> [return-type]

#. ``[safety]`` can either be ``safe``, ``unsafe``, or left unspecified in which
   case it is considered as ``safe``.

   - ``unsafe`` is the option you would typically select. In this case, the java
     method identified in the ``[import-string]`` will be run directly. This can
     be dangerous if the function can block in which case it will block the Eta
     RTS and reduce efficiency.

   - ``safe`` is the option you would select for functions that you would expect to
     block for some time, so they will be safely run in another thread to prevent
     the call from blocking the Eta RTS. This option must also be
     used when importing a Java method that eventually calls an exported Eta
     function.

#. ``[import-string]`` can take the following forms:

   - ``[java-method-name]``: Binds to an instance method. ``[java-method-name]``
     should be an unqualified Java instance method name.

   - ``@static [java-method-name]``: Binds to a static method.
     ``[java-method-name]`` should be a fully qualified Java static method name.

   - ``@new``: Binds to a constructor. The class to construct will be determined by
     the return type of the declaration.

   - ``@field [java-field-name]``: Binds to a getter or setter of an instance
     field, determined by the type signature. ``[java-field-name]`` should be an
     unqualified Java instance field name.

   - ``@static @field [java-field-name]``: Binds to a getter or setter of a field,
     determined by the type signature. ``[java-field-name]`` should be a
     fully qualified Java static field name.

   - ``@interface [java-interface-method]``: Binds to an interface method,
     determined by the type signature. ``[java-interface-name]`` should be a
     unqualified Java interface method name.

   - ``@wrapper [java-interface-method]``: Used for generating an Eta function
     that will generate an interface implementation, determined by the type
     signature.
     ``[java-interface-name]`` should be a unqualified Java interface method name.
     See :ref:`working-with-java-interfaces` for more information.

   - ``@wrapper @abstract [java-abstract-method]``:  Used for generating an Eta
     function that will generate an abstract class implementation, determined by
     the type signature.
     ``[java-method]`` should be a unqualified Java abstract method name.
     See :ref:`working-with-java-interfaces` for more information.

   - Not present: If you do not specify an import string, it will be taken as an
     instance method import and the ``[java-method-name]`` is taken to be the
     same as ``[eta-identifier]``.

#. ``[eta-identifier]`` should be a valid Eta identifier that will be used for
   calling the corresponding Java method inside of Eta code.

#. ``[argTypeN]`` should be a marshallable Eta type. See
   :ref:`marshalling-java-eta`.

#. ``[returnType]`` can be of three forms:

   - ``Java [jwt] [return-type]``: This is the form that is used typically and
     is always safe to use. ``[jwt]`` should be the JWT for the class which the
     declaration pertains. If the declaration has a ``@static`` annotation,
     this can be left free with a type variable instead of a concrete type.
     ``[return-type]`` should be a marshallable Eta type.

   - ``IO [return-type]``: This form is also safe and can be used for convenience.
     Note that if the import string does not have a ``@static`` annotation, you must
     supply the relevant JWT as the first argument (``[argType1]``).
     ``[return-type]`` should be a marshallable Eta type.

   - ``[return-type]``: This form has no monadic context and should only be used
     for immutable Java objects whose methods do not perform any side effects. Note
     that if the declaration does not have a ``@static`` annotation, you must
     supply the relevant JWT as the first argument (``[argType1]``).
     ``[return-type]`` should be a marshallable Eta type.

.. _java-imports-examples:

Examples
""""""""

**Importing Instance Methods**

Let's import the ``boolean canExecute()`` `instance method <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#canExecute()>`_
from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the import::

  data File = File @java.io.File
    deriving Class

  foreign import java unsafe canExecute :: Java File Bool
  foreign import java unsafe "canExecute" canExecute1 :: Java File Bool
  foreign import java unsafe "canExecute" canExecute2 :: File -> IO Bool
  -- Note: The example below is shown for illustration purposes and should never
  -- be done in practice because "canExecute" is not a pure function.
  foreign import java unsafe "canExecute" canExecute3 :: File -> Bool

**Importing Static Methods**

Let's import the ``File createTempFile(String, String)`` `static method <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)>`_ from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the import::

  data File = File @java.io.File
    deriving Class

  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile  :: String -> String -> Java a File
  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile1 :: String -> String -> IO File
  -- Note: The example below is shown for illustration purposes and should never
  -- be done in practice because "createTempFile" is not a pure function.
  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile2 :: String -> String -> File

**Importing Constructors**

Let's import the ``File(String)`` `constructor <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#File(java.lang.String)>`_ from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the import::

  data File = File @java.io.File
    deriving Class
  foreign import java unsafe "@new" newFile  :: String -> Java a File
  foreign import java unsafe "@new" newFile1 :: String -> IO File
  foreign import java unsafe "@new" newFile2 :: String -> File

**Importing Instance Fields**

Let's import the ``private String path`` `instance field <http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java/#165>`_
from the `java.io.File <http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java>`_ class. Note that the imports shown below
are purely for illustration purposes and will throw an exception if called because
``path`` is a private field.

The following are all equivalent ways of performing the get/set imports::

  data File = File @java.io.File
    deriving Class
  -- Imports for getting the field
  foreign import java unsafe "@field path" getFilePath  :: Java File String
  foreign import java unsafe "@field path" getFilePath1 :: File -> IO String
  foreign import java unsafe "@field path" getFilePath2 :: File -> String

  -- Imports for setting the field.
  foreign import java unsafe "@field path" setFilePath  :: String -> Java File ()
  foreign import java unsafe "@field path" setFilePath1 :: File -> String -> IO ()
  -- Note that setting the value of a field is always an impure operation so a pure
  -- import is not supported by the compiler

**Importing Static Fields**

Let's import the ``String pathSeparator`` `static field <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#pathSeparator>`_
from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the get/set imports::

  -- Imports for getting the field
  foreign import java unsafe "@static @field java.io.File.pathSeparator"
    getPathSeparator  :: Java a String
  foreign import java unsafe "@static @field java.io.File.pathSeparator"
    getPathSeparator1 :: IO String
  foreign import java unsafe "@static @field java.io.File.pathSeparator"
    getPathSeparator2 :: String

  -- Imports for setting the field.
  -- NOTE: These imports are only shown for illustration purposes, but they will
  -- crash if used since `pathSeparator` is a final field.
  foreign import java unsafe "@static @field java.io.File.pathSeparator"
    setPathSeparator  :: String -> Java a ()
  foreign import java unsafe "@static @field java.io.File.pathSeparator"
    setPathSeparator1 :: String -> IO ()

Working With the Java Monad
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now that we've gotten an idea of how to use imports, how do we use them in Eta code?
Eta code must eventually run in the ``IO`` monad and we currently don't know how
that can be done if we have an import that runs in the ``Java`` monad.

In the `Java <https://github.com/typelead/eta/blob/master/libraries/base/Java/Core.hs#L37>`_ module in the ``base`` package, the following functions are
available::

  -- Execute a Java action in the IO monad.
  java :: Java c a -> IO a

  -- Execute a Java action in the IO monad with respect to the
  -- given object.
  javaWith :: (Class c) => c -> Java c a -> IO a

  -- Execute a Java action in the Java monad of another class
  -- with respect to the given object.
  (<.>) :: (Class c) => c -> Java c a -> Java b a
  withObject :: (Class c) => c -> Java c a -> Java b a

  -- Chain Java actions.
  (>-) :: (Class b) => Java a b -> Java b c -> Java a c

  -- Execute an IO action inside of the Java monad
  io :: IO a -> Java c a

  -- Execute a Java action purely, i.e. order of execution does not matter.
  pureJava :: Java c a -> a

  -- Analagous to `javaWith`, but pure.
  pureJavaWith :: (Class c) => c -> Java c a -> a


Using the imports from :ref:`java-imports-examples`, we can write the following
program::

  main :: IO ()
  main = do
    executes <- java $ do
      file <- newFile "./dir/prog.exe"
      io $ putStrLn "Executing an IO action inside of Java!"
      file <.> canExecute
    if executes
    then putStrLn "File can execute!"
    else putStrLn "File cannot execute!"

Using different combinators, we can write it like this::

  main :: IO ()
  main = do
    -- Similar to Java code:
    -- File file = new File("./dir/prog.exe");
    file <- java $ newFile "./dir/prog.exe"
    putStrLn "Executing an IO action inside of Java!"
    -- Similar to Java code:
    -- boolean executes = file.canExecute();
    executes <- javaWith file canExecute
    if executes
    then putStrLn "File can execute!"
    else putStrLn "File cannot execute!"

Or::

  main :: IO ()
  main = java $ do
    -- Similar to Java code:
    -- boolean executes = new File("./dir/prog.exe").canExecute();
    executes <- newFile "./dir/prog.exe" >- canExecute
    io $ putStrLn "Executing an IO action inside of Java!"
    if executes
    then io $ putStrLn "File can execute!"
    else io $ putStrLn "File cannot execute!"

Working With Arrays
^^^^^^^^^^^^^^^^^^^

The utilities for working with arrays are defined in the ``Java.Array`` module
which is re-exported by the ``Java`` module. The API is shown below::

  -- The `c` type variable represents the type of the array.
  -- The `e` type variable represents the type of the element of the array.
  class (Class c) => JArray e c | c -> e where

    -- Create a new Java array.
    anew :: Int -> Java a c

    -- Get an element from a Java array.
    aget :: Int -> Java c e

    -- Set an element in a Java array.
    aset :: Int -> e -> Java c ()

  -- Get the length of an array
  alength :: JArray e c => Java c Int

  -- Convert a Java array to an Eta list
  arrayToList :: JArray e c => Java c [e]

  -- Convert a lazy Eta list to a Java array
  arrayFromList :: JArray e c => [e] -> Java a c

Note that the `e` type variable is determined from the `c` type variable and
vice-versa, so each array type is expected to have a unique element type and each
element type is expected to have a unique array type when using the API.

Primitive Arrays
""""""""""""""""

Primitive arrays have pre-defined instances in ``Java.Array``. The following table
lists the exported types and their element types.

+------------------------------+--------------------+---------------+
|    Java Type                 |  Array Type        |  Element Type |
+==============================+====================+===============+
|  ``boolean[]``               | ``JBooleanArray``  |   ``Bool``    |
+------------------------------+--------------------+---------------+
|  ``byte[]``                  | ``JByteArray``     |   ``Byte``    |
+------------------------------+--------------------+---------------+
|  ``short[]``                 | ``JShortArray``    |   ``Short``   |
+------------------------------+--------------------+---------------+
|  ``char[]``                  | ``JCharArray``     |   ``JChar``   |
+------------------------------+--------------------+---------------+
|  ``int[]``                   | ``JIntArray``      |   ``Int``     |
+------------------------------+--------------------+---------------+
|  ``long[]``                  | ``JLongArray``     |   ``Int64``   |
+------------------------------+--------------------+---------------+
|  ``float[]``                 | ``JFloatArray``    |   ``Float``   |
+------------------------------+--------------------+---------------+
|  ``double[]``                | ``JDoubleArray``   |   ``Double``  |
+------------------------------+--------------------+---------------+

Example
"""""""

.. code::

  {-# LANGUAGE ScopedTypeVariables #-}

  import Java

  main :: IO ()
  main = java $ do
    (arr :: JLongArray) <- arrayFromList [1..10]
    elems <- withObject arr $ mapM aget [0..9]
    io $ print elems
    withObject arr $ mapM_ (\i -> aset i (fromIntegral (i * 2))) [0..9]
    arrList <- arr <.> arrayToList
    io $ print arrList

Object Arrays
"""""""""""""

Object arrays must be explicitly declared as JWTs and must have an instance of the
`JArray` typeclass defined for them.

The ``Java.Array`` has one pre-defined object array: ``JStringArray`` which
corresponds to ``String[]`` in Java and has ``JString`` as the element type.

Example
"""""""

.. code::

    {-# LANGUAGE MagicHash, MultiParamTypeClasses, ScopedTypeVariables #-}

    import Java

    data JInteger = JInteger @java.lang.Integer
      = JInteger (Object# JInteger)
      deriving (Class, Show)

    data JIntegerArray = JIntegerArray @java.lang.Integer[]
      = JIntegerArray (Object# JIntegerArray)
      deriving Class

    foreign import java unsafe "@new" toJInteger :: Int -> JInteger
    foreign import java unsafe intValue :: JInteger -> Int

    -- There's a default instance for object arrays, so no need to define your own.
    instance JArray JInteger JIntegerArray

    main :: IO ()
    main = java $ do
      arr <- arrayFromList integers
      elems <- withObject arr $ mapM aget [0..9]
      io $ print elems
      withObject arr $ mapM_ (\i -> aset i (toJInteger (i * 2))) [0..9]
      arrList <- arr <.> arrayToList
      io $ print arrList
      where integers = map toJInteger [1..10]

Working With Subclasses
^^^^^^^^^^^^^^^^^^^^^^^

Motivation
""""""""""

Eta does not understand subclasses by default, so if you try to use a method
defined in a superclass on a subclass, it won't typecheck.

Using the imports from :ref:`java-imports-examples`,

.. code::

  foreign import java unsafe toString :: Object -> String

  data File = File @java.io.File
    deriving Class

  main :: IO ()
  main = do
    file <- java $ newFile "test.txt"
    -- This line will not typecheck since
    -- Object cannot match with File!
    putStrLn (toString file)

So how do we teach Eta about Java inheritance relationships to make Java code
handling smoother? By using the ``Extends`` typeclass from the standard library - it
is accessible after importing the ``Java`` module.

The Extends typeclass
"""""""""""""""""""""

.. code::

  class (Class a, Class b) => Extends a b where
    superCast :: a -> b
    unsafeCast :: b -> a
    ...

The ``Extends`` typeclass is a multi-parameter typeclass defined for JWTs where
``Extends a b`` means that JWT ``a`` is a subclass of JWT ``b``. The FFI has
built-in support for the ``Extends`` typeclass so you can freely add those
constraints into your imports.

For this typeclass, you don't define instances directly. Instead, you can
declaratively specify parent classes and interfaces using the ``Inherits`` type
family.

.. note::

  The standard library defines an alias for the ``Extends`` typeclass referred
  to as ``<:``. For example, ``Extends a Object`` can written as ``a <: Object``.
  We will be using this throughout the tutorial because it is more natural. You
  **must** enable the ``TypeOperators`` extension to use ``<:`` by placing
  ``{-# LANGUAGE TypeOperators #-}`` at the top of your file.


.. note::

    While you won't find many situations to use them, you can use the ``superCast``
    and ``unsafeCast`` functions when you need to cast between object types.

    ``superCast`` just does a conversion at the Eta-level and no explicit
    conversion is done at the Java-level, so this function is safe to use in all
    cases.

    ``unsafeCast`` does an explicit cast at the Java-level and can throw a
    ``ClassCastException`` if you make an invalid conversion, hence the name.

The Inherits type family
""""""""""""""""""""""""

.. code::

   type family Inherits (a :: *) :: [*]

The ``Inherits`` type family takes a JWT and returns a type-level list of JWTs.

**Example**

.. code::

   {-# LANGUAGE TypeFamilies, DataKinds #-}

   data Serializable
     = Serializable @java.io.Serializable
     deriving Class

   data File = File @java.io.File
     deriving Class

   type instance Inherits File = '[Object, Serializable]

Note that the ``TypeFamilies`` and the ``DataKinds`` extensions are required to
define the Java inheritance relationships and that the first element of the
type-level list **must be the parent class** and the remaining elements can be
the implemented interfaces in any order. Note that it is not necessary to
inform Eta about *all* the relationships, only those that you need for your
particular application.

Problem Resolution
""""""""""""""""""

The problematic code above can now be fixed::

  {-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

  foreign import java unsafe toString :: (a <: Object) => a -> String

  data File = File @java.io.File
    deriving Class

  type instance Inherits File = '[Object]

  main :: IO ()
  main = do
    file <- java $ newFile "test.txt"
    -- This line will now typecheck!
    putStrLn (toString file)

We can even change the code above to use the `Java` monad::

  {-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

  foreign import java unsafe toString :: (a <: Object) => Java a String

  data File = File @java.io.File
    deriving Class

  type instance Inherits File = '[Object]

  main :: IO ()
  main = do
    string <- java $ newFile "test.txt" >- toString
    putStrLn string

Note that you can specify an arbitrary number of ``Extends`` constraints based on
your use-case.

Working With Java Generics
^^^^^^^^^^^^^^^^^^^^^^^^^^

Now that we have access to Java inheritance relationships inside of Eta, we can
now work conveniently with Java Generics.

Importing Generic Classes
"""""""""""""""""""""""""

Importing generic classes is not much different than importing concrete classes -
just add some type parameter.

.. code::

  data List a = List (@java.util.List a)
    deriving Class

Importing Generic Methods
"""""""""""""""""""""""""

Importing generic methods is also not much different than importing concrete
methods - just add ``Extends`` constraints that specify the type bounds for each
of the generic parameters. If the parameter does not have a type bound, you should
specify ``Object``.

.. code::

  foreign import java unsafe "@interface add" add
    :: (a <: Object, b <: List a) => a -> Java b Bool

See the `java.util.List.add <https://docs.oracle.com/javase/7/docs/api/java/util/List.html#add(E)>`_
documentation.

Example
"""""""

A full example involving ``java.util.ArrayList`` can be executed in the
`Eta Playground <http://eta-lang.org/playground.html>`_.

Working With Java Enums
^^^^^^^^^^^^^^^^^^^^^^^

Many Java packages often contain Enums. Let's see how to handle them.
We'll be importing `ClientInfoStatus <https://docs.oracle.com/javase/7/docs/api/java/sql/ClientInfoStatus.html>`_ as an example.

.. code::

  data ClientInfoStatus = ClientInfoStatus @java.sql.ClientInfoStatus
    deriving Class

  type instance Inherits ClientInfoStatus = '[Enum ClientInfoStatus]

  foreign import java unsafe "@static @field java.sql.ClientInfoStatus.REASON_UNKNOWN"
    reasonUnknown :: ClientInfoStatus

  foreign import java unsafe "@static @field java.sql.ClientInfoStatus.REASON_UNKNOWN_PROPERTY"
    reasonUnknownProperty :: ClientInfoStatus

  foreign import java unsafe "@static @field java.sql.ClientInfoStatus.REASON_VALUE_INVALID"
    reasonValueInvalid :: ClientInfoStatus

  foreign import java unsafe "@static @field java.sql.ClientInfoStatus.REASON_VALUE_TRUNCATED"
    reasonValueTruncated :: ClientInfoStatus

Working with Variable Arguments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some methods in Java, like the method `java.util.Formatter.format <https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html#format(java.lang.String,%20java.lang.Object...)>`_
with signature ``Formatter (String format, Object.. args)``, take variable arguments. Variable arguments
are simply arrays, hence can be imported easily::

  data Formatter = Formatter @java.util.Formatter
    deriving Class

  -- Note that we didn't have to import `Object[]` because JObjectArray already exists
  -- in the standard library.
  foreign import java unsafe format :: String -> JObjectArray -> Java Formatter Formatter

Working with Java Converters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In Eta, there is a clear distinction JWTs and normal Eta types. Moreover, only JWTs can be used
in foreign imports/exports.

.. note::

   ``String`` is a notable exception to that rule because it's so commonly used that there's a
   special case that allows it an automatically converts it to ``JString``.

JWTs are inconvenient to use directly in Eta because they are just wrappers of native Java objects.
So, the following typeclass is defined in the standard library to help convert JWTs to common
Eta types like lists.

.. code::

    -- The `a` type variable should be a normal Eta type
    -- The `b` type variable should be a JWT or a primitive type (Byte, Short, Int, ...)
    class JavaConverter a b where
      toJava   :: a -> b
      fromJava :: b -> a

Many instances are provided for you by default so you can simply use `toJava` or `fromJava`
whenever you want to perform a conversion.

Example:

In this example, we want to work with the `java.io.File.listRoots <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#listRoots()>`_,
but it returns an array, which we can't work with that cleanly in Eta. So we convert it to an Eta
list with a helper function.

.. code::

   data Files = Files @java.io.File[]
     deriving Class

   -- Declare that `Files` is an array type with element type `File`.
   instance JArray File Files

   -- We import into the IO monad because it's more convenient for static methods.
   foreign import java unsafe "@static java.io.File.listRoots" listRoots' :: IO Files

   listRoots :: IO [File]
   listRoots = do
     filesArray <- listRoots'
     return $ fromJava filesArray

Example:

In this example, we want to work with the `java.nio.file.Paths.get <https://docs.oracle.com/javase/7/docs/api/java/nio/file/Paths.html#get(java.lang.String,%20java.lang.String...)>`_,
but it requires an array. To make it more convenient to use, we provide a helper that does the
conversion for us.

.. code::

   -- We import into the IO monad because it's more convenient for static methods.
   foreign import java unsafe "@static java.nio.file.Paths.get" mkPath'
     :: String -> JStringArray -> IO Path

   mkPath :: [String] -> IO Path
   mkPath pathFragments
     | length pathFragments == 0 = error "mkPath: Requires at least one path fragment"
     | (path:restPaths) <- pathFragments
     =  mkPath' path restArray
     where restJStrings = map toJava restPaths -- [JString]
           restArray    = toJava restJStrings  -- JStringArray

.. note::

  In some cases, you may need to specify a type annotation on the result of `fromJava` in order to
  specify what you what Eta type you want to convert to: ``(fromJava x :: DesiredResult)``.

.. _working-with-java-interfaces:

Working With Java Interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many Java interfaces often contain just a single method and such interfaces are
commonly used to pass functions and callbacks in Java. Many frameworks and
libraries use this type of interface frequently, so it useful to be able convert
Eta functions and implement these interfaces.

Suppose we try to make an implementation of `Runnable <https://docs.oracle.com/javase/7/docs/api/java/lang/Runnable.html>`_
in Eta::

  data Runnable = Runnable @java.lang.Runnable
    deriving Class

  foreign import java unsafe "@wrapper run"
    runnable :: Java Runnable () -> Runnable

  data Thread = Thread @java.lang.Thread
    deriving Class

  foreign import java unsafe "@new" newThread :: Runnable -> Java a Thread
  foreign import java unsafe start :: Java Thread ()

  main :: IO ()
  main = java $ newThread (runnable (io $ putStrLn "Run in another thread"))
             >- start

Note that this can be applied for abstract classes as well - just use a
``@wrapper @abstract`` annotation instead.

Example:

Let's try to wrap the `java.util.function.Function<T,R> <https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html>`_
interface in Java 8. The method we want to implement on the Eta side has signature ``R apply(T t)``.

The import would look like so:

.. code::

  data Function t r = Function (@java.util.function.Function t r)
    deriving Class

  foreign import java unsafe "@wrapper apply"
    mkFunction :: (t <: Object, r <: Object) => (t -> Java (Function t r) r) -> Function t r

Working With Covariance and Contravariance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In Java, covariance is expressed with ``? extends X`` and contravariance is expressed with
``? super Y``. The `andThen <https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html>`_
method has signature ``<V> Function<T,V> andThen(Function<? super R,? extends V> after)``.
It exhibits both covariance and contravariance so we will import it as an example

.. code::

  foreign import java unsafe "@interface andThen" andThen ::
    (t <: Object, r <: Object, v <: Object, r <: a, b <: v)
    => Function a b -> Java (Function t r) (Function t v)

For each ``?`` we should generate a fresh variable. In the case above we use ``a`` and ``b``.

Exporting Eta Methods
^^^^^^^^^^^^^^^^^^^^^^

Just as you can import Java methods into Eta, you can also export Eta functions
into Java.

General Syntax
""""""""""""""

.. code-block:: console

   foreign export java "[export-string]" [eta-identifier]
     :: [arg-type-1] -> [arg-type-2] -> .. -> Java [export-jwt] [return-type]

#. ``[export-string]`` should consist of ``@static`` followed by a fully qualified
   Java class name with the name of the static method appended to it with a dot. This
   is the method name that the exported function should be referred to in the Java
   world. (e.g. ``"@static com.org.SomeClass.someMethodName"``)


#. ``[eta-identifier]`` should be a valid Eta identifier for an *existing*
   Eta function that is the target of the export.

#. ``[arg-type-n]`` should be a marshallable Eta type.

#. ``[export-jwt]`` should be a JWT that refers to the class name of the exported
   class.

#. ``[return-type]`` should be a marshallable Eta type which is the result of the
   Eta function.

Example
"""""""

Here is an example::

  fib 0 = 1
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2)

  foreign export java "@static eta.example.MyExportedClass.fib"
    fib :: Int -> Int

This creates a class called ``eta.example.MyExportedClass`` with a default
constructor and static method ``int fib(int)``.

Setting Up The Project
""""""""""""""""""""""

Setup a project, just like :ref:`setting-up-first-project` with the following
changes:

#. **Main.hs**

   .. code::

      fib 0 = 1
      fib 1 = 1
      fib n = fib (n - 1) + fib (n - 2)

      foreign export java "@static eta.example.MyExportedClass.fib"
        fib :: Int -> Int

      main :: IO ()
      main = return ()

#. Run the following commands:

   .. code-block:: console

      etlas clean
      etlas configure --enable-uberjar-mode

   This will enable uberjar mode so that a standalone JAR will be built.

#. Execute ``etlas build`` which will generate the final JAR in
   ``dist/build/eta-first/eta-first.jar``. You can then proceed to take this
   JAR file to wherever necessary to import it to your JVM-based projects.

Importing from Java
"""""""""""""""""""

Assuming the JAR file for the compiled code above is in the classpath, you can
import from Java like this:

.. code-block:: java

   package eta.run;

   import eta.example.MyExportedClass;

   public class Main {
     public static void main(String[] args) {
       System.out.println("fib(1000): " + MyExportedClass.fib(1000));
     }
   }

Importing from Scala
""""""""""""""""""""

Assuming the JAR file for the compiled code above is in the classpath, you can
import from Scala like this:

.. code-block:: scala

    package eta.run

    import eta.example.MyExportedClass

    object EtaExports {
      def main(args: Array[String]) {
        val fib = MyExportedClass.fib(1000)
        println(s"fib(1000): $fib")
      }
    }

Importing from Clojure
""""""""""""""""""""""

Assuming the JAR file for the compiled code above is in the classpath, you can
import from Clojure like this:

.. code-block:: clojure

    (ns eta.run
        (:import [eta.example MyExportedClass]))

    (defn -main []
      (println (str "fib(1000): " (MyExportedClass/fib 1000))))

Add Java Files to Your Project
------------------------------

You can include Java-related files like ``.java``, ``.class``, ``.jar`` files to be
included in your project, by adding their paths to the ``java-sources:`` field in
the Cabal file.

Example
^^^^^^^

In this example, we'll take a look at including Java source files in our project
and importing the defined methods into Eta for use.

Setup a project, just like :ref:`setting-up-first-project` with the following
changes:

#. **Main.hs**

   .. code::

      import Java

      foreign import java unsafe "@static eta.first.Utils.createFile"
        createFile :: String -> IO ()

      -- Creates an empty file
      main :: IO ()
      main = createFile "HelloWorld.txt"

#. Create a new folder called ``java`` and a file ``Utils.java`` with the following
   contents:

   .. code-block:: java

      package eta.first;

      import java.nio.file.Files;
      import java.nio.file.Paths;

      public class Utils {

        /* This helper method lets us avoid variadic arguments which
           are a bit cumbersome to work with in Eta. */

        public static void createFile(String path) {
          Files.createFile(Paths.get(path));
        }
      }

   Your directory structure should look like this:

   .. code-block:: console

      eta-first/
      |--src/
      |----Main.hs
      |--java/
      |----Utils.java
      |--eta-first.cabal
      |--Setup.hs

   Your directory structure may vary based on the options you chose in the
   ``etlas init`` step.

#. Update ``eta-first.cabal``, adding a ``java-sources:`` field:

   .. code-block:: console

      java-sources: java/Utils.java

   .. note::

      You can add more Java-based files indented under the first entry with either
      relative or absolute paths. You can thus include arbitrary ``.jar`` files or
      even individual ``.class`` files that you need.

#. That's it! Run the example with ``etlas run``.

Adding Maven Dependencies to Your Project
-----------------------------------------

You can include Maven dependencies in the format of
``[groupId]:[artifactId]:[version]`` to include in your project in the
``maven-depends:`` field of the Cabal file under the ``library`` or
``executable`` section. Each successive entry should be placed on a
separate line and be separated with a comma.

You can add specific Maven repositories to resolve your dependencies against
with the ``maven-repos:`` field. You should specify each repository URL on its
own line (indented) and without commas (unlike the ``maven-depends:`` field). This
field also supports the following special keywords as shortcuts for some common
Maven repositories:

+------------------------------+------------------------------------------------------+
|    Keyword                   |  Maven Repository URL                                |
+==============================+======================================================+
|   central                    | https://repo1.maven.org/maven2/                      |
+------------------------------+------------------------------------------------------+
|   javaNet1                   | http://download.java.net/maven/1/                    |
+------------------------------+------------------------------------------------------+
|   sonatype:[type]*           | https://oss.sonatype.org/content/repositories/[type] |
+------------------------------+------------------------------------------------------+
|   jcenter                    | https://jcenter.bintray.com/                         |
+------------------------------+------------------------------------------------------+
|   bintray:[owner]:[repo]**   | https://dl.bintray.com/[owner]/[repo]/               |
+------------------------------+------------------------------------------------------+

\* ``[type]`` should be one of ``public``, ``snapshots``, or ``releases``.
\*\* ``[owner]`` should be the owner of the  repository and ``[repo]`` should be the name of the Bintray repository.

By default, ``etlas`` tries to resolve your dependencies against
``central``. So, you don't need to specify a repository unless
required.

.. note::

   If you need to access a Maven repository that requires credentials, you can
   specify the URL in the form ``http://[user]:[pass]@[repo-url]``.

Example
^^^^^^^

In this example, we'll be binding to the `Unirest <http://unirest.io/java>`_ library.

Setup a project, just like :ref:`setting-up-first-project` with the following
changes:

#. Update ``eta-first.cabal``, adding a ``maven-depends:`` field:

   .. code-block:: console

      maven-depends: com.mashape.unirest:unirest-java:1.4.9

#. **Main.hs**

   .. code::

      {-# LANGUAGE MagicHash, FlexibleContexts, DataKinds, TypeFamilies #-}

      import Java

      -- Imports from the Unirest API
      data BaseRequest = BR @com.mashape.unirest.request.BaseRequest
        deriving Class

      data GetRequest = GR @com.mashape.unirest.request.GetRequest
        deriving Class

      type instance Inherits GetRequest = '[BaseRequest]

      data HttpResponse a = HResp (com.mashape.unirest.http.HttpResponse a)
        deriving Class

      foreign import java unsafe "@static com.mashape.unirest.http.Unirest.shutdown"
        shutdownUnirest :: IO ()

      foreign import java unsafe "@static com.mashape.unirest.http.Unirest.get"
        get :: String -> Java a GetRequest

      foreign import java unsafe asString
        :: (a <: BaseRequest) => Java a (HttpResponse JString)

      foreign import java unsafe getBody
        :: (a <: Object) => Java (HttpResponse a) a

      -- Run a simple blocking GET request
      main :: IO ()
      main = do
        response <- java $ do
              get "https://jsonplaceholder.typicode.com/posts/1"
            >- asString
            >- getBody
        putStrLn $ read (show response)
        shutdownUnirest

#. That's it! Run the example with ``etlas run``.

More examples
-------------

Let us look at some more examples of exporting Eta methods to Java.

Example 1
^^^^^^^^^

In this example we are going to export the `Pipes <https://hackage.haskell.org/package/pipes>`_ library. Pipes
is a powerful stream processing library in Haskell. Let us go ahead and start with creating an ``etlas`` project.

.. code-block:: console

      mkdir pipes-test
      cd pipes-test
      etlas init

The project structure should look like this sfter you select the basic options:

.. code-block:: console

      pipes-test/
      |--src/
      |----Main.hs
      |--ChangeLog.md
      |--LICENSE
      |--pipes-test.cabal
      |--Setup.hs

Now modify the ``pipes-test.cabal`` file to include ``pipes`` in the ``build-depends:`` section. Followed by that run
``etlas build``. Now let us modify the ``Main.hs`` file to use ``Pipes`` to take an input from a stream and and output
each input to the output stream. We will try to restrict the program to take a maximum of 3 inputs from the input stream:

.. code::

   {-# LANGUAGE MagicHash,ScopedTypeVariables #-}
   module Main where
   import Java
   import Pipes
   import Control.Monad (replicateM_)
   import qualified Pipes.Prelude as P
   import System.IO

   take ::  Int -> Pipe a a IO ()
   take n = do
    replicateM_ n $ do
        x <- await
        yield x
    lift $ putStrLn "You shall not pass!"

   maxInput :: Int -> Producer String IO ()
   maxInput n = P.stdinLn >-> Main.take n

   main :: IO ()
   main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    runEffect $ maxInput 3 >-> P.stdoutLn


   foreign export java "@static com.typelead.Util.test" main :: IO ()

Here we have delegated the input and output to Eta side and are exporting the entire main method to Java. The line `hSetBuffering stdin LineBuffering` is optional. It establishes the whole point of ``Pipes`` by not holding the input in memory.
Now let us build the uber jar for this:

.. code-block:: console

      etlas clean
      etlas configure --enable-uberjar-mode

This should create the uber jar buried somewher inside the ``dist`` folder. Now let us test this jar by actually
creating a Java file and using an exported method. We can create the Java source file in the root of the project
itself. We shall call it ``Test.java``

.. code-block:: java

      package eta.first;

      import com.typelead.Util;

      public class Test {
      public static void main (String args []){
        Util x = new Util();
        x.test();
        }
      }

.. note::

   The package name is intentionally left blank. If you go ahead and use a package name like
   ``com.typelead.test``, you have to create a directory structure like the same, as it is a prevalent practise in Java.

Now let us try to run this. Assuming that you are still at the root of the project run this:

.. code-block:: console

      javac -cp ":/<your absolute file path here>/pipes-test/dist/build/pipes-test/pipes-test.jar" Test.java
      java -cp ".:/<your absolute file path here>/pipes-test/dist/build/pipes-test/pipes-test.jar" Test

You will get the console waiting for input. The moment you type something ad press Enter, it consumes that
and emits that into the output stream, one at a time. After the third input it will say "You shall not pass!"
and terminates the program. So through this example we were not only able to delegate the entire library from
Eta, but also the input and output parts too.


Contact Us
----------

If you had trouble with this tutorial, you can give us feedback by:

- filing an `issue <https://github.com/typelead/eta/issues/new>`_
- discussing with us on `Gitter <https://gitter.im/typelead/eta>`_
