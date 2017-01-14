Getting Started with Eta
========================

In the following sections, we'll cover how to get Eta installed on your system and
how to work with EPM projects. If at any point you get stuck with any of the steps
below, feel free to join us on `Gitter <https://gitter.im/typelead/eta>`_ so we can
help you troubleshoot.

Installating Eta
----------------

Currently, the there are only two ways of installing Eta:

1. Source
2. Docker

Work on setting up platform-specific installers is in progress.

.. warning::

  The Windows installation currently does not work. Work is in progress to fix up
  the installation, see `this issue <https://github.com/typelead/eta/issues/106>`_.

Method 1: Source Installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
**Estimated Time:** 30 minutes

Prerequisites
"""""""""""""

Make sure you have the following tools installed on your system:

- `Stack <https://docs.haskellstack.org/en/stable/README>`_
- `JDK 1.7 <http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html>`_ or `JDK 1.8 <http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html>`_

  - make sure ``javac`` and ``java`` are on the PATH

Installation
""""""""""""

Clone the repository and run the install script at the root of the repository:

  .. code-block:: console

    git clone --recursive https://github.com/typelead/eta
    cd eta
    ./install.sh

.. note::

  If you omit the `--recursive` flag to `git clone`, you will need to
  initialize the project's submodules before running `install.sh`:

  .. code-block:: console

    git submodule update --init --recursive

Once the installation is done, you will now have access to the following command-line tools:

- ``eta`` - The main compiler
- ``epm`` - The package manager and build tool

Check to ensure that they are on the ``PATH`` with the following commands:

.. code-block:: console

   eta --version
   epm --version

If you obtain an error that either tool is missing, run the following command:

.. code-block:: console

   stack path --local-bin

Add the path that you obtain in the output to your ``PATH`` environment variable.

Method 2: Docker
^^^^^^^^^^^^^^^^

Prerequisites
"""""""""""""

Make sure you have the following tools installed on your system:

- `Docker <https://docs.docker.com/engine/installation>`_

Installation
""""""""""""

To obtain an environment with `eta` and `epm`, run the following command:

.. code-block:: console

  docker run -it psibi/eta

Updating Eta
------------

Eta updates pretty fast and we're incorporating new patches on a daily basis that
you might want to get access to.

If you have Eta already installed, go to the root of this repository's clone on
your system, and run the following command:

.. code-block:: console

   ./update.sh

This will do a fresh installation, recompiling all the core libraries with the most
recent version of the compiler.

If you have existing EPM projects, make sure you run

.. code-block:: console

  epm clean
  epm install --dependencies-only

inside each project before proceeding with your normal development so that EPM
recognizes the updated libraries.

Running Your First Program
--------------------------

#. Create a new file called *Main.hs* and with the following contents::

    module Main where

    primes = filterPrime [2..]
      where filterPrime (p:xs) =
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

#. Run the following command on the command line to compile the program:

   .. code-block:: console

     eta Main.hs

   This will compile the program to a standalone JAR with the ``Run``- prefix.

#. Run the program with java:

   .. code-block:: console

     java -jar RunMain.jar

.. _setting-up-first-project:

Setting up your first EPM Project
---------------------------------

With EPM, you don't have to worry about remembering all the particular flags to
sent to ``eta``. You can simply specify what you want in a human-readable format
called Cabal. To learn more about the specification file format which is also used
in the Haskell ecosystem, read
`this <https://www.haskell.org/cabal/users-guide/developing-packages.html>`_ guide.

#. Create a new directory called ``eta-first`` and enter it.

   .. code-block:: console

      mkdir eta-first
      cd eta-first

#. Initialize the project with EPM.

   .. code-block:: console

      epm init

   This is an interactive command that will ask you questions and help you generate
   a Cabal project file for your project.

   - *Package name* - Press enter to select the default.
   - *Package version* - Press enter to select the default.
   - *License* - Press enter to select the default.
   - *Author name* - Press enter to select the default.
   - *Maintainer email* - Press enter to select the default.
   - *Project homepage URL* - Press enter to select the default.
   - *Project synopsis* - Press enter to select the default.
   - *Project category* - Press enter to select the default.
   - *Package build* - Press 2 to select **Executable**.
   - *Main module* - Press enter to select the default.
   - *Base language* - Press enter to select the default.
   - *Inline documentation* - Press enter to select the default.
   - *Source directory* - Press 2 to select **src**.

   This should generate two files: ``Setup.hs`` and ``eta-first.cabal``.

   The ``Setup.hs`` file can be ignored in most cases. It can be used to add EPM hooks
   to support your development workflow if required.

   The ``eta-first.cabal`` file is used to specify your project configuration. EPM
   will take care of calling the necessary tools to complete your build.


#. Add the files ``Main.hs`` and ``Primes.hs`` in ``src/`` as shown below.

   Main.hs

   .. code::

     module Main where

     import Primes

     main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

   Primes.hs

   .. code::

      module Primes where

      primes = filterPrime [2..]
        where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

   The project structure should look like this:

   .. code-block:: console

      eta-first/
      |--src/
      |----Main.hs
      |----Primes.hs
      |--eta-first.cabal
      |--Setup.hs

   Your directory structure may vary based on the options you chose in the
   ``epm init`` step.

#. Update ``eta-first.cabal``, adding an ``other-modules:`` field:

   .. code-block:: console

      other-modules: Primes

   Any additional modules you add to the project should be added at the same
   indentation level as the ``Primes`` entry, but below it.

#. To build & run, execute this command:

   .. code-block:: console

      epm run

   .. note:: 

      Note that this will create a JAR file *without* dependencies. This is
      best suited for development. If you would like to generate an uberjar, make
      sure you run the following two commands:

      .. code-block:: console

          epm clean
          epm configure --disable-executable-dynamic

      These commands need only be run once to set the local Cabal config. All
      future builds will generate uberjars. Beware that this can be very slow.
      Work is being done to
      `improve uberjar performance <https://github.com/typelead/eta/issues/20>`_.

Learning Eta
------------
Now that you're set up with Eta, the next step is to learn about how to write Eta
programs.

For tutorials & examples, see the following:

- `Eta 2048 Game Implementation <https://github.com/rahulmutt/eta-2048>`_
- `JDBC Example <https://github.com/tatut/eta-jdbc-example/blob/master/src/Main.hs>`_
- `Repository of Eta Examples <https://github.com/typelead/eta-examples>`_

For a list of the currently supported packages, see:

- `Eta Hackage <https://github.com/typelead/eta-hackage>`_

If you are comfortable with Haskell, you can skip over to the next section and
you'll just need to learn how to interact with Java.

If you are new to Haskell and pure functional programming in general, we suggest
the following resources to get your started with the basics:

- `Learn You a Haskell in a Nutshell <https://gist.github.com/mikehaertl/3258427>`_
- `Functional Programming By Example <http://caiorss.github.io/Functional-Programming>`_
- `Learn You a Haskell <http://learnyouahaskell.com>`_
- `Real World Haskell <http://book.realworldhaskell.org/read/>`_

Work is in progress to make a free ebook for Eta catered for Java programmers.

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
library like so::

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
       this.counter = initiail;
     }

     public void increment() {
       this.counter = Math.min(this.counter + 1, COUNTER_MAX);
       this.publicCounter = counter;
     }

     public int get() {
       return counter;
     }

     public int set(int value) {
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

   data {-# CLASS "eta.example.Counter" #-} Counter = Counter (Object# Counter)

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

   data {-# CLASS "[class-name]" #-} X = X (Object# X)
     deriving Class

- ``[class-name]`` should be the fully qualified Java class name and ``X`` should
  be the Eta name you would use to refer to the corresponding Java class in foreign
  imports. Note that ``[class-name]`` can also be converted to an array type by
  appending ``[]``.
- The ``Class`` typeclass is a built-in typeclass that is a marker for a JWT.
  **Make sure all your JWTs derive a Class instance.**

**Example**

.. code::

   data {-# CLASS "java.lang.Integer" #-} JInteger = JInteger (Object# JInteger)
   data {-# CLASS "java.lang.Integer[]" #-} JIntegers = JIntegers (Object# JIntegers)

In this example, we're declaring JWTs for the ``java.lang.Integer`` class and the
``java.lang.Integer[]`` array (which is technically a class on its own).

.. note::

   You may find the declaration syntax a bit cumbersome or even confusing. There is
   work underway to make it
   `a lot more pleasant <https://github.com/typelead/eta/issues/140>`_. If you have
   a preference of syntax, please let us know!

Deriving Standard Typeclass Instances
"""""""""""""""""""""""""""""""""""""

**Syntax** 

.. code::

   data {-# CLASS "[class-name]" #-} X = X (Object# X)
     deriving (Class, Eq, Show)

Currently, deriving the `Class`, `Eq`, and `Show` instances for JWTs is supported.
You should derive these instances based on the need of the application. The `Eq`
instance will use the underlying `Object.equals()` method and the `Show` instance
will use `Object.toString()`. To find out more about the `Class` typeclass, see
here.

.. _marshalling-java-eta:

Marshaling between Java and Eta types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Java Classes & Arrays
"""""""""""""""""""""

A Java Wrapper Type, as mentioned in the section :ref:`java-wrapper-type`, will
marshal to an object of the class given in the ``CLASS`` annotation.

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

When writing FFI declarations that return objects, you be wrap the result type
in a ``Maybe`` if the documentation of the corresponding Java method clearly
states that ``null`` is a potential return type. It is always safe to wrap the
result in a ``Maybe`` type, but the user will have to bear the burden of dealing
with an unnecessary case if the result is always a non-null object.

If the ``Maybe`` type is not used for a method that actually does return null, then
a ``NullPointerException`` will occur when a method is invoked on that object.

The Java Monad
^^^^^^^^^^^^^^

As mentioned before, the ``Java`` monad is used to contain the implicit ``this``
context. It can be effectively thought as a state monad with a given Java object
as the state.

.. code::

   newtype Java c a = Java {- Internal definition -}

As can be seen from the above definition, the `Java` monad has *two* type
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
   case it is considerd as ``safe``.

   - ``unsafe`` is the option you would typically select. In this case, the java
     method identified in the ``[import-string]`` will be run directly. This can
     be dangerous if the function can block in which case it will block the Eta
     RTS from switching the current green thread.

   - ``safe`` is the option you would select for functions that you would expect to
     block for some time, so they will be safely run in another thread to prevent
     the call from blocking the Eta's green threads. This option must also be
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

   - ``Java [jwt] [return-type]``: This is the form that is used typically.
     ``[jwt]`` should be the JWT for the class which the declaration pertains. If
     the declaration is has a ``@static`` annotation, this can be left free with
     type variable instead of a concrete type.
     ``[return-type]`` should be a marshallable Eta type.

   - ``IO [return-type]``: This form should be used sparingly and is only present
     as a convenience in the cases where the only usage of the . Note
     that if the declaration does not have a ``@static`` annotation, you must
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

  data {-# CLASS "java.io.File" #-} File = File (Object# File)

  foreign import java unsafe canExecute :: Java File Bool
  foreign import java unsafe "canExecute" canExecute1 :: Java File Bool
  foreign import java unsafe "canExecute" canExecute2 :: File -> IO Bool
  foreign import java unsafe "canExecute" canExecute3 :: File -> Bool

**Importing Static Methods**

Let's import the ``File createTempFile(String, String)`` `static method <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)>`_ from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the import::

  data {-# CLASS "java.io.File" #-} File = File (Object# File)

  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile  :: String -> String -> Java a File
  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile1 :: String -> String -> IO File
  foreign import java unsafe "@static java.io.File.createTempFile"
    createTempFile2 :: String -> String -> File

**Importing Constructors**

Let's import the ``File(String)`` `constructor <https://docs.oracle.com/javase/7/docs/api/java/io/File.html#File(java.lang.String)>`_ from the
`java.io.File <https://docs.oracle.com/javase/7/docs/api/java/io/File.html>`__
class.

The following are all equivalent ways of performing the import::

  data {-# CLASS "java.io.File" #-} File = File (Object# File)

  foreign import java unsafe "@new" newFile  :: String -> Java a File
  foreign import java unsafe "@new" newFile1 :: String -> IO File
  foreign import java unsafe "@new" newFile2 :: String -> File

**Importing Instance Fields**

Let's import the ``private String path`` `instance field <http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java/#165>`_
from the `java.io.File <http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java>`_ class. Note that the imports shown below
are purely for illustration purposes and will throw an exception if called because
``path`` is a private field.

The following are all equivalent ways of performing the get/set imports::

  data {-# CLASS "java.io.File" #-} File = File (Object# File)

  -- Imports for getting the field
  foreign import java unsafe "@field path" getFilePath  :: Java File String
  foreign import java unsafe "@field path" getFilePath1 :: File -> IO String
  foreign import java unsafe "@field path" getFilePath2 :: File -> String

  -- Imports for setting the field. 
  foreign import java unsafe "@field path" setFilePath  :: String -> Java File ()
  foreign import java unsafe "@field path" setFilePath1 :: File -> String -> IO ()

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

Working With Subclasses
^^^^^^^^^^^^^^^^^^^^^^^

Motivation
""""""""""

Eta does not understand subclasses by default, so if you try to use a method
defined in a superclass on a subclass, it won't typecheck.

Using the imports from :ref:`java-imports-examples`, 

.. code::

  foreign import java unsafe toString :: Object -> String

  data {-# CLASS "java.io.File" #-} File = File (Object# File)

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
    ...

The ``Extends`` typeclass is a multi-parameter typeclass defined for JWTs where
``Extends a b`` means that JWT ``a`` is a subclass of JWT ``b``. The FFI has
built-in support for the ``Extends`` typeclass so you can freely add those
constraints into your imports. But for this typeclass, you don't define instances
directly. Instead, you can declaratively specify parent classes and interfaces
using the ``Inherits`` type family.

The Inherits type family
""""""""""""""""""""""""

.. code::

   type family Inherits (a :: *) :: [*]

The ``Inherits`` type family takes a JWT and returns type-level list of JWTs.

**Example**

.. code::

   {-# LANGUAGE TypeFamilies, DataKinds #-}

   data {-# CLASS "java.io.Serializable" #-} Serializable
     = Serializable (Object# Serializable)
     deriving Class

   data {-# CLASS "java.io.File" #-} File = File (Object# File)
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

  foreign import java unsafe toString :: Extends a Object => a -> String

  data {-# CLASS "java.io.File" #-} File = File (Object# File)
    deriving Class

  type instance Inherits File = '[Object, Serializable]

  main :: IO ()
  main = do
    file <- java $ newFile "test.txt"
    -- This line will now typecheck!
    putStrLn (toString file)

We can even change the code above to use the `Java` monad::

  {-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

  foreign import java unsafe toString :: Extends a Object => Java a String

  data {-# CLASS "java.io.File" #-} File = File (Object# File)
    deriving Class

  type instance Inherits File = '[Object, Serializable]

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

  data {-# CLASS "java.util.List" #-} List a = List (Object# (List a))
    deriving Class

Importing Generic Methods
"""""""""""""""""""""""""

Importing generic methods is also not much different than importing concrete
methods - just add ``Extends`` constraints that specify the type bounds for each
of the generic parameters. If the parameter does not have a type bound, you should
specify ``Object``.

.. code::

  foreign import java unsafe "@interface add" add
    :: (Extends a Object, Extends b (List a)) => a -> Java b Bool

See the `java.util.List.add <https://docs.oracle.com/javase/7/docs/api/java/util/List.html#add(E)>`_
documentation.

Example
"""""""

A full example involving ``java.util.ArrayList`` can be executed in the
`Eta Playground <http://eta-lang.org/playground.html>`_.

.. _working-with-java-interfaces:

Working With Java Interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many Java interfaces often contain just a single method and such interfaces are
commonly used to pass functions and callbacks in Java. Many frameworks and
libraries use this type of interface frequently, so it useful to be able convert
Eta functions and implement these interfaces.

Suppose we try to make an implementation of `Runnable <https://docs.oracle.com/javase/7/docs/api/java/lang/Runnable.html>`_
in Eta::

  data {-# CLASS "java.lang.Runnable" #-} Runnable = Runnable (Object# Runnable)

  foreign import java unsafe "@wrapper run"
    runnable :: Java Runnable () -> Runnable

  data {-# CLASS "java.lang.Thread" #-} Thread = Thread (Object# Thread)

  foreign import java unsafe "@new" newThread :: Runnable -> Java a Thread
  foreign import java unsafe start :: Java Thread ()

  main :: IO ()
  main = java $ newThread (runnable (io $ putStrLn "Run in another thread"))
             >- start

Note that this can be applied for abstract classes as well - just use a
``@wrapper @abstract`` annotation instead.

Exporting Eta Methods
^^^^^^^^^^^^^^^^^^^^^^

Just as you can import Java methods into Eta, you can also export Eta fuctions
into Java.

General Syntax
""""""""""""""

.. code-block:: console

   foreign export java "[export-string]" [eta-identifier]
     :: [arg-type-1] -> [arg-type-2] -> .. -> Java [export-jwt] [return-type]

#. ``[export-string]`` should be an unqualified Java instance method name that
   is the exported function should be referred to in the Java world.

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

  data {-# CLASS "eta.example.MyExportedClass" #-} MyExportedClass
    = MyExportedClass (Object# MyExportedClass)

  fib' 0 = 1
  fib' 1 = 1
  fib' n = fib' (n - 1) + fib' (n - 2)

  fib :: Int -> Java MyExportedClass Int
  fib n = return $ fib' n

  foreign export fib :: Java MyExportedClass Int

This creates a class called ``eta.example.MyExportedClass`` with a default
constructor and single instance method ``fib``.

Setting Up The Project
""""""""""""""""""""""

Setup a project, just like :ref:`setting-up-first-project` with the following
changes:

#. **Main.hs**

   .. code::

      {-# LANGUAGE MagicHash #-}

      import Java

      data {-# CLASS "eta.example.MyExportedClass" #-} MyExportedClass
        = MyExportedClass (Object# MyExportedClass)

      fib' 0 = 1
      fib' 1 = 1
      fib' n = fib' (n - 1) + fib' (n - 2)

      fib :: Int -> Java MyExportedClass Int
      fib n = return $ fib' n

      foreign export fib :: Java MyExportedClass Int

      main :: IO ()
      main = return ()

#. Run the following commands:

   .. code-block:: console

      epm clean
      epm configure --disable-executable-dynamic

   This will enable uberjar mode so that a standalone JAR will be built.

#. Execute ``epm build`` which will generate the final JAR in
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
       MyExportedClass mec = new MyExportedClass();
       System.out.println("fib(1000): " + mec.fib(1000));
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
        val mec = new MyExportedClass
        val fib = mec.fib(1000)
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
      (let [mec (MyExportedClass.)]
        (println (str "fib(1000): " (.fib mec 1000)))))

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
   ``epm init`` step.

#. Update ``eta-first.cabal``, adding a ``java-sources:`` field:

   .. code-block:: console

      java-sources: java/Utils.java

   .. note::

      You can add more Java-based files indented under the first entry with either
      relative or absolute paths. You can thus include arbitrary ``.jar`` files or
      even individual ``.class`` files that you need.

#. That's it! Run the example with ``epm run``.

Contact Us
----------

If you had trouble with this tutorial, you can give us feedback by:

- filing an `issue <https://github.com/typelead/eta/issues/new>`_
- discussing with us on `Gitter <https://gitter.im/typelead/eta>`_
