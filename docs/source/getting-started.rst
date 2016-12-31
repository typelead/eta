Getting Started with Eta
========================

In this tutorial, you'll compile a simple program with Eta and run it, illustrating the basic workflows.

In Eta, there are primarily two workflows:

#. Trying out simple programs without any dependencies. (EPM not needed)

#. Working on a multi-file, multi-dependency project. (EPM needed)

Without EPM
------------

#. Create a new file called *Main.hs* and put the following as its contents::

    module Main where

    primes = filterPrime [2..]
      where filterPrime (p:xs) =
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

#. Run the following command on the command line to compile the program::

    eta -o Out.jar Main.hs

   This will compile the program to a standalone JAR.

#. Run the program with java::

    java -classpath Out.jar eta.main

   The ``eta.main`` class contains the entry point that initializes the Eta runtime
   system and runs the compiled program.

With EPM
---------

With EPM, things become *much* smoother.

#. Create a new directory called ``eta-first`` and enter it.

   .. code::

      mkdir eta-first
      cd eta-first

#. Initialize the project with EPM.

   .. code:: bash

      epm init

   This is an interactive command that will ask you questions and help you generate
   a cabal project file for your project. Select **Executable** (option 2) for project
   type and ``src`` for the source directory. The project structure should look
   like this::

      eta-first/
      |--src/
      |--eta-first.cabal
      |

   Your directory structure may vary based on the options you chose, such as the
   license type.

#. Add the files ``Main.hs`` and ``Primes.hs`` in ``src/`` as shown below.

   ``Main.hs``
   .. code::

    module Main where

    import Primes

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

   ``Primes.hs``
   .. code::

      module Primes where

      primes = filterPrime [2..]
        where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

#. Update ``eta-first.cabal``, adding an ``other-modules:`` field::

      other-modules: Primes```

#. To build & run, execute this command::

    epm run

#. That build may have been slow. In order to make the build faster, configure the
   project to make a dynamic executable::

    epm configure --enable-executable-dynamic
    epm run

   Note that you don't have to run ``configure`` again from then on unless you want to
   revert it back to uberjar-mode with ``--disable-executable-dynamic``.

Contact Us
----------

If you had trouble with this tutorial, you can give us feedback by:

- filing an `issue <https://github.com/typelead/eta/issues/new>`_
- discussing with us on `Gitter <https://gitter.im/typelead/eta>`_
