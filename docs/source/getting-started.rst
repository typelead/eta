Getting Started with Eta
========================

In the following sections, we'll cover how to get Eta installed on your system and
how to work with Etlas projects. If at any point you get stuck with any of the steps
below, feel free to join us on `Gitter <https://gitter.im/typelead/eta>`_ so we can
help you troubleshoot.

Installing Eta
--------------

Currently, there are a few ways of installing Eta:

#. Binary Installation
#. Source Installation
#. Docker Image
#. Nix Environment

Method 1: Binary Installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Eta Version: 0.7.0b2

Etlas Version: 1.1.0.0

The installation process works as follows:

1. You will download the ``etlas`` executable.

2. As soon as you start using ``etlas``, ``eta`` and the standard libraries will automatically be downloaded automatically for you on demand.

3. You can run ``etlas update`` followed by ``etlas select latest`` to ensure you have the latest version of Eta.

4. You can access the ``eta`` command directly via ``etlas exec eta -- [args]``.

You can install Etlas by following the steps below:

1. Make sure `JDK 7 or above <http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html>`_ are installed on your system and ``java`` is on the ``$PATH``.

2. Download the Etlas binary for your platform.

   - `Linux (64-bit) <http://88a2a1b21f8e03a6bc8d-8f2e61d843ea88e4f30ab3f81ca0e396.r42.cf5.rackcdn.com/etlas-1.1.0.0/binaries/x86_64-linux/etlas>`_
   - `Mac (64-bit) <http://88a2a1b21f8e03a6bc8d-8f2e61d843ea88e4f30ab3f81ca0e396.r42.cf5.rackcdn.com/etlas-1.1.0.0/binaries/x86_64-osx/etlas>`_
   - `Windows (64-bit) <http://88a2a1b21f8e03a6bc8d-8f2e61d843ea88e4f30ab3f81ca0e396.r42.cf5.rackcdn.com/etlas-1.1.0.0/binaries/x86_64-windows/etlas.exe>`_

3. Place the binary in in your ``$PATH``.

4. Give the program executable permissions (on Unix-based systems).

.. code-block:: console

   $ chmod +x etlas

5. Start using it and it will download everything you need on demand. Head over to :ref:`setting-up-first-project`.

Method 2: Source Installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Prerequisites
"""""""""""""

Make sure you have the listed tools/libraries installed on your system. Check the
OS-specific sections for additional requirements.

General
"""""""

- `Stack <https://docs.haskellstack.org/en/stable/README>`_
  - Make sure the path that is obtained from running ``stack path --local-bin`` is present on the ``PATH``.
- `JDK 1.7 <http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html>`_ or `JDK 1.8 <http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html>`_

  - Make sure ``javac`` and ``java`` are on the PATH.

Ubuntu
""""""

- Install ``bz2``, ``ncurses`` and ``zlib``.

.. code-block:: console

   $ sudo apt-get install zlib1g-dev libncurses5-dev libbz2-dev

- Make sure locale is set to UTF8.

.. code-block:: console

   $ export LC_ALL=en_US.UTF-8
   $ export LANG=en_US.UTF-8

Fedora
""""""

- Install ``bzip2``, ``ncurses`` and ``zlib``.

.. code-block:: console

   $ sudo dnf install zlib-devel ncurses-devel bzip2-devel

OpenSUSE
""""""""

- Install ``bz2``, ``ncurses``, and ``zlib``.

.. code-block:: console

   $ sudo zypper install zlib-devel libncurses5 ncurses5-devel libbz2-devel

OS X
""""

- Make sure you have `XCode <https://developer.apple.com/xcode/>`_ installed and
  have accepted the license agreement (run XCode at least once).

Windows
"""""""

- Install ``Cygwin`` and ``MSYS``.

.. note::

  An alternate method of installing Eta on Windows 10 is with WSL (`Windows sub-system for Linux <https://msdn.microsoft.com/en-gb/commandline/wsl/about>`_).

  1. Enable WSL within Windows (`Instructions <https://msdn.microsoft.com/en-gb/commandline/wsl/install_guide>`_)
  2. Minimum Windows version: Version 10 build 14986
  3. Enter the root folder of WSL, and create a “.local” folder, and then inside of this create a “bin” folder.
  4. Follow the source installation method.

Installation
""""""""""""

Clone the repository and run the install script at the root of the repository.
Replace ``[current-stable-tag]`` with the tag listed in the README of the eta repo.

  .. code-block:: console

     $ git clone --recursive --branch [current-stable-tag] https://github.com/typelead/eta
     $ cd eta
     $ ./install.sh # or install.cmd in windows command prompt

.. note::

  If you omit the ``--recursive`` flag to ``git clone``, you will need to
  initialize the project's submodules before running ``install.sh`` or ``install.cmd``:

  .. code-block:: console

     $ git submodule update --init --recursive

Once the installation is done, you will now have access to the following command-line tools:

- ``eta`` - The main compiler
- ``etlas`` - The package manager and build tool

Check to ensure that they are on the ``PATH`` with the following commands:

.. code-block:: console

   $ eta --version
   $ etlas --version

If you obtain an error that either tool is missing, run the following command:

.. code-block:: console

   $ stack path --local-bin

Add the path that you obtain in the output to your ``PATH`` environment variable.


Method 3: Docker Image
^^^^^^^^^^^^^^^^^^^^^^

Prerequisites
"""""""""""""

Make sure you have the following tools installed on your system:

- `Docker <https://docs.docker.com/engine/installation>`_

Installation
""""""""""""

To obtain an environment with ``eta`` and ``etlas``, run the following command:

.. code-block:: console

   $ docker run -it typelead/eta

Method 4: Nix Environment
^^^^^^^^^^^^^^^^^^^^^^^^^

Prerequisites
"""""""""""""

Make sure you have the following tools installed on your system:

- `Nix <https://nixos.org/nix/>`_

- Clone ``eta`` github repository (don't forget recursive mode) as explained in `Method 2: Source Installation`_ but **don't run** ``install.sh`` or ``install.cmd``

Installation
""""""""""""

To obtain an environment with ``eta`` and ``etlas``, from directory of ``eta`` github cloned repository, run the following command:

.. code-block:: console

   $ nix-shell -A eta-build-shell

Updating Eta
------------

Eta updates pretty fast and we're incorporating new patches on a daily basis that
you might want to get access to.

If you have Eta already installed, go to the root of this repository's clone on
your system, and run the following command:

.. code-block:: console

   $ ./update.sh # or update.cmd in windows command prompt

This will do a fresh installation, recompiling all the core libraries with the most
recent version of the compiler.

If you have existing Etlas projects, make sure you run

.. code-block:: console

   $ etlas clean
   $ etlas install --dependencies-only

inside each project before proceeding with your normal development so that Etlas
recognizes the updated libraries.

Running Your First Program
--------------------------

#. Create a new file called ``Main.hs`` and with the following contents::

    module Main where

    primes = filterPrime [2..]
      where filterPrime (p:xs) =
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

#. Run the following command on the command line to compile the program:

   .. code-block:: console

      $ eta Main.hs

   This will compile the program to a standalone JAR with the ``Run``- prefix.

#. Run the program with java:

   .. code-block:: console

      $ java -jar RunMain.jar

.. _setting-up-first-project:

Setting up your first Etlas Project
-----------------------------------

With Etlas, you don't have to worry about remembering all the particular flags to
sent to ``eta``. You can simply specify what you want in a human-readable format
called Cabal. To learn more about the specification file format which is also used
in the Haskell ecosystem, read
`this <https://www.haskell.org/cabal/users-guide/developing-packages.html>`_ guide.

#. Create a new directory called ``eta-first`` and enter it.

   .. code-block:: console

      $ mkdir eta-first
      $ cd eta-first

#. Initialize the project with Etlas.

   .. code-block:: console

      $ etlas init

      Package name? [default: eta-first]
      Package version? [default: 0.1.0.0]
      Please choose a license:
        1) GPL-2
        2) GPL-3
        3) LGPL-2.1
        4) LGPL-3
        5) AGPL-3
        6) BSD2
      * 7) BSD3
        8) MIT
        9) ISC
        10) MPL-2.0
        11) Apache-2.0
        12) PublicDomain
        13) AllRightsReserved
        14) Other (specify)
      Your choice? [default: BSD3]
      Author name? [default: ...]
      Maintainer email? [default: ...]
      Project homepage URL?
      Project synopsis?
      Project category:
      * 1) (none)
        2) Codec
        3) Concurrency
        4) Control
        5) Data
        6) Database
        7) Development
        8) Distribution
        9) Game
        10) Graphics
        11) Language
        12) Math
        13) Network
        14) Sound
        15) System
        16) Testing
        17) Text
        18) Web
        19) Other (specify)
      Your choice? [default: (none)]
      What does the package build:
        1) Library
        2) Executable
      Your choice? 2
      What is the main module of the executable:
      * 1) Main.hs (does not yet exist, but will be created)
        2) Main.lhs (does not yet exist, but will be created)
        3) Other (specify)
      Your choice? [default: Main.hs (does not yet exist, but will be created)]
      Source directory:
      * 1) (none)
        2) src
        3) Other (specify)
      Your choice? [default: (none)] 2
      What base language is the package written in:
      * 1) Haskell2010
        2) Haskell98
        3) Other (specify)
      Your choice? [default: Haskell2010] 1
      Add informative comments to each field in the cabal file (y/n)? [default: n] n

      Guessing dependencies...

      Generating LICENSE...
      Generating Setup.hs...
      Generating ChangeLog.md...
      Generating example.cabal...

   The project structure should look like this:

   .. code-block:: console

      eta-first
      - src
        - Main.hs
      - ChangeLog.md
      - LICENSE
      - eta-first.cabal
      - Setup.hs

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

#. Update ``eta-first.cabal``, adding an ``other-modules:`` field:

   .. code-block:: console

      other-modules: Primes

   Any additional modules you add to the project should be added at the same
   indentation level as the ``Primes`` entry, but below it.

#. To build & run, execute this command:

   .. code-block:: console

      $ etlas run

   .. note::

      Note that this will create a JAR file *without* dependencies. This is
      best suited for development.

      For production deployments, you may want to generate a standalone JAR
      file, also called an **uberjar**. If you would like to generate an uberjar,
      run the following two commands:

      .. code-block:: console

         $ etlas clean
         $ etlas configure --enable-uberjar-mode

      These commands need only be run once to set the local Etlas configuration.
      To go back to shared mode for the project:

      .. code-block:: console

         $ etlas clean
         $ etlas configure --disable-uberjar-mode

      Beware that this can be very slow. Work is being done to
      `improve uberjar performance <https://github.com/typelead/eta/issues/20>`_.

Learning Eta
------------
Now that you're set up with Eta, the next step is to learn about how to write Eta
programs. If you are already familiar with haskell you can jump straight to the
:ref:`interacting-with-java` section in Eta Tutorials to learn about how to
connect with Java libraries.

If you are new to Haskell and pure functional programming in general, we suggest
you to head over to `Tour of Eta <https://tour.eta-lang.org>`_. Check the
`Eta Blog <https://blog.eta-lang.org>`_ for more tutorials and updates.

For tutorials & examples, see the following:

- :ref:`eta-tutorials`
- `Eta 2048 Game Implementation <https://github.com/rahulmutt/eta-2048>`_
- `JDBC Example <https://github.com/tatut/eta-jdbc-example/blob/master/src/Main.hs>`_
- `Neo4j Example <https://github.com/Prillan/eta-neo4j-example>`_
- `Kafka Client <https://github.com/haskell-works/eta-kafka-client>`_
- `Kafka Conduit <https://github.com/haskell-works/eta-kafka-conduit>`_
- `Repository of Eta Examples <https://github.com/typelead/eta-examples>`_
- `Android Example in Eta <https://brianmckenna.org/blog/eta_android>`_
- `AWS Lambda Functions with Eta <https://mattops.io/aws-lambda-functions-with-eta/>`_
- `Eta Playground by Filippo Vitale <https://github.com/filippovitale/eta-playground/>`_
- `Integrate Eta into your Scala Projects <https://blog.eta-lang.org/integrating-eta-into-your-scala-projects-a8d494a2c5b0>`_
- `Basic Dependent Typing in Eta <https://github.com/typelead/eta-examples/tree/master/4-matrix>`_
- `Haskell Servant in Eta <https://github.com/rahulmutt/eta-servant-example>`_

For a list of the currently supported Haskell Packages, see:

- `Eta Hackage <https://github.com/typelead/eta-hackage>`_

Contact Us
----------

If you had trouble with this tutorial, you can give us feedback by:

- filing an `issue <https://github.com/typelead/eta/issues/new>`_
- discussing with us on `Gitter <https://gitter.im/typelead/eta>`_
