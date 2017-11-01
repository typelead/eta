Etlas User Guide
================

.. note::

   This user guide is still under active development. Please consult the `Cabal 1.22.8.0 User Guide <http://cabal.readthedocs.io/en/latest>`_
   as it is very similar to Etlas.

Configuration File
------------------

You can find your Etlas user configuration file at:

* Unix-based systems (Linux, Mac): ``~/.etlas/config``
* Windows: ``%APPDATA%\Roaming\etlas\config``

The following sections will describe in detail the meaning of all the configuration
options.

Remote Repositories
^^^^^^^^^^^^^^^^^^^

Remote repositories contain indices that point to the locations of source and/or binary
packages that can be used with Eta. Etlas reads these indices and will query them
whenever you invoke ``etlas install [package-name]``.

.. code:: config
   repository [repository-name]
     url: [repository-url]
     git-indexed: [indexed]

where:
* ``[git-indexed]`` takes either ``True`` or ``False``.
  - ``True`` means that the remote repository is backed by a git repository that
    indexes all the packages and their corresponding source git repositories.
    (e.g. Etlas)
  - ``False`` means that the remote repository is backed by a server that mirrors
    Hackage. (e.g. Hackage itself)

* ``[repository-url]`` is the url to:
  - the git repository, if git-indexed
  - the Hackage mirror, if not git-indexed

Remaining Fields
^^^^^^^^^^^^^^^^

A field takes the form of:

.. code:: config
   [field-name]: [field-value]

For Example:

.. code:: config
   auto-update: True

+-----------------+------------------------+--------------+----------------+
| Name            | Description            | Valid Values | Default Value  |
+=================+========================+==============+================+
| auto-update     | If turned on, will automatically update the package index periodically caches so the user stays up to date with the latest package patches. | False, True | True |
+-----------------+------------------------+--------------+----------------+

TODO: This is incomplete.

Installing Packages
-------------------

Package Types
^^^^^^^^^^^^^

Eta packages come in two types:

1. **Hackage Packages** - Packages that can be found in `Hackage <https://hackage.haskell.org>`_, Haskell's package repository.

2. **Etlas Packages** - Etlas is the package repository for Eta. We refer to a
package that can only be compiled with Eta (and not GHC) as an Etlas package. This
means the package contains Eta-specific features or uses the Java Foreign Function
Interface.

`etlas` itself can only distinguish between the two based on the source package indexes.

From Etlas
^^^^^^^^^^

From Hackage
^^^^^^^^^^^^

Interacting with Etlas
----------------------

Interacting with Hackage
------------------------

Eta's `etlas` derives from Haskell's `cabal`, so it has great support for working with
Hackage, Haskell's package repository.

If you need to use a package from Hackage, you invoke the following command:

.. code-block:: console

   etlas install [hackage-package-name]

For Example:

Suppose you want to install the
`bytestring <https://hackage.haskell.org/package/bytestring>`_ package.

.. code-block:: console

   etlas install bytestring

There are cases where this command might take a long time.
If so, terminate the command (typically via Ctrl-C) and instead run:

.. code-block:: console

   etlas install [hackage-package-name] --allow-newer

which will fix in 99% of the time.

Etlas Packages Support
----------------------

You can install a package by typing:

.. code-block:: console

   etlas install [package-name]

Visit `eta-hackage <https://github.com/typelead/eta-hackage>`_ to get a list of
packages currently supported by Eta.

To know the details of the flags supported by ``etlas``:

.. code-block:: console

   etlas --help

This will help with a basic suggestion of what each flag does. 

Below we note some important commands associated with etlas:

#. .. code-block:: console

      etlas init

This initializes the skeleton of the project. It asks a few standard questions to build the project template.

#. .. code-block:: console

      etlas build

As the name implies, this builds the project. This should be executed from the root of the project where the ``cabal`` file exists.

#. .. code-block:: console

      etlas install

This command builds the source project and actually installs it in the ``.etlas`` directory of the home folder, making it universally accessible across your system.

#. .. code-block:: console

      etlas run

This command looks for the ``main :: IO ()`` method in the code and actually builds and then runs that method.

#. .. code-block:: console

      etlas configure

As the name implies, it is used to configure the project. You can find out more about each flag by typing ``etlas configure --help``.

#. .. code-block:: console

      etlas test

Actually execute the tests written in this project.

#. .. code-block:: console

      etlas haddock

Generates the `haddock <https://www.haskell.org/haddock/>`_ documentation of the project.

#. .. code-block:: console

      etlas install --enable-tests

This enables tests and install HSpec (and any other needed dependencies).

#. .. code-block:: console

      etlas sdist

It packages up the files needed to build the project into a ``tar`` ball. Head `over here <https://wiki.haskell.org/How_to_write_a_Haskell_program#Using_Cabal>`_ for more.

https://wiki.haskell.org/How_to_write_a_Haskell_program

A thorough coverage of all the available fields in a cabal file is mentioned `here <http://cabal.readthedocs.io/en/latest/cabal-projectindex.html>`_ 


Etlas Configuration
-------------------

You can edit the cabal configuration file to change various default settings for etlas and the external programs used by it, for *nix based systems this is:

#. .. code-block:: console

      ~/.cabal/config

The config file on a Windows system is:

#. .. code-block:: console

      %appdata%\cabal\config
      
Proxy Configuration
-------------------

Etlas tries to use your default proxy settings for its connections to download content. However, you can set the proxy settings for fetching maven dependencies in the cabal config file, as `java proxy options <https://docs.oracle.com/javase/8/docs/technotes/guides/net/proxies.html>`_:

Changing:

#. .. code-block:: console

      program-default-options
      
      ....
      
      -- java-options:

for something like (for https connections):

#. .. code-block:: console

      program-default-options
      
      ....
      
      java-options: -Dhttps.proxyHost=host -Dhttps.proxyPort=port -Dhttps.proxyUser=user -Dhttps.proxyPassword=pass
