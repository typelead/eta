# Source

## Requirements

You must have the following installed on your system:

- [Stack](https://docs.haskellstack.org/en/stable/README)
    - Make sure the path that is obtained from running `stack path --local-bin` is present on the `PATH`.
- [JDK 7 or above](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
    - Make sure `javac` and `java` are on the PATH.
- Check the OS-specific sections for additional requirements

## Ubuntu

-   Install `bz2`, `ncurses` and `zlib`.

    ```sh
    $ sudo apt-get install zlib1g-dev libncurses5-dev libbz2-dev
    ```

-   Make sure locale is set to UTF-8.

    ```sh
    $ export LC_ALL=en_US.UTF-8
    $ export LANG=en_US.UTF-8
    ```

## Fedora

Install `bzip2`, `ncurses` and `zlib`.

```sh
$ sudo dnf install zlib-devel ncurses-devel bzip2-devel
```

## OpenSUSE

Install `bz2`, `ncurses`, and `zlib`.

```sh
$ sudo zypper install zlib-devel libncurses5 ncurses5-devel libbz2-devel
```

## OS X

Make sure you have [XCode](https://developer.apple.com/xcode/) installed and
  have accepted the license agreement (run XCode at least once).

## Windows

On Windows, you have two options:

1.  [Windows Subsystem for Linux](https://msdn.microsoft.com/en-gb/commandline/wsl/about) (Windows 10 build 14986 or above)

    1. [Enable WSL](https://msdn.microsoft.com/en-gb/commandline/wsl/install_guide).
    2. Enter the root folder of WSL, and create a `.local` folder, and then inside of this create a `bin` folder.
    3. Follow the instructions for Ubuntu above to setup your environment.
    4. Proceed to source installation instructions below.

2.  [Cygwin](https://cygwin.com/index.html) & [MSYS](http://www.mingw.org/wiki/MSYS)

    1. [Install Cygwin](https://cygwin.com/install.html).
    2. [Install MSYS](http://www.mingw.org/wiki/MSYS).
    3. Proceed to source installation instructions below.

## Installation

You have two options for installing:

- The `stable` branch which contains the last version that had a binary release

```sh
$ git clone --branch stable --recursive --depth 1 https://github.com/typelead/eta
$ cd eta
```

- The `master` branch which has the latest version of Eta.

```sh
$ git clone --recursive --depth 1 https://github.com/typelead/eta
$ cd eta
```

Below are the OS-specific installation instructions:

**Linux/OS X**

```sh
$ ./install.sh
```

**Windows**

```sh
$ install.cmd
```

If you omit the `--recursive` flag to `git clone`, you will need to initialize the project’s submodules before installing:

```sh
$ git submodule sync
$ git submodule update --init --recursive
```

Once the installation is done, you will now have access to the following command-line tools:

- `eta` - The main compiler
- `etlas` - The package manager and build tool

Check to ensure that they are on the `PATH`.

```sh
$ eta --version
$ etlas --version
```

If you obtain an error that either tool is missing, run the following command:

```sh
$ stack path --local-bin
```

Add the path that you obtain in the output to your `PATH` environment variable.

## Updating Eta

If you want to update your source installation, you can follow the steps below.

### Existing Clone

If you already have an existing clone of the repository, you can run the command below:

**Linux/OS X**

```sh
$ ./update.sh
```

**Windows**

```sh
$ update.cmd
```

### Fresh Clone

If you want to do a fresh clone, you can run the steps below:

```sh
$ git clone --recursive --depth 1 https://github.com/typelead/eta
$ cd eta
```

**Linux/OS X**

```sh
$ ./cleaninstall.sh
```

**Windows**

```sh
$ cleaninstall.cmd
```

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
