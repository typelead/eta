# Etlas

## How It Works

You'll download the Etlas binary for your platform and make it available in your system `PATH` so that you can use the `etlas` command from your terminal. Etlas will install Eta for you when you build your first project.

## Requirements

You must have the following installed on your system:

- [JDK 7 or above](https://www.oracle.com/java/technologies/javase-downloads.html)
- [Git 1.8 or above](https://git-scm.com/book/en/v1/Getting-Started-Installing-Git)

## Installation

1.  Download the `etlas` executable for your platform below.

    **Etlas v1.5.0.0**

    - [Linux (64-bit)](https://cdnverify.eta-lang.org/eta-binaries/etlas-1.5.0.0/binaries/x86_64-linux/etlas)
    - [Mac (64-bit)](https://cdnverify.eta-lang.org/eta-binaries//etlas-1.5.0.0/binaries/x86_64-osx/etlas)
    - [Windows (64-bit)](https://cdnverify.eta-lang.org/eta-binaries/etlas-1.5.0.0/binaries/x86_64-windows/etlas.exe)

    ### Note:

    On Safari the extension of the etlas binary is `dms` when it shouldn't have an extension at all. You can fix this by manually renaming the file to avoid the `.dms` extension.

2.  If on Linux or OS X, give the program executable permissions.

    ```sh
    $ chmod +x /path/to/etlas
    ```

3.  Place the downloaded binary in in your `$PATH` and verify that it is.

    ```sh
    $ etlas --version
    ```

4.  Make sure `java` is on the `$PATH`.

    ```sh
    $ java -version
    ```

5.  Ensure that you have updated to the latest version of Eta.

    ```sh
    $ etlas update
    $ etlas select latest
    ```


6.  As soon as you start using `etlas`, `eta` and the standard libraries will be downloaded automatically for you on demand.

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
