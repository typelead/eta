# Source

## Requirements

You must have the following installed on your system:

- [JDK 7 or above](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
- [Git 1.8 or above](https://git-scm.com/book/en/v1/Getting-Started-Installing-Git)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

You can replace `[tag]` with one of the tags listed [here](https://github.com/typelead/etlas/releases).

```sh
$ git clone --recursive https://github.com/typelead/etlas
$ git checkout [tag]
$ git submodule sync
$ git submodule update --init --recursive
$ stack install etlas
```

## Next Section

Now that we have installed Etlas, let's learn about the basics.
