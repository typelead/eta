# Revision history for eta

## 0.0.6b8  -- 2017-05-30

### Eta
* `Java.Do` module is now available that lets you use do-notation for Java objects that satisfy a monad-like structure.
* The `getClass#` primop has been removed due to being ill-defined (performs type inspection). (#403)

### Etlas
* Eta binaries are installed if no `eta` is found on the PATH.
* Proxy settings now work (#387).

## 0.0.6b7  -- 2017-05-22

### Eta

* `eta --numeric-version` now returns the build number as well.
  - For Eta v1.2.6b6, `eta --numeric-version` will return `1.2.6.6`

* The compiler now passes two CPP constants - `ETA_VERSION` and `ETA_BUILD_NUMBER`
  - For Eta v1.2.6b6, `ETA_VERSION=126` and `ETA_BUILD_NUMBER=6`

* The global package database has moved from `~/.eta/package.conf.d/*` to `~/.eta/[numeric-version]/package.conf.d/*`.
  - This was done to facilitate multiple Eta installations simultaneously.

* `cleaninstall.sh`/`Build.hs` have some minor changes:
  - You can pass `-` to specify that you want the default installation directory.
  - All additional arguments are passed to `Build.hs`.
  - A new flag `--binaries` is now supported that allows you to select the folder in which to store binary distributions of the boot libraries.

### Etlas

- Fixed #313 so that you can now download the package index from Hackage directly.
- `etlas bdist` will create a binary distribution of the package for which you want to compile.
  - If the package is `base-4.8.2.0`, then the binary package is `base-4.8.2.0-bin.tar.gz` and it will be placed in the `dist` folder.
- `etlas install --binaries-output-dir=[output-dir]` will create binary distributions for the current package and all its dependencies and place them in `[output-dir]`.
- `etlas install [package]-bin.tar.gz` will install a binary distribution.

## 0.0.6b6  -- 2017-05-18

* Fixed serious regression on Maybe handling in FFI imports. (#315)



