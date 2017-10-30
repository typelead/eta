with import <nixpkgs> { };

# To get a shell with all of Eta's dependencies:
#   $ nix-shell -A eta-build-shell
#   [nix-shell] $ eta-build uninstall
#   [nix-shell] $ eta-build install

let
  hpkgs = haskell.packages.ghc7103.override {
    overrides = self: super: {
      Cabal_1_24_2_0 = haskell.lib.overrideCabal super.Cabal_1_24_2_0 (drv: {
        # Cabal bug doesn't compile Setup.hs with MIN_VERSION set:
        # https://github.com/haskell/cabal/issues/3003#issuecomment-167572308
        preCompileBuildDriver = ''
          ${drv.preCompileBuildDriver or ""}
          setupCompileFlags+=" -DMIN_VERSION_binary_0_8_0=1"
        '';
      });
      Cabal = self.Cabal_1_24_2_0;

      tasty-ant-xml = haskell.lib.doJailbreak super.tasty-ant-xml;
      binary = haskell.lib.dontCheck self.binary_0_8_5_1 or self.binary_0_8_4_1;

      codec-jvm = self.callPackage ./utils/nix/codec-jvm.nix { };
      hackage-security = haskell.lib.dontCheck (self.callPackage ./utils/nix/hackage-security.nix { });
      eta-boot-th = self.callPackage ./utils/nix/eta-boot-th.nix { };
      eta-boot = self.callPackage ./utils/nix/eta-boot.nix { };
      eta-pkg = self.callPackage ./utils/nix/eta-pkg.nix { };
      etlas-cabal = self.callPackage ./utils/nix/etlas-cabal.nix { };

      etlas = haskell.lib.overrideCabal (self.callPackage ./utils/nix/etlas.nix { }) (drv: {
        # Nix should only compile Setup.hs with setup-depends, but it doesn't:
        # https://github.com/NixOS/nixpkgs/issues/24809
        preCompileBuildDriver = ''
          ${drv.preCompileBuildDriver or ""}
          setupCompileFlags+=" -hide-package=etlas-cabal"
        '';
        isLibrary = false;
        jailbreak = true;
      });

      eta = haskell.lib.overrideCabal (self.callPackage ./utils/nix/eta.nix { }) (drv: {
        src = onlyFiles ["compiler" "include" "eta" "eta.cabal" "LICENSE"] drv.src;
        isLibrary = false;
        doCheck = false;
        jailbreak = true;
      });

      eta-build = self.callPackage ./utils/nix/eta-build.nix { };

      zip = haskell.lib.dontCheck super.zip;
    };
  };

  eta-build-shell = runCommand "eta-build-shell" {
    # Libraries don't pass -encoding to javac.
    LC_ALL = "en_US.utf8";
    buildInputs = [
      hpkgs.eta
      hpkgs.eta-build
      hpkgs.eta-pkg
      hpkgs.etlas
      gitMinimal
      jdk
      glibcLocales
    ];
  } "";

  rootName = name: builtins.elemAt (lib.splitString "/" name) 0;
  isValidFile = name: files: builtins.elem (rootName name) files;
  relative = path: name: lib.removePrefix (toString path + "/") name;
  onlyFiles = files: path: builtins.filterSource (name: type: isValidFile (relative path name) files) path;
in
hpkgs // { inherit eta-build-shell; }
