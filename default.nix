with import <nixpkgs> { };

# To get a shell with all of Eta's dependencies:
#   $ nix-shell -A eta-build-shell
#   [nix-shell] $ eta-build uninstall
#   [nix-shell] $ eta-build install

let
  # Eta's Cabal can't be built with itself. Stack (incorrectly) caches a Simple
  # Setup so it accidentally works around this problem. Here we build a Setup so
  # that we can (incorrectly) reuse it, like Stack does.
  Setup = stdenv.mkDerivation {
    name = "eta-setup-hack";
    phases = ["buildPhase" "installPhase"];
    buildInputs = [hpkgs.ghc];
    buildPhase = ''
      ghc --make -o Setup ${./Setup.hs}
    '';
    installPhase = ''
      mv Setup $out
    '';
  };

  Cabal = self: haskell.lib.overrideCabal (self.callPackage ./utils/nix/Cabal.nix { }) (drv: {
    postCompileBuildDriver = ''
      rm Setup
      ln -s ${Setup} Setup
    '';
    doCheck = false;
  });

  hpkgs = haskell.packages.ghc7103.override {
    overrides = self: super: {
      tasty-ant-xml = haskell.lib.doJailbreak super.tasty-ant-xml;

      epm = haskell.lib.overrideCabal (self.callPackage ./utils/nix/epm.nix {
        Cabal = Cabal self;
      }) (drv: {
        postCompileBuildDriver = ''
          rm Setup
          ln -s ${Setup} Setup
        '';
        doCheck = false;
      });
      eta-pkg = haskell.lib.doJailbreak (self.callPackage ./utils/nix/eta-pkg.nix { });
      eta-pkgdb = self.callPackage ./utils/nix/eta-pkgdb.nix { };
      codec-jvm = self.callPackage ./utils/nix/codec-jvm.nix { };

      eta-build = self.callPackage ./utils/nix/eta-build.nix { };

      eta = haskell.lib.overrideCabal (self.callPackage ./utils/nix/eta.nix { }) (drv: {
        src = onlyFiles ["compiler" "include" "eta" "eta.cabal" "LICENSE"] drv.src;
        isLibrary = false;
        doCheck = false;
        jailbreak = true;
      });
    };
  };

  eta-build-shell = runCommand "eta-build-shell" {
    # Libraries don't pass -encoding to javac.
    LC_ALL = "en_US.utf8";
    buildInputs = [
      hpkgs.eta
      hpkgs.eta-build
      hpkgs.eta-pkg
      hpkgs.epm
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
hpkgs // { Cabal = Cabal hpkgs; inherit eta-build-shell; }
