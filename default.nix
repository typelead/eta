with import <nixpkgs> { };

let
  rootName = name: builtins.elemAt (lib.splitString "/" name) 0;
  isValidFile = name: files: builtins.elem (rootName name) files;
  relative = path: name: lib.removePrefix (toString path + "/") name;
  onlyFiles = files: path: builtins.filterSource (name: type: isValidFile (relative path name) files) path;

  eta-nix = fetchTarball "https://github.com/eta-lang/eta-nix/archive/1e53e1cb7a900585949c36df4340e41c5cbcda5d.tar.gz";

  rewriteRelative = top: path:
    let path' = lib.removePrefix top (builtins.toString path);
    in if lib.isStorePath path' then path' else ./. + path';

  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (lib.overrideExisting args {
      src = rewriteRelative eta-nix args.src;
    });

    etlas = haskell.lib.overrideCabal super.etlas (drv: {
      # Nix should only compile Setup.hs with setup-depends, but it doesn't:
      # https://github.com/NixOS/nixpkgs/issues/24809
      preCompileBuildDriver = ''
        ${drv.preCompileBuildDriver or ""}
        setupCompileFlags+=" -hide-package=etlas-cabal"
      '';
    });
    eta = haskell.lib.overrideCabal super.eta (drv: {
      # Makes the build a bit faster
      src = onlyFiles ["compiler" "include" "eta" "eta.cabal" "LICENSE" "tests"] drv.src;
    });
  };
  hpkgs = (import eta-nix { }).override { inherit overrides; };
in
hpkgs // {
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
}
