with import <nixpkgs> { };

let
  rootName = name: builtins.elemAt (lib.splitString "/" name) 0;
  isValidFile = name: files: builtins.elem (rootName name) files;
  relative = path: name: lib.removePrefix (toString path + "/") name;
  onlyFiles = files: path: builtins.filterSource (name: type: isValidFile (relative path name) files) path;

  eta-nix = fetchTarball "https://github.com/eta-lang/eta-nix/archive/2ec126d2caaa4636faeb4acd07f9143169bc5631.tar.gz";

  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (lib.overrideExisting args {
      src = if args.src.url == "https://github.com/typelead/eta.git" then ./. else args.src;
    });

    eta = haskell.lib.overrideCabal super.eta (drv: {
      # Makes the build a bit faster
      src = onlyFiles ["compiler" "include" "eta" "eta.cabal" "LICENSE" "tests"] drv.src;
    });
  };
  eta = import eta-nix { inherit pkgs overrides; };
  etaPackages = callPackage "${eta-nix}/eta-modules" { etaHaskellPackages = eta; };
in
eta // {
  inherit etaPackages;
}
