let
config = {
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        parser-combinators = haskellPackagesNew.callPackage ./parser-combinators.nix { };
        megaparsec =
          pkgs.haskell.lib.dontHaddock
          (pkgs.haskell.lib.dontCheck
          (haskellPackagesNew.callPackage ./megaparsec6.nix { }));
  	    egg = haskellPackagesNew.callPackage ./default.nix { };
	    };
    };
  };
};

pkgs = import <nixpkgs> { inherit config; };

in  { egg = pkgs.haskellPackages.egg; }
