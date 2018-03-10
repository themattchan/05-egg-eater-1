let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
	  egg = haskellPackagesNew.callPackage ./default.nix {};
	  }; }; }; };
      
  pkgs = import <nixpkgs> { inherit config; };

in  { egg = pkgs.haskellPackages.egg; }