{ mkDerivation, aeson, array, base, bytestring, cmdargs, containers
, directory, filepath, megaparsec, mtl, pretty, process, semigroups
, stdenv, tasty, tasty-hunit, tasty-rerun, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "egg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base cmdargs containers directory filepath megaparsec mtl
    pretty process semigroups tasty tasty-hunit tasty-rerun text
    transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring directory filepath tasty tasty-hunit
    tasty-rerun text unordered-containers
  ];
  description = "Starter code for compiler-fer-de-lance";
  license = stdenv.lib.licenses.mit;
}
