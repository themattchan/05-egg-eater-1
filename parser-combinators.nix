{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parser-combinators";
  version = "0.4.0";
  sha256 = "1azkz0a6ikym02s8wydjcklp7rz8k512bs4s9lp9g1g03m0yj95i";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
