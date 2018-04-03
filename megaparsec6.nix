{ mkDerivation, base, bytestring, case-insensitive, containers
, deepseq, hspec, hspec-expectations, mtl, parser-combinators
, QuickCheck, scientific, stdenv, text, transformers
}:
mkDerivation {
  pname = "megaparsec";
  version = "6.4.1";
  sha256 = "0w0kw8g7c6c3sp0fpgfqjc2w032dv9s7jnyn1dx71hk5mifh2h6y";
  revision = "2";
  editedCabalFile = "ce4a50df9eb57fd9c16c72443dd7363bf2e51b65110b4451a6ddd944a7a0046e";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring containers hspec hspec-expectations mtl QuickCheck
    scientific text transformers
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
