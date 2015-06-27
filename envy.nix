{ mkDerivation, base, bytestring, containers, hspec, mtl
, QuickCheck, quickcheck-instances, stdenv, text, time
, transformers
}:
mkDerivation {
  pname = "envy";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring containers mtl text time transformers
  ];
  testDepends = [
    base bytestring hspec mtl QuickCheck quickcheck-instances text time
    transformers
  ];
  description = "An environmentally friendly way to deal with environment variables";
  license = stdenv.lib.licenses.bsd3;
}
