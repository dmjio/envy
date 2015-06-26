{ mkDerivation, base, bytestring, containers, hspec, QuickCheck
, quickcheck-instances, stdenv, text, time
}:
mkDerivation {
  pname = "envy";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring containers hspec QuickCheck quickcheck-instances
    text time
  ];
  testDepends = [
    base bytestring hspec QuickCheck quickcheck-instances text time
  ];
  description = "Continuation based parsing for environment variables";
  license = stdenv.lib.licenses.bsd3;
}
