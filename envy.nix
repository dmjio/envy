{ mkDerivation, base, bytestring, containers, hspec, mtl
, quickcheck-instances, stdenv, text, time
}:
mkDerivation {
  pname = "envy";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring containers mtl quickcheck-instances text time
  ];
  testDepends = [
    base bytestring hspec mtl quickcheck-instances text time
  ];
  description = "Parsing for environment variables";
  license = stdenv.lib.licenses.bsd3;
}
