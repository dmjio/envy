{ mkDerivation, base, containers, hspec, stdenv, text }:
mkDerivation {
  pname = "env";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base containers text ];
  testDepends = [ base hspec ];
  description = "Continuation based parsing for environment variables";
  license = stdenv.lib.licenses.bsd3;
}
