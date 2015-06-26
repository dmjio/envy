{ mkDerivation, base, containers, hspec, stdenv, text, transformers
}:
mkDerivation {
  pname = "envy";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base containers hspec text transformers ];
  testDepends = [ base hspec text transformers ];
  description = "Continuation based parsing for environment variables";
  license = stdenv.lib.licenses.bsd3;
}
