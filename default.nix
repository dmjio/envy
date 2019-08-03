{ compiler ? "ghc865" }:
let
  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ${compiler} = pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              envy = buildStrictly (self.callCabal2nix "envy" ./. {});
            };
          };
        };
      };
    };
  };
in
  with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler};
  envy
