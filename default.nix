{ compiler ? "ghc902" }:
let
  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ${compiler} = pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              envy = self.callCabal2nix "envy" ./. {};
            };
          };
        };
      };
    };
  };
in
  with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler};
  envy
