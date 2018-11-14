{ compiler ? "ghc844" }:
let
  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ${compiler} = pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              envy = buildStrictly (self.callPackage ./envy.nix {});
            };
          };
        };
      };
    };
  };
in
  with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler};
  envy
