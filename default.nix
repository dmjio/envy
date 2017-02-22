{ compiler ? "ghc802" }:
  let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell.packages.${compiler} = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: rec {
            envy = buildStrictly (self.callPackage ./envy.nix {});
          };
        };
      };
   };
   in with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler}; { 
     inherit envy;
   }
