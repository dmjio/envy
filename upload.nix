{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
with pkgs.haskell.lib;
sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callCabal2nix "envy" ./. {}))

