{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
with pkgs.haskell.lib;
sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./envy.nix {}))

