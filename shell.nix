{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
        (import ./default.nix { inherit nixpkgs compiler; }).env
