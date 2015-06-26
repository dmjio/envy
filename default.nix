{ nixpkgs ? import <nixpkgs> {} }:
            nixpkgs.pkgs.haskellPackages.callPackage ./env.nix
           { }
