{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chefkoch.nix { }
