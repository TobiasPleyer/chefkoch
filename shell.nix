{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
