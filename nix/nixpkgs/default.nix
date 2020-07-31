args@{ ... }:

let
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json));
in
  import nixpkgs (args // { overlays = [ (import ../haskell/overlay.nix) ]; })
