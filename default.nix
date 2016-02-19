{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-3_16" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./bfg.nix { }
