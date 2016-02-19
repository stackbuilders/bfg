{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-3_16" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
