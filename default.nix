{ pkgs ? import ./nix/nixpkgs.nix { } }:

let

  self = import ./nix/pkgs { inherit pkgs; };
  deps = import ./nix/deps { inherit pkgs; };

in deps // self
