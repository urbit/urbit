let

  pkgs = import ./nix/pkgs {};
  deps = import ./nix/deps {};

in

  deps // pkgs
