let

  rev  = "d50d46941ee4445ffcc257c41cf60a73f31c367d";
  hash = "0drg5r3ak3myv7cifikv68d2iyb6h55531r3d4drp2sfmchc9l1v";
  pkgs = builtins.fetchTarball {
    name = "nixpkgs-2020-03-06";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = hash;
  };

in

import pkgs {}
