let

  rev  = "19.09";
  hash = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  pkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = hash;
  };

in

import pkgs {}
