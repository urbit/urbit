let

  rev  = "0a11634a29c1c9ffe7bfa08fc234fef2ee978dbb";
  pkgs = builtins.fetchTarball {
    name = "nixpkgs-2020-06-06";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0vj5k3djn1wlwabzff1kiiy3vs60qzzqgzjbaiwqxacbvlrci10y";
  };

in

import pkgs {}
