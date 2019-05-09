let

  rev  = "61c3169a0e17d789c566d5b241bfe309ce4a6275";
  hash = "0qbycg7wkb71v20rchlkafrjfpbk2fnlvvbh3ai9pyfisci5wxvq";
  pkgs = builtins.fetchTarball {
    name = "nixpkgs-2019-01-15";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = hash;
  };

in

import pkgs {}
