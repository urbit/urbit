let

  rev  = "368b72a71dee920c79d98b27b524d62550a347ec";
  hash = "0m970cy8366sdiw90345f1fa2zyicd5vinmjljfxg0nx9p934rcp";
  pkgs = builtins.fetchTarball {
    name = "nixpkgs-2019-01-15";
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    sha256 = hash;
  };

in

import pkgs {}
