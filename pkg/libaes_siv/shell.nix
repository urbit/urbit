let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "libaes_siv";
  packages = ps: [ ps.libaes_siv ];
}
