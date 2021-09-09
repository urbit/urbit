let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "urcrypt";
  packages = ps: [ ps.urcrypt ];
}
