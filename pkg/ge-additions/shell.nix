let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "ge-additions";
  packages = ps: [ ps.ge-additions ];
}
