let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "ent";
  packages = ps: [ ps.ent ];
}
