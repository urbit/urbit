let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "urbit";
  packages = ps: [ ps.urbit ];
}
