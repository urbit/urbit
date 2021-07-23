let

  pkgs = import ../../default.nix { };

in pkgs.shellFor {
  name = "herb";
  packages = ps: [ ps.herb ];
}
