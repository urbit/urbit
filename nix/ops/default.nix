{ pkgs ? import ../nixpkgs.nix, debug ? false }:

let

  deps  = import ../deps { inherit pkgs; };
  tlon  = import ../pkgs { inherit pkgs; };
  arvo  = tlon.arvo;
  urbit = tlon.urbit;

  bootbrass = ../../bin/brass.pill;
  bootsolid = ../../bin/solid.pill;

in

rec {

  zod = import ./fakeship {
    inherit pkgs tlon deps arvo debug;
    pill = bootsolid;
    ship = "zod";
  };

  bus = import ./fakeship {
    inherit pkgs tlon deps arvo debug;
    pill = bootsolid;
    ship = "bus";
  };

  test = import ./test {
    inherit pkgs tlon deps debug;
    ship = bus;
  };

  solid = import ./solid {
    inherit arvo pkgs tlon deps debug;
    pier = zod;
  };

  brass = import ./brass {
    inherit arvo pkgs tlon deps debug;
    pier = zod;
  };

}
