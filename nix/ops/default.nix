{ pkgs ? import ../nixpkgs.nix, debug ? false }:

let

  deps  = import ../deps { inherit pkgs; };
  tlon  = import ../pkgs { inherit pkgs; };
  arvo  = tlon.arvo;
  urbit = tlon.urbit;

  bootbrass = ../../bin/brass.pill;
  bootsolid = ../../bin/solid.pill;

  rawzod = import ./fakeship {
    inherit pkgs tlon deps debug;
    pill = bootsolid;
    ship = "zod";
    arvo = null;
  };

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

in

rec {

  test = import ./test {
    inherit pkgs tlon deps debug;
    ship = bus;
  };

  solid = import ./solid {
    inherit arvo pkgs tlon deps debug;
    pier = rawzod;
  };

  brass = import ./brass {
    inherit arvo pkgs tlon deps debug;
    pier = zod;
  };

  ivory = import ./ivory {
    inherit arvo pkgs tlon deps debug;
    pier = zod;
  };

}
