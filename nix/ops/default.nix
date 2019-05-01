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

  bootzod = import ./fakeship {
    inherit pkgs tlon deps debug;
    brass = bootbrass;
    ship = "zod";
  };

  bootbus = import ./fakeship {
    inherit pkgs tlon deps debug;
    brass = bootbrass;
    ship = "bus";
  };

  test = import ./test {
    inherit pkgs tlon deps arvo debug;
    ship = bootzod;
  };

  solid = import ./solid {
    inherit arvo pkgs tlon deps debug;
    fakezod = bootzod;
  };

  brass = import ./brass {
    inherit arvo pkgs tlon deps debug;
    fakezod = bootzod;
  };

  fakezod = import ./fakeship {
    inherit pkgs tlon deps brass debug;
    ship = "zod";
  };

}
