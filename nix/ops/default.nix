{ pkgs ? import ../nixpkgs.nix }:

let

  deps  = import ../deps { inherit pkgs; };
  tlon  = import ../pkgs { inherit pkgs; };
  arvo  = tlon.arvo;
  urbit = tlon.urbit;

in

rec {

  bootzod = import ./fakeship {
    inherit pkgs tlon deps urbit;
    brass = ../../bin/brass.pill;
    ship = "zod";
  };

  bootbus = import ./fakeship {
    inherit pkgs tlon deps urbit;
    brass = ../../bin/brass.pill;
    ship = "bus";
  };

  test = import ./test {
    inherit pkgs tlon deps urbit arvo;
    ship = bootzod;
  };

  solid = import ./solid {
    inherit arvo pkgs tlon deps urbit;
    fakezod = bootzod;
  };

  brass = import ./brass {
    inherit arvo pkgs tlon deps urbit;
    fakezod = bootzod;
  };

  fakezod = import ./fakeship {
    inherit pkgs tlon deps urbit brass;
    ship = "zod";
  };

}
