{ pkgs ? import ../nixpkgs.nix, debug ? false }:

let

  tlon  = import ../pkgs { inherit pkgs; };
  arvo  = tlon.arvo;
  herb  = tlon.herb;
  urbit = if debug then tlon.urbit-debug else tlon.urbit;

  bootbrass = ../../bin/brass.pill;
  bootsolid = ../../bin/solid.pill;

  rawzod = import ./fakeship {
    inherit pkgs herb urbit;
    pill = bootsolid;
    ship = "zod";
    arvo = null;
  };

  zod = import ./fakeship {
    inherit pkgs herb urbit arvo;
    pill = bootsolid;
    ship = "zod";
  };

  bus = import ./fakeship {
    inherit pkgs herb urbit arvo;
    pill = bootsolid;
    ship = "bus";
  };

in

rec {

  test = import ./test {
    inherit pkgs herb urbit;
    ship = bus;
  };

  solid = import ./solid {
    inherit pkgs herb urbit arvo;
    pier = rawzod;
  };

  brass = import ./brass {
    inherit pkgs herb urbit arvo;
    pier = zod;
  };

  ivory = import ./ivory {
    inherit pkgs herb urbit arvo;
    pier = zod;
  };

  image = import ./image {
    inherit pkgs urbit;
    pill = bootsolid;
  };

}
