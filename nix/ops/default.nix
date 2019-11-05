{ pkgs ? import ../nixpkgs.nix, debug ? false }:

let

  tlon         = import ../pkgs { inherit pkgs; };
  arvo         = tlon.arvo;
  arvo-ropsten = tlon.arvo-ropsten;
  herb         = tlon.herb;
  urbit        = if debug then tlon.urbit-debug else tlon.urbit;

  bootbrass = ../../bin/brass.pill;
  bootsolid = ../../bin/solid.pill;

  rawzod = import ./fakeship {
    inherit pkgs herb urbit;
    pill = bootsolid;
    ship = "zod";
    arvo = null;
  };

  ropzod = import ./fakeship {
    inherit pkgs herb urbit;
    pill = bootsolid;
    ship = "zod";
    arvo = arvo-ropsten;
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

  brass-ropsten = import ./brass {
    inherit pkgs herb urbit;
    arvo = arvo-ropsten;
    pier = ropzod;
  };

  ivory = import ./ivory {
    inherit pkgs herb urbit arvo;
    pier = zod;
  };

  ivory-ropsten = import ./ivory {
    inherit pkgs herb urbit;
    arvo = arvo-ropsten;
    pier = ropzod;
  };

  image = import ./image {
    inherit pkgs urbit;
    pill = bootsolid;
  };

}
