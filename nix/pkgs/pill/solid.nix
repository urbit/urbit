{ stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, curl }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/solid.pill; };

in {
  inherit lfs;

  build = import ./builder.nix {
    inherit stdenvNoCC urbit arvo curl;

    name = "solid";
    builder = ./solid.sh;
    pier = bootFakeShip {
      inherit urbit;

      arvo = null;
      pill = solid.lfs;
      ship = "zod";
    };
  };
}
