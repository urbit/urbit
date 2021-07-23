{ stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, herb }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/solid.pill; };

in {
  inherit lfs;

  build = import ./builder.nix {
    inherit stdenvNoCC urbit arvo herb;

    name = "solid";
    builder = ./solid.sh;
    pier = bootFakeShip {
      inherit urbit herb;

      arvo = null;
      pill = solid.lfs;
      ship = "zod";
    };
  };
}
