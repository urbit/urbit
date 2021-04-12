{ lib, stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, herb
, withRopsten ? false }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/brass.pill; };

in {
  build = import ./builder.nix {
    inherit stdenvNoCC urbit herb;

    name = "brass" + lib.optionalString withRopsten "-ropsten";
    builder = ./brass.sh;
    arvo = if withRopsten then arvo.ropsten else arvo;
    pier = bootFakeShip {
      inherit urbit herb;

      pill = solid.lfs;
      ship = "zod";
    };
  };
} // lib.optionalAttrs (!withRopsten) { inherit lfs; }
