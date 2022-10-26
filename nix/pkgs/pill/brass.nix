{ lib, stdenvNoCC, fetchGitHubLFS, bootFakeShip, solid, urbit, arvo, curl
, withGoerli ? false }:

let

  lfs = fetchGitHubLFS { src = ../../../bin/brass.pill; };

in {
  build = import ./builder.nix {
    inherit stdenvNoCC urbit curl;

    name = "brass" + lib.optionalString withGoerli "-goerli";
    builder = ./brass.sh;
    arvo = if withGoerli then arvo.goerli else arvo;
    pier = bootFakeShip {
      inherit urbit;

      pill = solid.lfs;
      ship = "zod";
    };
  };
} // lib.optionalAttrs (!withGoerli) { inherit lfs; }
