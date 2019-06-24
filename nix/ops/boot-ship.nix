{ pkgs ? import ../nixpkgs.nix
, debug ? false
, ship ? "zod"
, pill ? ../../bin/solid.pill
}:

let

  deps  = import ../deps { inherit pkgs; };
  tlon  = import ../pkgs { inherit pkgs; };
  arvo  = tlon.arvo;
  urbit = tlon.urbit;

in

import ./fakeship {
  inherit pkgs tlon deps arvo pill ship debug;
}
