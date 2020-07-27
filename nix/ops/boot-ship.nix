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
  herb  = tlon.herb;

in

import ./fakeship {
  inherit pkgs arvo pill ship urbit herb;
}
