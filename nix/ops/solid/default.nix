{ pkgs, tlon, deps, fakezod, arvo, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation rec {
  name        = "solid";
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb ];

  URBIT   = urbitExe
  FAKEZOD = fakezod;
  ARVO    = arvo;
}
