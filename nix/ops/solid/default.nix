{ pkgs, tlon, deps, pier, arvo, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation rec {
  name        = "solid";
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb ];

  URBIT = urbitExe;
  PIER  = pier;
  ARVO  = arvo;
}
