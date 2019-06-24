{ pkgs, tlon, deps, pier, arvo, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation rec {
  name        = "brass";
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb pkgs.coreutils ];

  URBIT = urbitExe;
  PIER  = pier;
  ARVO  = arvo;
}
