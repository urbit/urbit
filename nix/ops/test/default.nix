{ pkgs, tlon, deps, ship, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation rec {
  name        = "test";
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb ];

  URBIT = urbitExe;
  SHIP  = ship;
}
