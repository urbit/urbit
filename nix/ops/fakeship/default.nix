{ pkgs, tlon, deps, pill, ship, arvo, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation {
  name        = "fake" + ship;
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb ];
  URBIT       = urbitExe;
  ARVO        = arvo;
  PILL        = pill;
  SHIP        = ship;
}
