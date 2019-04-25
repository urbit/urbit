{ pkgs, tlon, deps, brass, ship, debug }:

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
  PILL        = brass;
  SHIP        = ship;
}
