{ pkgs, tlon, deps, fakezod, arvo, debug }:

let

  urbitExe = if debug
             then "${tlon.urbit-debug}/bin/urbit-debug -g"
             else "${tlon.urbit}/bin/urbit";

in

pkgs.stdenv.mkDerivation rec {
  name        = "brass";
  builder     = ./builder.sh;
  buildInputs = [ tlon.herb pkgs.coreutils ];

  URBIT   = urbitExe;
  FAKEZOD = fakezod;
  ARVO    = arvo;
}
