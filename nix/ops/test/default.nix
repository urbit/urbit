{ pkgs, tlon, deps, urbit, ship, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "test";
  builder     = ./builder.sh;
  buildInputs = [ urbit tlon.herb pkgs.coreutils ];

  SHIP = ship;
  ARVO = arvo;
}
