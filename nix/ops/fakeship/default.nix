{ pkgs, herb, urbit, pill, ship, arvo }:

pkgs.stdenv.mkDerivation {
  name        = "fake" + ship;
  builder     = ./builder.sh;
  buildInputs = [ herb ];
  URBIT       = urbit.meta.exe;
  ARVO        = arvo;
  PILL        = pill;
  SHIP        = ship;
}
