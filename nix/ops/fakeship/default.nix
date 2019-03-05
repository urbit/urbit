{ pkgs, tlon, deps, urbit, brass, ship ? "zod" }:

pkgs.stdenv.mkDerivation rec {
  name        = "fake" + ship;
  builder     = ./builder.sh;
  buildInputs = [ urbit tlon.urb ];
  PILL        = brass;
  SHIP        = ship;
}
