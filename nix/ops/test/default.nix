{ pkgs, herb, urbit, ship }:

pkgs.stdenv.mkDerivation rec {
  name        = "test";
  builder     = ./builder.sh;
  buildInputs = [ herb ];

  URBIT = urbit.meta.exe;
  SHIP  = ship;
}
