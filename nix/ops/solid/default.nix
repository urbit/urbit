{ pkgs, herb, urbit, pier, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "solid";
  builder     = ./builder.sh;
  buildInputs = [ herb ];

  URBIT = urbit.exe;
  PIER  = pier;
  ARVO  = arvo;
}
