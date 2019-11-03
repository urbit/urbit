{ pkgs, herb, urbit, pier, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "brass";
  builder     = ./builder.sh;
  buildInputs = [ herb pkgs.coreutils ];

  URBIT = urbit.meta.exe;
  PIER  = pier;
  ARVO  = arvo;
}
