{ pkgs, herb, urbit, pier, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "brass";
  builder     = ./builder.sh;
  buildInputs = [ herb pkgs.coreutils ];

  URBIT = ubit.meta.exe;
  PIER  = pier;
  ARVO  = arvo;
}
