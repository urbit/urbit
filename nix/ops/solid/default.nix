{ pkgs, tlon, deps, urbit, fakezod, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "solid";
  builder     = ./builder.sh;
  buildInputs = [ urbit tlon.herb pkgs.coreutils ];

  FAKEZOD = fakezod;
  ARVO    = arvo;
}
