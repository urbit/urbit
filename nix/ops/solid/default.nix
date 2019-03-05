{ pkgs, tlon, deps, urbit, fakezod, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "solid";
  builder     = ./builder.sh;
  buildInputs = [ urbit tlon.urb pkgs.coreutils ];

  FAKEZOD = fakezod;
  ARVO    = arvo;
}
