{ pkgs, tlon, deps, urbit, fakezod, arvo }:

pkgs.stdenv.mkDerivation rec {
  name        = "brass";
  builder     = ./builder.sh;
  buildInputs = [ urbit tlon.urb pkgs.coreutils ];

  FAKEZOD = fakezod;
  ARVO    = arvo;
}
