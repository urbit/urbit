{ pkgs, landscape }:

pkgs.stdenv.mkDerivation {
  inherit landscape;

  name = "arvo";
  builder = ./builder.sh;
  src = ../../../pkg/arvo;
  meta = {
    priority = 0;
  };

}
