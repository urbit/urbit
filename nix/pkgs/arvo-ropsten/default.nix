{ pkgs, landscape }:

pkgs.stdenv.mkDerivation {
  inherit landscape;

  name = "arvo-ropsten";
  buildInputs = [ pkgs.bc ];
  builder = ./builder.sh;
  src = ../../../pkg/arvo;
}
