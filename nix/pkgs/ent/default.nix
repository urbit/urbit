{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "ent";
  src     = ../../../pkg/ent;
  builder = ./builder.sh;
}
