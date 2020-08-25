{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "ent-7506f";
  builder = ./builder.sh;
  src     = ../../../pkg/ent;
}
