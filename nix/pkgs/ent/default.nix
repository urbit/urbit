{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name    = "ent-7506f";
  builder = ./builder.sh;
  src     = ../../../pkg/ent;
}
