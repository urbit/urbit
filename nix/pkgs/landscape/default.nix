{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "landscape";
  builder = ./builder.sh;
  src = ../../../pkg/interface;
}
