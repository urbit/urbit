{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "murmur3";
  src     = pkgs.sources.murmur3;
  builder = ./builder.sh;
}
