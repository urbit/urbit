{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "softfloat3";
  src     = pkgs.sources.softfloat3;
  builder = ./builder.sh;
}
