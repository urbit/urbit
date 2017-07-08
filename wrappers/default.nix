{ nixpkgs }:
nixpkgs.stdenv.mkDerivation rec {
  name = "cross-wrappers";
  builder = ./builder.sh;
}
