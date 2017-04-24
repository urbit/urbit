{ nixpkgs }:

nixpkgs.stdenv.mkDerivation rec {
  name = "pkg-config-cross";
  builder = ./builder.sh;
}
