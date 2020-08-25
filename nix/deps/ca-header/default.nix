{ pkgs }:

pkgs.stdenv.mkDerivation {
  name              = "ca-bundle.h";
  builder           = ./builder.sh;
  nativeBuildInputs = [ pkgs.cacert pkgs.xxd ];
}
