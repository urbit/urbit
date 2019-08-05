{ pkgs }:

pkgs.stdenv.mkDerivation {
  name              = "ca-bundle.h";
  builder           = ./builder.sh;
  nativeBuildInputs = with pkgs; [ cacert xxd ];
}
