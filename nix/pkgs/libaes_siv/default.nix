{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "libaes_siv";
  builder = ./builder.sh;
  src     = ../../../pkg/libaes_siv;

  nativeBuildInputs = [ pkgs.openssl ];
}
