{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "libaes_siv";
  src  = ../../../pkg/libaes_siv;
  builder = ./builder.sh;

  buildInputs = [ pkgs.openssl ];
}
