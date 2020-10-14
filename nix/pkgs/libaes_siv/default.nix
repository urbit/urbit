{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name    = "libaes_siv";
  builder = ./builder.sh;
  src     = ../../../pkg/libaes_siv;

  nativeBuildInputs = [ pkgs.openssl ];
}
