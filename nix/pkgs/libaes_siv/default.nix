{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "libaes_siv";
  src  = ../../../pkg/libaes_siv;

  buildInputs = [ pkgs.openssl ];
}
