{ pkgs, ge-additions, ed25519 }:

pkgs.stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  buildInputs = [ pkgs.openssl ed25519 ge-additions ];
}
