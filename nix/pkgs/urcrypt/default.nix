{ pkgs, ed25519, ge-additions, argon2, libaes_siv }:

pkgs.stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  buildInputs = [ pkgs.openssl ed25519 ge-additions argon2 libaes_siv ];
}
