{ stdenv, openssl, gmp, ed25519, secp256k1, argon2, libaes_siv }:

stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  buildInputs = [
    openssl gmp ed25519 secp256k1 argon2 libaes_siv
  ];
}
