{ stdenv, openssl, gmp, secp256k1, argon2, scrypt, libaes_siv }:

stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  buildInputs = [
    openssl gmp secp256k1 argon2 scrypt libaes_siv
  ];
}
