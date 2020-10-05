{ stdenv, pkgconfig, openssl, gmp, secp256k1, scrypt, libaes_siv }:

stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  buildInputs = [
    pkgconfig openssl gmp secp256k1 scrypt libaes_siv
  ];
}
