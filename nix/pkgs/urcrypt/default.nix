{ stdenv, autoconf, automake, libtool, m4, pkgconfig, openssl, gmp, secp256k1, scrypt, libaes_siv }:

stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  nativeBuildInputs =
    [ autoconf automake libtool m4 pkgconfig ];

  buildInputs =
    [ openssl gmp secp256k1 scrypt libaes_siv ];
}
