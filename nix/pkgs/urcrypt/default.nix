{ stdenv, autoreconfHook, pkgconfig, openssl, gmp, secp256k1, scrypt, libaes_siv }:

stdenv.mkDerivation rec {
  name = "urcrypt";
  src  = ../../../pkg/urcrypt;

  nativeBuildInputs =
    [ autoreconfHook pkgconfig ];

  buildInputs =
    [ openssl gmp secp256k1 scrypt libaes_siv ];
}
