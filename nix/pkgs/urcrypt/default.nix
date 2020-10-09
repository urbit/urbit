{ pkgs, deps }:

pkgs.stdenv.mkDerivation rec {
  name    = "urcrypt";
  builder = ./builder.sh;
  src     = ../../../pkg/urcrypt;

  nativeBuildInputs =
    with pkgs;
    [ autoconf automake libtool m4 pkgconfig ];

  buildInputs =
    with pkgs;
    with deps;
    [ openssl gmp secp256k1 scrypt libaes_siv ];
}
