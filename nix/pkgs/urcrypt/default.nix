{ stdenv, autoreconfHook, pkgconfig, openssl, gmp, secp256k1, scrypt, libaes_siv
, enableStatic ? stdenv.hostPlatform.isStatic }:

stdenv.mkDerivation rec {
  name = "urcrypt";
  src  = ../../../pkg/urcrypt;

  # XX why are these required for darwin?
  dontDisableStatic = enableStatic;

  configureFlags = if enableStatic
    then [ "--disable-shared" "--enable-static" ]
    else [];

  nativeBuildInputs =
    [ autoreconfHook pkgconfig ];

  buildInputs =
    [ openssl gmp secp256k1 scrypt libaes_siv ];
}
