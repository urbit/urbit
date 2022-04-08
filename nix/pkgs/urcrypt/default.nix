{ stdenv, autoreconfHook, pkgconfig
, libaes_siv, libecc, openssl, secp256k1
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

  propagatedBuildInputs =
    [ openssl secp256k1 libaes_siv libecc ];
}
