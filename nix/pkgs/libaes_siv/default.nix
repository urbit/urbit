{ lib, stdenv, openssl, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "libaes_siv";
  src = lib.cleanSource ../../../pkg/libaes_siv;

  buildInputs = [ openssl ];

  installFlags = [ "PREFIX=$(out)" ];

  inherit enableParallelBuilding;
}
