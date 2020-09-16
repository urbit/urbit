{ lib, stdenv, ed25519, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "ge-additions";
  src = lib.cleanSource ./.;

  buildInputs = [ ed25519 ];

  installFlags = [ "PREFIX=$(out)" ];

  inherit enableParallelBuilding;
}

