{ lib, stdenv, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "ent";
  src = lib.cleanSource ./.;

  postPatch = ''
    patchShebangs ./configure
  '';

  installFlags = [ "PREFIX=$(out)" ];

  inherit enableParallelBuilding;
}
