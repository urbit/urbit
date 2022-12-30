{ self, lib, stdenv, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  name = "ent";
  src = lib.cleanSource "${self}/pkg/ent";

  postPatch = ''
    patchShebangs ./configure
  '';

  installFlags = [ "PREFIX=$(out)" ];

  inherit enableParallelBuilding;
}
