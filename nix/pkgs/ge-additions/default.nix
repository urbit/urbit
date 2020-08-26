{ pkgs, ed25519 }:

pkgs.stdenv.mkDerivation {
  name = "ge-additions";
  src  = ../../../pkg/ge-additions;

  buildInputs = [ ed25519 ];
}
