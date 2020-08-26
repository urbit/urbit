{ pkgs, ed25519 }:

pkgs.stdenv.mkDerivation {
  name    = "ge-additions";
  src     = ../../../pkg/ge-additions;
  builder = ./builder.sh;

  buildInputs = [ ed25519 ];
}
