{ pkgs, ed25519 }:

pkgs.stdenv.mkDerivation {
  name    = "ge-additions";
  builder = ./builder.sh;
  src     = ../../../pkg/ge-additions;

  nativeBuildInputs = [ ed25519 ];

  CFLAGS="-I${ed25519}/include";
  LDFLAGS="-L${ed25519}/lib";
}
