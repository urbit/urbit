{ pkgs }:

pkgs.stdenv.mkDerivation {
  name    = "secp256k1";
  src     = pkgs.sources.secp256k1;
  builder = ./builder.sh;

  configureFlags = [
    "--disable-shared"
    "--enable-module-recovery"
  ];

  buildInputs = [
    pkgs.gmp
  ];

  nativeBuildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.libtool
    pkgs.m4
  ];

  CFLAGS = "-fPIC";
}
